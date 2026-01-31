module fluff_lsp_framing
    ! LSP Content-Length framing for stdio transport.
    ! LSP messages over stdio use header framing:
    !   Content-Length: <length>\r\n
    !   \r\n
    !   <JSON-RPC body of exactly <length> bytes>
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
    implicit none
    private

    integer, parameter :: MAX_HEADER_LINE_LEN = 256
    integer, parameter :: MAX_CONTENT_LEN = 10000000
    character(len=*), parameter :: CONTENT_LENGTH_PREFIX = "Content-Length: "
    character(len=*), parameter :: CRLF = char(13)//char(10)

    public :: lsp_read_framed_message
    public :: lsp_write_framed_message
    public :: lsp_parse_content_length

contains

    subroutine lsp_read_framed_message(message, success, error_msg)
        ! Read a single LSP message with Content-Length framing from stdin.
        ! Reads headers until blank line, then reads exact content_length bytes.
        character(len=:), allocatable, intent(out) :: message
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg

        character(len=MAX_HEADER_LINE_LEN) :: header_line
        integer :: content_length, iostat, i
        logical :: found_content_length
        character :: ch

        success = .false.
        error_msg = ""
        message = ""
        content_length = -1
        found_content_length = .false.

        ! Read headers until empty line (CRLF CRLF)
        do
            call read_header_line(header_line, iostat)
            if (iostat /= 0) then
                error_msg = "Failed to read header line"
                return
            end if

            ! Empty line signals end of headers
            if (len_trim(header_line) == 0) exit

            ! Parse Content-Length header
            if (index(header_line, CONTENT_LENGTH_PREFIX) == 1) then
                call lsp_parse_content_length(trim(header_line), content_length, &
                                              success)
                if (.not. success) then
                    error_msg = "Invalid Content-Length header"
                    return
                end if
                found_content_length = .true.
            end if
        end do

        if (.not. found_content_length) then
            error_msg = "Missing Content-Length header"
            success = .false.
            return
        end if

        if (content_length <= 0) then
            error_msg = "Invalid Content-Length value"
            success = .false.
            return
        end if

        if (content_length > MAX_CONTENT_LEN) then
            error_msg = "Content-Length exceeds maximum"
            success = .false.
            return
        end if

        ! Read exactly content_length bytes
        allocate (character(len=content_length) :: message)
        do i = 1, content_length
            read (input_unit, '(A1)', advance='no', iostat=iostat) ch
            if (iostat /= 0) then
                error_msg = "Failed to read message body"
                success = .false.
                return
            end if
            message(i:i) = ch
        end do

        success = .true.
    end subroutine lsp_read_framed_message

    subroutine read_header_line(line, iostat)
        ! Read a single header line terminated by CRLF.
        character(len=*), intent(out) :: line
        integer, intent(out) :: iostat

        character :: ch, prev_ch
        integer :: pos

        line = ""
        pos = 0
        prev_ch = char(0)

        do
            read (input_unit, '(A1)', advance='no', iostat=iostat) ch
            if (iostat /= 0) return

            ! Check for CRLF sequence
            if (prev_ch == char(13) .and. ch == char(10)) then
                ! Remove trailing CR from line
                if (pos > 0) then
                    line(pos:pos) = ' '
                    pos = pos - 1
                end if
                iostat = 0
                return
            end if

            ! Store character if not CR (will add CR to line, remove if followed by LF)
            pos = pos + 1
            if (pos <= len(line)) then
                line(pos:pos) = ch
            end if

            prev_ch = ch
        end do
    end subroutine read_header_line

    subroutine lsp_parse_content_length(header, content_length, success)
        ! Parse Content-Length value from header line.
        character(len=*), intent(in) :: header
        integer, intent(out) :: content_length
        logical, intent(out) :: success

        integer :: prefix_len, iostat
        character(len=32) :: value_str

        success = .false.
        content_length = -1

        prefix_len = len(CONTENT_LENGTH_PREFIX)
        if (len_trim(header) <= prefix_len) return

        value_str = adjustl(header(prefix_len + 1:))
        read (value_str, *, iostat=iostat) content_length
        success = (iostat == 0 .and. content_length >= 0)
    end subroutine lsp_parse_content_length

    subroutine lsp_write_framed_message(message)
        ! Write an LSP message with Content-Length framing to stdout.
        character(len=*), intent(in) :: message

        character(len=32) :: length_str
        integer :: msg_len

        msg_len = len(message)
        write (length_str, '(I0)') msg_len

        ! Write headers
        write (output_unit, '(A)', advance='no') CONTENT_LENGTH_PREFIX
        write (output_unit, '(A)', advance='no') trim(length_str)
        write (output_unit, '(A)', advance='no') CRLF
        write (output_unit, '(A)', advance='no') CRLF

        ! Write body
        write (output_unit, '(A)', advance='no') message

        ! Flush output
        flush (output_unit)
    end subroutine lsp_write_framed_message

end module fluff_lsp_framing
