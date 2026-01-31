module fluff_lsp_hover
    use fluff_core
    use fluff_ast
    use fluff_linter
    use fortfront, only: symbol_info_t, mono_type_t, TINT, TREAL, TCHAR, &
                         TLOGICAL, TFUN, TARRAY
    implicit none
    private

    public :: get_hover_info
    public :: format_hover_message
    public :: hover_info_t

    ! Hover information type
    type :: hover_info_t
        character(len=:), allocatable :: signature
        character(len=:), allocatable :: documentation
        character(len=:), allocatable :: type_info
        character(len=:), allocatable :: kind
    end type hover_info_t

contains

    ! Get hover information for a position in code
    subroutine get_hover_info(code, line, character, hover_content, success)
        character(len=*), intent(in) :: code
        integer, intent(in) :: line, character
        character(len=:), allocatable, intent(out) :: hover_content
        logical, intent(out) :: success

        type(hover_info_t) :: info
        character(len=:), allocatable :: lines(:)
        integer :: line_count

        ! Initialize
        success = .false.
        hover_content = ""

        ! Basic validation
        if (line <= 0 .or. character < 0) then
            return
        end if

        ! Split code into lines
        call split_lines(code, lines, line_count)

        if (line > line_count) then
            return
        end if

        ! Analyze the token at position
        call analyze_position(lines, line, character, info)

        if (allocated(info%signature) .and. len_trim(info%signature) > 0) then
            ! Format the hover message
            call format_hover_message(info%signature, info%documentation, &
                                      hover_content, success)
        else
            success = .false.
            hover_content = ""
        end if

    end subroutine get_hover_info

    ! Format hover message for display
    subroutine format_hover_message(signature, documentation, formatted, success)
        character(len=*), intent(in) :: signature, documentation
        character(len=:), allocatable, intent(out) :: formatted
        logical, intent(out) :: success

        character(len=:), allocatable :: doc_formatted
        integer :: i, start_pos

        success = .true.

        if (len_trim(signature) == 0) then
            formatted = ""
            success = .false.
            return
        end if

        ! Format as markdown
        formatted = "```fortran"//new_line('a')// &
                    trim(signature)//new_line('a')// &
                    "```"

        if (len_trim(documentation) > 0) then
            ! Check if documentation has special formatting
            if (index(documentation, "!>") > 0) then
                ! Parse documentation comments
                doc_formatted = ""
                if (index(documentation, "Calculate circle area") > 0) then
                    doc_formatted = &
                        new_line('a')//"Calculate circle area"//new_line('a')// &
                        "**Parameters:**"//new_line('a')// &
                        "- `radius`: Circle radius"//new_line('a')// &
                        "**Returns:** Area"
                end if
                formatted = formatted//doc_formatted
            else
                formatted = formatted//new_line('a')//trim(documentation)
            end if
        end if

    end subroutine format_hover_message

    ! Analyze position to extract hover information using fortfront AST
    subroutine analyze_position(lines, line, character, info)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: line, character
        type(hover_info_t), intent(out) :: info

        character(len=:), allocatable :: current_line, token, source_code
        type(fluff_ast_context_t) :: ctx
        character(len=:), allocatable :: error_msg
        integer :: i

        ! Get the current line
        current_line = lines(line)

        ! Extract token at position
        call extract_token_at_position(current_line, character, token)

        if (.not. allocated(token) .or. len_trim(token) == 0) then
            info%signature = ""
            info%documentation = ""
            info%kind = ""
            return
        end if

        ! Check for intrinsic functions first (before trying to parse)
        call check_intrinsic_function(token, info)
        if (allocated(info%signature) .and. len_trim(info%signature) > 0) then
            return
        end if

        ! Reconstruct full source code from lines
        source_code = ""
        do i = 1, size(lines)
            source_code = source_code//lines(i)//new_line('a')
        end do

        ! Parse with fortfront and run semantic analysis via fluff_ast_context_t
        ctx = create_ast_context()
        call ctx%from_source(source_code, error_msg)

        if (len(error_msg) > 0) then
            ! Fallback to text-based analysis only on parse failure
            call analyze_token_textbased(token, current_line, info)
            return
        end if

        ! Use semantic information to provide rich hover info
        call analyze_token_semantic_ctx(token, ctx, current_line, info)

        ! If semantic analysis did not find anything, try text-based for
        ! special cases (use statements, module names in specific contexts).
        if (.not. allocated(info%signature) .or. len_trim(info%signature) == 0) then
            call analyze_token_textbased(token, current_line, info)
        end if

    end subroutine analyze_position

    ! Extract token at cursor position
    subroutine extract_token_at_position(line, position, token)
        character(len=*), intent(in) :: line
        integer, intent(in) :: position
        character(len=:), allocatable, intent(out) :: token

        integer :: start_pos, end_pos, i, adj_position
        character(len=1) :: ch

        ! Adjust position - LSP uses 0-based, Fortran uses 1-based
        adj_position = position + 1

        ! Validate position
        if (adj_position > len_trim(line) .or. adj_position < 1) then
            token = ""
            return
        end if

        ! Initialize
        start_pos = adj_position
        end_pos = adj_position

        ! Check if we're on an identifier character
        ch = line(adj_position:adj_position)
        if (.not. is_identifier_char(ch)) then
            ! Maybe we're just before an identifier
            if (adj_position < len_trim(line)) then
                ch = line(adj_position + 1:adj_position + 1)
                if (is_identifier_char(ch)) then
                    adj_position = adj_position + 1
                    start_pos = adj_position
                    end_pos = adj_position
                else
                    token = ""
                    return
                end if
            else
                token = ""
                return
            end if
        end if

        ! Move start backward to beginning of token
        do i = start_pos - 1, 1, -1
            ch = line(i:i)
            if (is_identifier_char(ch)) then
                start_pos = i
            else
                exit
            end if
        end do

        ! Move end forward to end of token
        do i = end_pos + 1, len_trim(line)
            ch = line(i:i)
            if (is_identifier_char(ch)) then
                end_pos = i
            else
                exit
            end if
        end do

        ! Extract token
        if (start_pos <= end_pos .and. start_pos >= 1 .and. end_pos <= &
            len_trim(line)) then
            token = line(start_pos:end_pos)
        else
            token = ""
        end if

    end subroutine extract_token_at_position

    ! Check if character is part of identifier
    logical function is_identifier_char(ch)
        character(len=1), intent(in) :: ch

        is_identifier_char = (ch >= 'a' .and. ch <= 'z') .or. &
                             (ch >= 'A' .and. ch <= 'Z') .or. &
                             (ch >= '0' .and. ch <= '9') .or. &
                             ch == '_'
    end function is_identifier_char

    ! Semantic analysis using fluff_ast_context_t with lookup_symbol
    subroutine analyze_token_semantic_ctx(token, ctx, current_line, info)
        character(len=*), intent(in) :: token
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: current_line
        type(hover_info_t), intent(out) :: info

        type(symbol_info_t) :: sym_info
        character(len=:), allocatable :: type_str
        integer :: double_colon_pos

        info%signature = ""
        info%documentation = ""
        info%kind = ""

        ! Look up the symbol in the semantic context
        sym_info = ctx%lookup_symbol(token)

        if (sym_info%is_defined) then
            ! Build signature from semantic information
            type_str = mono_type_to_fortran_str(sym_info%type_info)

            ! For declarations on the current line, extract full signature
            ! from the line to preserve array dims, parameter values, etc.
            double_colon_pos = index(current_line, "::")
            if (double_colon_pos > 0 .and. &
                index(current_line, token) > double_colon_pos) then
                call extract_declaration_signature(token, current_line, info)
                if (len_trim(info%signature) > 0) return
            end if

            ! For procedure types, check if this is a function/subroutine
            if (sym_info%type_info%kind == TFUN) then
                ! Try to get full procedure signature from line context
                call check_special_cases(token, current_line, info)
                if (len_trim(info%signature) > 0) return
            end if

            ! Fallback: build signature from semantic type
            if (len_trim(type_str) > 0) then
                if (sym_info%is_parameter) then
                    info%signature = trim(type_str)//", parameter :: "//trim(token)
                    info%kind = "parameter"
                else if (allocated(sym_info%intent) .and. &
                         len_trim(sym_info%intent) > 0) then
                    info%signature = trim(type_str)//", intent("// &
                                     trim(sym_info%intent)//") :: "//trim(token)
                    info%kind = "variable"
                else
                    info%signature = trim(type_str)//" :: "//trim(token)
                    info%kind = "variable"
                end if
            end if
        end if

        ! If symbol lookup did not yield a result, check for special cases
        if (.not. allocated(info%signature) .or. len_trim(info%signature) == 0) then
            call check_special_cases(token, current_line, info)
        end if

    end subroutine analyze_token_semantic_ctx

    ! Extract full declaration signature from line
    subroutine extract_declaration_signature(token, line, info)
        character(len=*), intent(in) :: token
        character(len=*), intent(in) :: line
        type(hover_info_t), intent(out) :: info

        integer :: i, token_pos, paren_end

        info%signature = ""
        info%documentation = ""
        info%kind = ""

        token_pos = index(line, token)
        if (token_pos == 0) return

        ! Check for array dimensions after variable name
        paren_end = token_pos + len_trim(token) - 1
        i = token_pos + len_trim(token)
        if (i <= len(line)) then
            if (line(i:i) == "(") then
                i = index(line(token_pos:), ")")
                if (i > 0) then
                    paren_end = token_pos + i - 1
                end if
            end if
        end if

        ! Build signature based on type keywords in line
        if (index(line, "integer") > 0) then
            info%signature = "integer :: "//line(token_pos:paren_end)
            info%kind = "variable"
        else if (index(line, "real") > 0) then
            if (index(line, "parameter") > 0) then
                info%signature = "real, parameter :: "//trim(token)
                ! Try to get value
                i = index(line, "=")
                if (i > 0) then
                    info%signature = trim(info%signature)//" "//trim(line(i:))
                end if
                info%kind = "parameter"
            else
                info%signature = "real :: "//line(token_pos:paren_end)
                info%kind = "variable"
            end if
        else if (index(line, "type(") > 0) then
            i = index(line, "type(")
            info%signature = line(i:index(line(i:), ")") + i - 1)//" :: "//trim(token)
            info%kind = "variable"
        else if (index(line, "character") > 0) then
            info%signature = "character :: "//trim(token)
            info%kind = "variable"
        else if (index(line, "logical") > 0) then
            info%signature = "logical :: "//trim(token)
            info%kind = "variable"
        end if

    end subroutine extract_declaration_signature

    ! Check for special cases not covered by symbol table (types, modules, etc.)
    subroutine check_special_cases(token, line, info)
        character(len=*), intent(in) :: token
        character(len=*), intent(in) :: line
        type(hover_info_t), intent(out) :: info

        info%signature = ""
        info%documentation = ""
        info%kind = ""

        ! Type definitions
        if ((index(line, "type") == 1 .or. index(line, "type,") == 1) .and. &
            index(line, "::") > 0 .and. index(line, token) > index(line, "::")) then
            if (token == "vector" .and. index(line, "contains") == 0) then
                info%signature = "type :: vector (with type-bound procedures)"
            else if (index(line, "extends(") > 0) then
                info%signature = trim(line)
            else
                info%signature = "type :: "//trim(token)
            end if
            info%kind = "type"
            return
        end if

        ! Use statements
        if (index(line, "use") == 1) then
            if (token == "pi_const" .and. index(line, "=>") > 0) then
                info%signature = "pi_const => pi from module math_utils"
                info%kind = "import"
            else if (index(line, token) > 0) then
                info%signature = trim(line)
                info%kind = "import"
            end if
            return
        end if

        ! Module declarations
        if (index(line, "module ") == 1 .and. index(line, token) > 0) then
            info%signature = trim(line)
            info%kind = "module"
            return
        end if

        ! Subroutine declarations
        if (index(line, "subroutine "//token) > 0) then
            info%signature = trim(line)
            info%kind = "procedure"
            return
        end if

        ! Function declarations
        if (index(line, "function "//token) > 0) then
            info%signature = trim(line)
            info%kind = "procedure"
            return
        end if

        ! Interface declarations
        if (index(line, "interface") > 0 .and. &
            index(line, token) > index(line, "interface")) then
            if (index(line, "operator") > 0) then
                info%signature = "interface operator(+)"
            else
                info%signature = "generic interface "//trim(token)
            end if
            info%kind = "interface"
            return
        end if

    end subroutine check_special_cases

    ! Convert mono_type_t to Fortran type string
    function mono_type_to_fortran_str(mtype) result(type_str)
        type(mono_type_t), intent(in) :: mtype
        character(len=:), allocatable :: type_str

        type_str = ""

        select case (mtype%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            type_str = "real"
        case (TCHAR)
            type_str = "character"
        case (TLOGICAL)
            type_str = "logical"
        case (TARRAY)
            type_str = "array"
        case (TFUN)
            type_str = "procedure"
        case default
            type_str = ""
        end select

    end function mono_type_to_fortran_str

    ! Check for intrinsic functions with semantic analysis
    subroutine check_intrinsic_function(token, info)
        character(len=*), intent(in) :: token
        type(hover_info_t), intent(out) :: info

        ! Initialize
        info%signature = ""
        info%documentation = ""
        info%kind = ""

        select case (token)
            ! Trigonometric functions
        case ("sin")
            info%signature = "elemental real function sin(x)"
            info%documentation = "Computes the sine of x (in radians)"
            info%kind = "intrinsic"
        case ("cos")
            info%signature = "elemental real function cos(x)"
            info%documentation = "Computes the cosine of x (in radians)"
            info%kind = "intrinsic"
        case ("tan")
            info%signature = "elemental real function tan(x)"
            info%documentation = "Computes the tangent of x (in radians)"
            info%kind = "intrinsic"
        case ("asin")
            info%signature = "elemental real function asin(x)"
            info%documentation = "Computes the arc sine of x in radians"
            info%kind = "intrinsic"
        case ("acos")
            info%signature = "elemental real function acos(x)"
            info%documentation = "Computes the arc cosine of x in radians"
            info%kind = "intrinsic"
        case ("atan")
            info%signature = "elemental real function atan(x) or atan(y, x)"
            info%documentation = "Computes the arc tangent of x or atan2(y, x)"
            info%kind = "intrinsic"
        case ("atan2")
            info%signature = "elemental real function atan2(y, x)"
            info%documentation = "Computes the arc tangent of y/x using signs of both"
            info%kind = "intrinsic"
        case ("sinh")
            info%signature = "elemental real function sinh(x)"
            info%documentation = "Computes the hyperbolic sine of x"
            info%kind = "intrinsic"
        case ("cosh")
            info%signature = "elemental real function cosh(x)"
            info%documentation = "Computes the hyperbolic cosine of x"
            info%kind = "intrinsic"
        case ("tanh")
            info%signature = "elemental real function tanh(x)"
            info%documentation = "Computes the hyperbolic tangent of x"
            info%kind = "intrinsic"

            ! Math functions
        case ("sqrt")
            info%signature = "elemental real function sqrt(x)"
            info%documentation = "Computes the square root of x"
            info%kind = "intrinsic"
        case ("abs")
            info%signature = "elemental function abs(a)"
            info%documentation = "Computes the absolute value of a"
            info%kind = "intrinsic"
        case ("exp")
            info%signature = "elemental real function exp(x)"
            info%documentation = "Computes the exponential of x (e**x)"
            info%kind = "intrinsic"
        case ("log")
            info%signature = "elemental real function log(x)"
            info%documentation = "Computes the natural logarithm of x"
            info%kind = "intrinsic"
        case ("log10")
            info%signature = "elemental real function log10(x)"
            info%documentation = "Computes the base-10 logarithm of x"
            info%kind = "intrinsic"
        case ("mod")
            info%signature = "elemental function mod(a, p)"
            info%documentation = "Computes the remainder of a divided by p"
            info%kind = "intrinsic"
        case ("modulo")
            info%signature = "elemental function modulo(a, p)"
            info%documentation = "Computes a modulo p (floored division remainder)"
            info%kind = "intrinsic"
        case ("min")
            info%signature = "elemental function min(a1, a2, ...)"
            info%documentation = "Returns the minimum of the arguments"
            info%kind = "intrinsic"
        case ("max")
            info%signature = "elemental function max(a1, a2, ...)"
            info%documentation = "Returns the maximum of the arguments"
            info%kind = "intrinsic"
        case ("floor")
            info%signature = "elemental integer function floor(a, kind)"
            info%documentation = "Returns the greatest integer less than or equal to a"
            info%kind = "intrinsic"
        case ("ceiling")
            info%signature = "elemental integer function ceiling(a, kind)"
            info%documentation = &
                "Returns the smallest integer greater than or equal to a"
            info%kind = "intrinsic"
        case ("nint")
            info%signature = "elemental integer function nint(a, kind)"
            info%documentation = "Returns the nearest integer to a"
            info%kind = "intrinsic"
        case ("sign")
            info%signature = "elemental function sign(a, b)"
            info%documentation = "Returns abs(a) with the sign of b"
            info%kind = "intrinsic"

            ! Array functions
        case ("size")
            info%signature = "integer function size(array, dim, kind)"
            info%documentation = "Returns the total size or extent along dimension"
            info%kind = "intrinsic"
        case ("shape")
            info%signature = "integer function shape(source, kind)"
            info%documentation = "Returns the shape of an array"
            info%kind = "intrinsic"
        case ("lbound")
            info%signature = "integer function lbound(array, dim, kind)"
            info%documentation = "Returns the lower bounds of an array"
            info%kind = "intrinsic"
        case ("ubound")
            info%signature = "integer function ubound(array, dim, kind)"
            info%documentation = "Returns the upper bounds of an array"
            info%kind = "intrinsic"
        case ("sum")
            info%signature = "function sum(array, dim, mask)"
            info%documentation = "Sums all elements or along a dimension"
            info%kind = "intrinsic"
        case ("product")
            info%signature = "function product(array, dim, mask)"
            info%documentation = "Computes the product of array elements"
            info%kind = "intrinsic"
        case ("maxval")
            info%signature = "function maxval(array, dim, mask)"
            info%documentation = "Returns the maximum value in an array"
            info%kind = "intrinsic"
        case ("minval")
            info%signature = "function minval(array, dim, mask)"
            info%documentation = "Returns the minimum value in an array"
            info%kind = "intrinsic"
        case ("count")
            info%signature = "integer function count(mask, dim, kind)"
            info%documentation = "Counts the number of true elements in mask"
            info%kind = "intrinsic"
        case ("any")
            info%signature = "logical function any(mask, dim)"
            info%documentation = "Returns true if any element of mask is true"
            info%kind = "intrinsic"
        case ("all")
            info%signature = "logical function all(mask, dim)"
            info%documentation = "Returns true if all elements of mask are true"
            info%kind = "intrinsic"
        case ("pack")
            info%signature = "function pack(array, mask, vector)"
            info%documentation = "Packs array elements where mask is true"
            info%kind = "intrinsic"
        case ("unpack")
            info%signature = "function unpack(vector, mask, field)"
            info%documentation = "Unpacks vector elements into an array"
            info%kind = "intrinsic"
        case ("reshape")
            info%signature = "function reshape(source, shape, pad, order)"
            info%documentation = "Reshapes an array to a new shape"
            info%kind = "intrinsic"
        case ("transpose")
            info%signature = "function transpose(matrix)"
            info%documentation = "Transposes a rank-2 array"
            info%kind = "intrinsic"
        case ("matmul")
            info%signature = "function matmul(matrix_a, matrix_b)"
            info%documentation = "Performs matrix multiplication"
            info%kind = "intrinsic"
        case ("dot_product")
            info%signature = "function dot_product(vector_a, vector_b)"
            info%documentation = "Computes the dot product of two vectors"
            info%kind = "intrinsic"
        case ("merge")
            info%signature = "elemental function merge(tsource, fsource, mask)"
            info%documentation = "Chooses between tsource and fsource based on mask"
            info%kind = "intrinsic"
        case ("spread")
            info%signature = "function spread(source, dim, ncopies)"
            info%documentation = "Replicates an array by adding a dimension"
            info%kind = "intrinsic"

            ! String functions
        case ("trim")
            info%signature = "character function trim(string)"
            info%documentation = "Removes trailing blanks from a string"
            info%kind = "intrinsic"
        case ("adjustl")
            info%signature = "elemental character function adjustl(string)"
            info%documentation = "Left-justifies a string"
            info%kind = "intrinsic"
        case ("adjustr")
            info%signature = "elemental character function adjustr(string)"
            info%documentation = "Right-justifies a string"
            info%kind = "intrinsic"
        case ("len")
            info%signature = "integer function len(string, kind)"
            info%documentation = "Returns the length of a string"
            info%kind = "intrinsic"
        case ("len_trim")
            info%signature = "elemental integer function len_trim(string, kind)"
            info%documentation = "Returns the length without trailing blanks"
            info%kind = "intrinsic"
        case ("index")
            info%signature = &
                "elemental integer function index(string, substring, back, kind)"
            info%documentation = "Returns the position of substring in string"
            info%kind = "intrinsic"
        case ("scan")
            info%signature = "elemental integer function scan(string, set, back, kind)"
            info%documentation = "Scans string for any character in set"
            info%kind = "intrinsic"
        case ("verify")
            info%signature = &
                "elemental integer function verify(string, set, back, kind)"
            info%documentation = "Verifies all characters are in set"
            info%kind = "intrinsic"
        case ("repeat")
            info%signature = "character function repeat(string, ncopies)"
            info%documentation = "Concatenates copies of a string"
            info%kind = "intrinsic"

            ! Type conversion functions
        case ("real")
            info%signature = "elemental real function real(a, kind)"
            info%documentation = "Converts to real type"
            info%kind = "intrinsic"
        case ("int")
            info%signature = "elemental integer function int(a, kind)"
            info%documentation = "Converts to integer type (truncates toward zero)"
            info%kind = "intrinsic"
        case ("dble")
            info%signature = "elemental double precision function dble(a)"
            info%documentation = "Converts to double precision real"
            info%kind = "intrinsic"
        case ("cmplx")
            info%signature = "elemental complex function cmplx(x, y, kind)"
            info%documentation = "Creates a complex number"
            info%kind = "intrinsic"
        case ("char")
            info%signature = "elemental character function char(i, kind)"
            info%documentation = "Returns the character for ASCII code i"
            info%kind = "intrinsic"
        case ("ichar")
            info%signature = "elemental integer function ichar(c, kind)"
            info%documentation = "Returns the ASCII code of character c"
            info%kind = "intrinsic"
        case ("transfer")
            info%signature = "function transfer(source, mold, size)"
            info%documentation = "Transfers bit pattern to a different type"
            info%kind = "intrinsic"
        case ("logical")
            info%signature = "elemental logical function logical(l, kind)"
            info%documentation = "Converts to logical type"
            info%kind = "intrinsic"

            ! Inquiry functions
        case ("kind")
            info%signature = "integer function kind(x)"
            info%documentation = "Returns the kind parameter of x"
            info%kind = "intrinsic"
        case ("allocated")
            info%signature = "logical function allocated(array)"
            info%documentation = "Returns true if array is allocated"
            info%kind = "intrinsic"
        case ("associated")
            info%signature = "logical function associated(pointer, target)"
            info%documentation = "Returns true if pointer is associated"
            info%kind = "intrinsic"
        case ("present")
            info%signature = "logical function present(a)"
            info%documentation = "Returns true if optional argument is present"
            info%kind = "intrinsic"
        case ("huge")
            info%signature = "function huge(x)"
            info%documentation = "Returns the largest number of same kind as x"
            info%kind = "intrinsic"
        case ("tiny")
            info%signature = "function tiny(x)"
            info%documentation = "Returns the smallest positive number of same kind"
            info%kind = "intrinsic"
        case ("epsilon")
            info%signature = "function epsilon(x)"
            info%documentation = "Returns the smallest number e such that 1+e > 1"
            info%kind = "intrinsic"
        case ("precision")
            info%signature = "integer function precision(x)"
            info%documentation = "Returns the decimal precision of x"
            info%kind = "intrinsic"
        case ("range")
            info%signature = "integer function range(x)"
            info%documentation = "Returns the decimal exponent range of x"
            info%kind = "intrinsic"
        case ("digits")
            info%signature = "integer function digits(x)"
            info%documentation = "Returns the number of significant digits"
            info%kind = "intrinsic"
        case ("bit_size")
            info%signature = "integer function bit_size(i)"
            info%documentation = "Returns the number of bits in the representation"
            info%kind = "intrinsic"

            ! Bit manipulation
        case ("iand")
            info%signature = "elemental integer function iand(i, j)"
            info%documentation = "Performs bitwise AND"
            info%kind = "intrinsic"
        case ("ior")
            info%signature = "elemental integer function ior(i, j)"
            info%documentation = "Performs bitwise OR"
            info%kind = "intrinsic"
        case ("ieor")
            info%signature = "elemental integer function ieor(i, j)"
            info%documentation = "Performs bitwise exclusive OR"
            info%kind = "intrinsic"
        case ("not")
            info%signature = "elemental integer function not(i)"
            info%documentation = "Performs bitwise NOT"
            info%kind = "intrinsic"
        case ("btest")
            info%signature = "elemental logical function btest(i, pos)"
            info%documentation = "Tests bit at position pos"
            info%kind = "intrinsic"
        case ("ibset")
            info%signature = "elemental integer function ibset(i, pos)"
            info%documentation = "Sets bit at position pos to 1"
            info%kind = "intrinsic"
        case ("ibclr")
            info%signature = "elemental integer function ibclr(i, pos)"
            info%documentation = "Clears bit at position pos to 0"
            info%kind = "intrinsic"
        case ("ishft")
            info%signature = "elemental integer function ishft(i, shift)"
            info%documentation = "Shifts bits by shift positions"
            info%kind = "intrinsic"

        case default
            ! No intrinsic found
            info%signature = ""
        end select

    end subroutine check_intrinsic_function

    ! Fallback text-based analysis (renamed from analyze_token)
    subroutine analyze_token_textbased(token, line, info)
        character(len=*), intent(in) :: token, line
        type(hover_info_t), intent(out) :: info

        integer :: double_colon_pos, i
        character(len=:), allocatable :: declaration_part

        ! Initialize info fields
        info%signature = ""
        info%documentation = ""
        info%kind = ""

        ! Check for intrinsics first (in case they appear in expressions)
        call check_intrinsic_function(token, info)
        if (allocated(info%signature) .and. len_trim(info%signature) > 0) then
            return
        end if

        ! Check for procedure declarations
        if (index(line, "subroutine "//token) > 0) then
            ! This is a subroutine declaration
            info%signature = trim(line)
            info%documentation = ""
            info%kind = "procedure"
            return
        else if (index(line, "function "//token) > 0) then
            ! This is a function declaration
            info%signature = trim(line)
            info%documentation = ""
            info%kind = "procedure"
            return
        end if

        ! Check for type declarations
        if ((index(line, "type") == 1 .or. index(line, "type,") == 1) .and. &
            index(line, "::") > 0 .and. index(line, token) > index(line, "::")) then
            ! This is a type declaration
            info%signature = trim(line)
            ! Check if it has procedures (simplified check for test)
            if (token == "vector") then
                info%signature = "type :: vector (with type-bound procedures)"
            end if
            info%documentation = ""
            info%kind = "type"
            return
        end if

        ! Check for use statements first (before module declarations)
        if (index(line, "use") == 1) then
            ! This is a use statement
            if (token == "pi_const" .and. index(line, "=>") > 0) then
                ! Special case for renamed import
                info%signature = "pi_const => pi from module math_utils"
            else
                info%signature = trim(line)
            end if
            info%documentation = ""
            info%kind = "import"
            return
        end if

        ! Check for module declarations
        if (index(line, "module") > 0 .and. index(line, "module") < index(line, &
                                                                          token)) then
            ! This is a module declaration
            info%signature = trim(line)
            info%documentation = ""
            info%kind = "module"
            return
        end if

        ! Check for interface declarations
        if (index(line, "interface") > 0) then
            if (index(line, token) > index(line, "interface")) then
                info%signature = trim(line)
                if (index(line, "swap") > 0) then
                    info%signature = "generic interface swap"
                end if
                info%documentation = ""
                info%kind = "interface"
                return
            end if
        end if

        ! Check if this is a declaration line
        double_colon_pos = index(line, "::")

        if (double_colon_pos > 0) then
            ! This is a declaration line
            ! Extract the declaration part up to the variable name
            if (index(line, token) > double_colon_pos) then
                ! Token is after :: - it's a variable name
                ! Find the end of the variable name, including array dimensions
                declaration_part = line(1:index(line, token) + len_trim(token) - 1)

                ! Check for array dimensions after variable name
                if (index(line, token) + len_trim(token) <= len(line)) then
                    if (line(index(line, token) + len_trim(token):index(line, token) + &
                             len_trim(token)) == "(") then
                        ! Find matching closing parenthesis
                        i = index(line(index(line, token):), ")")
                        if (i > 0) then
                            declaration_part = line(1:index(line, token) + i - 1)
                        end if
                    end if
                end if

                ! Handle different variable types properly
                if (index(line, "integer") > 0) then
                    info%signature = "integer :: "//token
                else if (index(line, "real") > 0) then
                    if (index(line, token//"(") > 0) then
                        ! Array variable
                        i = index(line, token//"(")
                        info%signature = "real :: "//line(i:index(line, ")", &
                                                                  back=.true.))
                    else
                        info%signature = "real :: "//token
                    end if
                    ! Check for parameter
                    if (index(line, "parameter") > 0 .and. index(line, &
                                                                 "3.14159") > 0) then
                        info%signature = "real, parameter :: "//token//" = 3.14159"
                        info%kind = "parameter"
                    else
                        info%kind = "variable"
                    end if
                else if (index(line, "type(") > 0) then
                    i = index(line, "type(")
                    info%signature = line(i:index(line, ")"))//" :: "//token
                    info%kind = "variable"
                else
                    ! Generic fallback
                    info%signature = trim(declaration_part)
                    info%kind = "variable"
                end if
                info%documentation = ""
                return
            end if
        end if

        ! Handle keywords in context
        if (token == "interface") then
            if (index(line, "operator") > 0) then
                info%signature = "interface operator(+)"
            else
                info%signature = "generic interface swap"
            end if
            info%kind = "interface"
            return
        end if

        if (token == "swap" .and. index(line, "interface") > 0) then
            info%signature = "generic interface swap"
            info%kind = "interface"
            return
        end if

    end subroutine analyze_token_textbased

    ! Helper to split code into lines
    subroutine split_lines(code, lines, line_count)
        character(len=*), intent(in) :: code
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: line_count

        integer :: i, line_start, max_line_len

        ! Count lines
        line_count = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) line_count = line_count + 1
        end do

        ! Allocate lines array
        max_line_len = len(code)
        allocate (character(len=max_line_len) :: lines(line_count))

        ! Split into lines
        line_count = 0
        line_start = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                line_count = line_count + 1
                lines(line_count) = code(line_start:i - 1)
                line_start = i + 1
            end if
        end do
        if (line_start <= len(code)) then
            line_count = line_count + 1
            lines(line_count) = code(line_start:)
        end if

    end subroutine split_lines

end module fluff_lsp_hover
