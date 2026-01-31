module fluff_lsp_server
    use fluff_diagnostics, only: diagnostic_t
    use fluff_formatter, only: formatter_engine_t
    use fluff_linter, only: linter_engine_t
    use fluff_lsp_protocol, only: lsp_publish_diagnostics_notification
    implicit none
    private

    public :: fluff_lsp_server_t
    public :: workspace_t
    public :: document_t

    ! Document representation for LSP
    type :: document_t
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: language_id
        integer :: version = 0
        character(len=:), allocatable :: content
        logical :: is_open = .false.
    contains
        procedure :: initialize => document_initialize
        procedure :: update_content => document_update_content
        procedure :: close => document_close
    end type document_t

    ! Workspace management
    type :: workspace_t
        character(len=:), allocatable :: root_path
        type(document_t), allocatable :: documents(:)
        integer :: document_count = 0
    contains
        procedure :: initialize => workspace_initialize
        procedure :: add_document => workspace_add_document
        procedure :: remove_document => workspace_remove_document
        procedure :: find_document => workspace_find_document
        procedure :: get_document_by_uri => workspace_get_document_by_uri
    end type workspace_t

    ! Main LSP server type
    type :: fluff_lsp_server_t
        type(workspace_t) :: workspace
        logical :: is_initialized = .false.
        logical :: is_shutdown = .false.
        logical :: supports_hover = .true.
        logical :: supports_formatting = .true.
        logical :: supports_diagnostics = .true.
        character(len=:), allocatable :: pending_notification
        logical :: has_pending_notification = .false.
    contains
        procedure :: initialize => lsp_server_initialize
        procedure :: shutdown => lsp_server_shutdown
        procedure :: handle_text_document_did_open
        procedure :: handle_text_document_did_change
        procedure :: handle_text_document_did_save
        procedure :: handle_text_document_did_close
        procedure :: handle_text_document_diagnostic
        procedure :: publish_diagnostics
        procedure :: format_document
        procedure :: get_server_capabilities
        procedure :: get_initialize_result
        procedure :: pop_notification
    end type fluff_lsp_server_t

contains

    ! Document methods
    subroutine document_initialize(this, uri, language_id, version, content)
        class(document_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, language_id, content
        integer, intent(in) :: version

        this%uri = uri
        this%language_id = language_id
        this%version = version
        this%content = content
        this%is_open = .true.

    end subroutine document_initialize

    subroutine document_update_content(this, new_content, new_version)
        class(document_t), intent(inout) :: this
        character(len=*), intent(in) :: new_content
        integer, intent(in) :: new_version

        this%content = new_content
        this%version = new_version

    end subroutine document_update_content

    subroutine document_close(this)
        class(document_t), intent(inout) :: this

        this%is_open = .false.

    end subroutine document_close

    ! Workspace methods
    subroutine workspace_initialize(this, root_path)
        class(workspace_t), intent(inout) :: this
        character(len=*), intent(in) :: root_path

        this%root_path = root_path
        this%document_count = 0
        if (allocated(this%documents)) deallocate (this%documents)
        allocate (this%documents(0))

    end subroutine workspace_initialize

    subroutine workspace_add_document(this, document)
        class(workspace_t), intent(inout) :: this
        type(document_t), intent(in) :: document

        type(document_t), allocatable :: temp_docs(:)

        ! Extend array
        allocate (temp_docs(this%document_count + 1))
        if (this%document_count > 0) temp_docs(1:this%document_count) = this%documents
        temp_docs(this%document_count + 1) = document

        call move_alloc(temp_docs, this%documents)
        this%document_count = this%document_count + 1

    end subroutine workspace_add_document

    subroutine workspace_remove_document(this, uri, success)
        class(workspace_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        logical, intent(out) :: success

        type(document_t), allocatable :: temp_docs(:)
        integer :: i, j

        success = .false.

        ! Find document index
        do i = 1, this%document_count
            if (this%documents(i)%uri == uri) then
                ! Remove document by copying others
                if (this%document_count > 1) then
                    allocate (temp_docs(this%document_count - 1))
                    j = 1
                    do j = 1, this%document_count
                        if (j < i) then
                            temp_docs(j) = this%documents(j)
                        else if (j > i) then
                            temp_docs(j - 1) = this%documents(j)
                        end if
                    end do
                    call move_alloc(temp_docs, this%documents)
                else
                    deallocate (this%documents)
                    allocate (this%documents(0))
                end if
                this%document_count = this%document_count - 1
                success = .true.
                exit
            end if
        end do

    end subroutine workspace_remove_document

    function workspace_find_document(this, uri) result(index)
        class(workspace_t), intent(in) :: this
        character(len=*), intent(in) :: uri
        integer :: index

        integer :: i

        index = -1
        do i = 1, this%document_count
            if (this%documents(i)%uri == uri) then
                index = i
                exit
            end if
        end do

    end function workspace_find_document

    function workspace_get_document_by_uri(this, uri) result(document)
        class(workspace_t), intent(in) :: this
        character(len=*), intent(in) :: uri
        type(document_t) :: document

        integer :: index

        index = this%find_document(uri)
        if (index > 0) then
            document = this%documents(index)
        else
            ! Return empty document if not found
            document%uri = ""
            document%language_id = ""
            document%version = -1
            document%content = ""
            document%is_open = .false.
        end if

    end function workspace_get_document_by_uri

    ! LSP Server methods
    subroutine lsp_server_initialize(this, root_path)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: root_path

        call this%workspace%initialize(root_path)
        this%is_initialized = .true.
        this%is_shutdown = .false.

    end subroutine lsp_server_initialize

    subroutine lsp_server_shutdown(this)
        class(fluff_lsp_server_t), intent(inout) :: this

        this%is_shutdown = .true.
        this%is_initialized = .false.

        if (allocated(this%workspace%documents)) then
            deallocate (this%workspace%documents)
        end if
        this%workspace%document_count = 0

    end subroutine lsp_server_shutdown

    function get_initialize_result(this) result(result_json)
        class(fluff_lsp_server_t), intent(in) :: this
        character(len=:), allocatable :: result_json

        character(len=:), allocatable :: capabilities

        capabilities = this%get_server_capabilities()
        result_json = '{"capabilities":'//capabilities// &
                      ',"serverInfo":{"name":"fluff","version":"0.1.0"}}'

    end function get_initialize_result

    subroutine handle_text_document_did_open(this, uri, language_id, version, &
                                             content, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, language_id, content
        integer, intent(in) :: version
        logical, intent(out) :: success

        type(document_t) :: document

        call document%initialize(uri, language_id, version, content)
        call this%workspace%add_document(document)
        success = .true.

        ! Trigger diagnostics
        call this%handle_text_document_diagnostic(uri, success)

    end subroutine handle_text_document_did_open

    subroutine handle_text_document_did_change(this, uri, new_version, &
                                               new_content, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, new_content
        integer, intent(in) :: new_version
        logical, intent(out) :: success

        integer :: doc_index

        success = .false.
        doc_index = this%workspace%find_document(uri)

        if (doc_index > 0) then
            call this%workspace%documents(doc_index)%update_content(new_content, &
                                                                    new_version)
            success = .true.

            ! Trigger diagnostics
            call this%handle_text_document_diagnostic(uri, success)
        end if

    end subroutine handle_text_document_did_change

    subroutine handle_text_document_did_save(this, uri, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        logical, intent(out) :: success

        integer :: doc_index

        success = .false.
        doc_index = this%workspace%find_document(uri)

        if (doc_index > 0) then
            success = .true.
            ! Trigger diagnostics on save
            call this%handle_text_document_diagnostic(uri, success)
        end if

    end subroutine handle_text_document_did_save

    subroutine handle_text_document_did_close(this, uri, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        logical, intent(out) :: success

        call this%workspace%remove_document(uri, success)

    end subroutine handle_text_document_did_close

    subroutine handle_text_document_diagnostic(this, uri, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        logical, intent(out) :: success

        type(document_t) :: document
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg

        success = .false.
        document = this%workspace%get_document_by_uri(uri)

        if (document%is_open) then
            ! Create temporary file to lint the content
            call linter%initialize()

            block
                use fluff_ast, only: fluff_ast_context_t
                type(fluff_ast_context_t) :: ast_ctx

                call ast_ctx%from_source(document%content, error_msg)
                if (error_msg == "") then
                    call linter%lint_ast(ast_ctx, diagnostics)
                    call this%publish_diagnostics(uri, diagnostics, success)
                else
                    print *, "ERROR: fortfront AST parsing failed in LSP server!"
                    print *, "Error: ", error_msg
                    print *, "Document URI: ", uri
                    print *, "File a GitHub issue at https://github.com/fortfront/"// &
                        "fortfront"
                    error stop "AST parsing required - no fallbacks!"
                end if
            end block
        end if

    end subroutine handle_text_document_diagnostic

    subroutine publish_diagnostics(this, uri, diagnostics, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        type(diagnostic_t), intent(in) :: diagnostics(:)
        logical, intent(out) :: success

        character(len=:), allocatable :: notification

        call lsp_publish_diagnostics_notification(uri, diagnostics, &
                                                  notification, success)
        if (.not. success) return

        this%pending_notification = notification
        this%has_pending_notification = .true.
    end subroutine publish_diagnostics

    subroutine pop_notification(this, notification, found)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: notification
        logical, intent(out) :: found

        if (.not. this%has_pending_notification) then
            notification = ""
            found = .false.
            return
        end if

        notification = this%pending_notification
        this%pending_notification = ""
        this%has_pending_notification = .false.
        found = .true.
    end subroutine pop_notification

    subroutine format_document(this, uri, formatted_content, success)
        class(fluff_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: formatted_content
        logical, intent(out) :: success

        type(document_t) :: document
        type(formatter_engine_t) :: formatter
        character(len=:), allocatable :: error_msg

        success = .false.
        document = this%workspace%get_document_by_uri(uri)

        if (document%is_open) then
            call formatter%initialize()
            call formatter%format_source(document%content, formatted_content, &
                                         error_msg)
            success = (error_msg == "")
        end if

    end subroutine format_document

    function get_server_capabilities(this) result(capabilities)
        class(fluff_lsp_server_t), intent(in) :: this
        character(len=:), allocatable :: capabilities
        character(len=:), allocatable :: hover_cap
        character(len=:), allocatable :: formatting_cap

        ! Return JSON representation of server capabilities
        hover_cap = merge('true ', 'false', this%supports_hover)
        formatting_cap = merge('true ', 'false', this%supports_formatting)

        capabilities = '{"textDocumentSync":2,'// &
                       '"hoverProvider":'//hover_cap//','// &
                       '"definitionProvider":false,'// &
                       '"documentFormattingProvider":'//formatting_cap//','// &
                       '"diagnosticProvider":{'// &
                       '"interFileDependencies":true}'// &
                       '}'

    end function get_server_capabilities

end module fluff_lsp_server
