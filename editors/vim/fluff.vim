" Vim configuration for fluff linter integration
" Add this to your .vimrc or init.vim

" Basic fluff integration
if executable('fluff')
    " Set makeprg to use fluff
    set makeprg=fluff\ check\ %

    " Define error format for fluff output
    set errorformat=%f:%l:%c:\ %t%n:\ %m

    " Auto-run fluff on save
    autocmd BufWritePost *.f90,*.F90,*.f95,*.F95 :make

    " Enable syntax checking with fluff
    function! FluffCheck()
        exec ':make'
        :copen
    endfunction

    " Map <F5> to run fluff check
    nnoremap <F5> :call FluffCheck()<CR>
endif