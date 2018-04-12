" Change mapleader
let mapleader=","

" Mapping NERDTree 
map <C-n> :NERDTreeToggle<CR>

:nmap <Leader>i :TsuImport<CR>
autocmd FileType typescript setlocal completeopt+=menu,preview
autocmd FileType typescript nmap <buffer> <Leader>t : <C-u>echo tsuquyomi#hint()<CR>

" Resize split
map <c-o> :vertical resize +10<CR>
map <c-_> :vertical resize -10<CR>

" FZF
map <c-p> :GFiles<CR>

" Formatting with Beautify
autocmd FileType html noremap <buffer> <leader>f :call HtmlBeautify()<cr>

" source $MYVIMRC reloads the saved $MYVIMRC
:nmap <Leader>s :source $MYVIMRC<CR>

" opens $MYVIMRC for editing, or use :tabedit $MYVIMRC
:nmap <Leader>v :e $MYVIMRC<CR>

" String replacement
nnoremap <Leader>r *``cgn
nnoremap <Leader>R #``cgN

" Camelcase search
nnoremap <C-Left> :call search('\<\<Bar>\u', 'bW')<CR>
nnoremap <C-Right> :call search('\<\<Bar>\u', 'W')<CR>

inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-x><C-o>

