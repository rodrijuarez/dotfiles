" Change mapleader
let mapleader=","

" Mapping NERDTree
function NERDTreeYankCurrentNode()
    let n = g:NERDTreeFileNode.GetSelected()
    if n != {}
        call setreg('"', n.path.str())
    endif
endfunction

map <C-n> :NERDTreeToggle<CR>
:nmap <Leader>z :call NERDTreeYankCurrentNode()<CR>
:nmap <Leader>nf :NERDTreeFind<CR>


autocmd FileType typescript nmap <Leader>i :TsuImport<CR>
autocmd FileType typescript nmap <Leader>f :TsuTypeDefinition<CR>
autocmd FileType typescript nmap <buffer> <Leader>t : <C-u>echo tsuquyomi#hint()<CR>
autocmd FileType typescript setlocal completeopt+=menu,preview
autocmd FileType typescript nmap <buffer> <Leader>t : <C-u>echo tsuquyomi#hint()<CR>

" Resize split
map <c-o> :vertical resize +10<CR>
map <c-_> :vertical resize -10<CR>

" FZF
map <c-f> :Files<CR>
map <c-p> :GFiles<CR>

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

" copen
nnoremap <S-Tab> :cn<CR>

inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-x><C-o>

" Clojure
let g:rainbow_active = 1
autocmd FileType clojure nmap <Leader>p :Cljfmt<CR>

