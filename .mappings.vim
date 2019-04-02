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


autocmd FileType typescript nmap <Leader>i :TsuImport<CR>
autocmd FileType typescript nmap <Leader>g :TsuTypeDefinition<CR>
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

" Haskell
nnoremap <Leader>ht :GhcModType<cr>
nnoremap <Leader>htc :GhcModTypeClear<cr>
nnoremap <leader>tsh :echo tsuquyomi#hint()<cr>
autocmd FileType haskell nnoremap <buffer> <leader>? :call ale#cursor#ShowCursorDetail()<cr>



let g:intero_start_immediately = 1
" Load the neomake plugin files
autocmd FileType haskell packadd neomake
" Load the intero-vim plugin files
autocmd FileType haskell packadd intero-vim

" Start intero
autocmd FileType haskell nnoremap <leader>m :InteroStart<CR>
" Kill intero
autocmd FileType haskell nnoremap <leader>mk :InteroKill<CR>

" Open intero/GHCi split horizontally
autocmd FileType haskell nnoremap <leader>mo :InteroOpen<CR>
" Hide intero/GHCi split
autocmd FileType haskell nnoremap <leader>mh :InteroHide<CR>

" Manually save and reload
autocmd FileType haskell nnoremap <leader>mr :InteroReload<CR>

" Load individual modules
autocmd FileType haskell nnoremap <leader>mm :InteroLoadCurrentModule<CR>
autocmd FileType haskell nnoremap <leader>mf :InteroLoadCurrentFile<CR>

" Type-related information
autocmd FileType haskell nnoremap <leader>mt :InteroGenericType<CR>
autocmd FileType haskell nnoremap <leader>mT :InteroType<CR>

" Insert type above identifier under cursor
autocmd FileType haskell nnoremap <leader>mi :InteroTypeInsert<CR>

" Navigation
autocmd FileType haskell nnoremap <leader>mg :InteroGoToDef<CR>

" Managing targets
" Prompts you to enter targets (no silent):
" autocmd FileType haskell nnoremap <leader>is :InteroSetTargets<SPACE>
"augroup END
" }}}
map <+> :RangerCurrentFile<CR>
