source ~/.plugins.vim
source ~/.mappings.vim

" Backspace fix on Mac
set backspace=indent,eol,start

" Display line numbers
set number

" Highlight searches
set hlsearch

" ../../../swap files
set directory=$HOME/.vim/swapfiles/

" Typescript Suggestions
let g:tsuquyomi_completion_detail = 1
let g:tsuquyomi_disable_quickfix=1
let g:tsuquyomi_shortest_import_path = 1

" Python version
let g:pymode_python = 'python3'


" no swap, no swag
set noswapfile

" Colorscheme
:colorscheme gruvbox

" ale config for vim
let g:airline#extensions#ale#enabled = 1

" 256 color support
set t_Co=256

" Auto ident
set ai

" Folding
set foldmethod=syntax

" Yanking into clipboard
if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif
