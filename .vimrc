source ~/.plugins.vim
source ~/.mappings.vim

" Backspace fix on Mac
set backspace=indent,eol,start

" Display line numbers
set number

" Highlight searches
set hlsearch

" Typescript Suggestions
let g:tsuquyomi_disable_quickfix=1
let g:tsuquyomi_shortest_import_path = 1

" Python version
let g:pymode_python = 'python3'


" no swap, no swag
set noswapfile

" hidden
set hidden

" Colorscheme
:colorscheme gruvbox

" ale config for vim
let g:airline#extensions#ale#enabled = 1

" 256 color support
set t_Co=256

filetype plugin on
set omnifunc=syntaxcomplete#Complete

" Yanking into clipboard
if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif
