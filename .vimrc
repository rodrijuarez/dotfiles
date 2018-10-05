source ~/.plugins.vim
source ~/.mappings.vim
source ~/.tags.vim

let &t_ut=''
" Backspace fix on Mac
set backspace=indent,eol,start
set colorcolumn=80

" Display line numbers
set number

" Highlight searches
set hlsearch

" NERDTree
let NERDTreeShowHidden=1
let NERDTreeHijackNetrw=1

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

highlight Comment cterm=italic
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
"set termguicolors

" ale config for vim
let g:airline#extensions#ale#enabled = 1

" 256 color support
set t_Co=256

" Identation and syntax completion
filetype plugin on
filetype plugin indent on

set omnifunc=syntaxcomplete#Complete

set relativenumber
set cursorline

" cursorline
set lazyredraw
set ttyfast
set regexpengine=1
set noshowcmd

" characters display
set list
set listchars=space:·

" Yanking into clipboard
if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif
