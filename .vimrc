source ~/.plugins.vim
source ~/.mappings.vim
source ~/.preview.vim

let &t_ut=''
" Backspace fix on Mac
set backspace=indent,eol,start
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
set colorcolumn=80

" Display line numbers
set number

" Highlight searches
set hlsearch

" NERDTree
let NERDTreeShowHidden=1

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
let g:gruvbox_italic = 1
let g:gruvbox_italicize_comments = 1

highlight Comment cterm=italic

" ale config for vim
let g:airline#extensions#ale#enabled = 1

" 256 color support
set t_Co=256

set smartcase
set smarttab
set smartindent
set autoindent
set softtabstop=2
set shiftwidth=2
set expandtab
set incsearch

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

" Enable persistent undo
set undodir=~/.vim/undodir
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload

" Ranger
let g:NERDTreeHijackNetrw = 0
let g:ranger_replace_netrw = 1
