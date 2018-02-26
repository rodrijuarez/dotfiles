" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Plugins for JavaScript
Plug 'pangloss/vim-javascript'

" Asynchronous execution library for Vim
Plug 'Shougo/vimproc.vim', {'do' : 'make'}

" Syntax HighLighting
Plug 'leafgarland/typescript-vim'

" Prettier
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

" Airline
Plug 'bling/vim-airline'

" Nerdcommenter
Plug 'scrooloose/nerdcommenter'

" vim-jsbeautify
Plug 'maksimr/vim-jsbeautify'

" Auto pairs for brackets and other stuff...
Plug 'jiangmiao/auto-pairs'

" YouCompleteMe
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

" Plugins for TypeScript	
Plug 'Quramy/tsuquyomi'

" Fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Dev icons
Plug 'ryanoasis/vim-devicons'

" Themes changing
Plug 'flazz/vim-colorschemes'

" Replace with esteroides
Plug 'tpope/vim-abolish'

" Initialize plugin system
call plug#end()

" Backspace fix on Mac
set backspace=indent,eol,start

" Display line numbers
set number

" Change mapleader
let mapleader=","

" Highlight searches
set hlsearch

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Mapping NERDTree 
map <C-n> :NERDTreeToggle<CR>

" Typescript Suggestions
let g:tsuquyomi_completion_detail = 1
let g:tsuquyomi_disable_quickfix=1
let g:tsuquyomi_shortest_import_path = 1

:nmap <Leader>i :TsuImport<CR>
autocmd FileType typescript setlocal completeopt+=menu,preview

" ../../../swap files
set directory=$HOME/.vim/swapfiles/

" Highlight current line
"set cursorline

" Python version
let g:pymode_python = 'python3'

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

set noswapfile

set t_Co=256

if has('clipboard')
    if has('unnamedplus')  " When possible use + register for copy-paste
        set clipboard=unnamed,unnamedplus
    else         " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif
