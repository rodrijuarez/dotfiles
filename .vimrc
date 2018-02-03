" Use the Solarized Dark theme
set background=dark
colorscheme solarized
let g:solarized_termtrans=1

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
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

" Plugins for TypeScript
Plug 'Quramy/tsuquyomi'

" Syntax HighLighting
Plug 'leafgarland/typescript-vim'

" Prettier
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

" Airline
Plug 'bling/vim-airline'

" Nerdcommenter
Plug 'scrooloose/nerdcommenter'

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

" ctrlp
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

" swap files
set directory=$HOME/.vim/swapfiles/

" Typescript Suggestions
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-x><C-o>
let g:tsuquyomi_completion_detail = 2

" Highlight current line
set cursorline

" Python version
let g:pymode_python = 'python3'
