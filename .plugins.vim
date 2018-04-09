" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-vinegar'

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

" Plugins for TypeScript	
Plug 'Quramy/tsuquyomi'

" YouCompleteMe
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

" Fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Dev icons
Plug 'ryanoasis/vim-devicons'

" Themes changing
Plug 'flazz/vim-colorschemes'

" Replace with esteroides
Plug 'tpope/vim-abolish'

" Haskell
Plug 'itchyny/vim-haskell-indent'
Plug 'w0rp/ale'

" Track the engine.
Plug 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'

" Initialize plugin system
call plug#end()

