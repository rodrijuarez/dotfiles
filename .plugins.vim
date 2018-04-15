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
Plug 'w0rp/ale'

" Beautify HTML
Plug 'maksimr/vim-jsbeautify'

" Elm
Plug 'elmcast/elm-vim'

" Editor config
Plug 'editorconfig/editorconfig-vim'

" Surround
Plug 'tpope/vim-surround'

" Git
Plug 'airblade/vim-gitgutter'

" Tags
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'majutsushi/tagbar'

" Identation
Plug 'nathanaelkane/vim-indent-guides'

" Initialize plugin system
call plug#end()

