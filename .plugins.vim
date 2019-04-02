" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
"Plug 'tpope/vim-vinegar'
Plug 'francoiscabrol/ranger.vim'
Plug 'rbgrouleff/bclose.vim'

" Asynchronous execution library for Vim
Plug 'Shougo/vimproc.vim', {'do' : 'make'}

" Syntax HighLighting
Plug 'leafgarland/typescript-vim'
Plug 'elzr/vim-json'

" Others
Plug 'yardnsm/vim-import-cost', { 'do': 'npm install' }

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
Plug 'jremmen/vim-ripgrep'

" Dev icons
Plug 'ryanoasis/vim-devicons'

" Themes changing
Plug 'flazz/vim-colorschemes'

" Replace with esteroides
Plug 'tpope/vim-abolish'

" Haskell
Plug 'w0rp/ale'
Plug 'alx741/vim-hindent'
Plug 'itchyny/vim-haskell-indent'
Plug 'Shougo/vimproc'
Plug 'vim-scripts/haskell.vim'

" Elm
Plug 'elmcast/elm-vim'

" Editor config
Plug 'editorconfig/editorconfig-vim'

" Surround
Plug 'tpope/vim-surround'

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Clojure
Plug 'vim-scripts/VimClojure'
Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'luochen1990/rainbow'
Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'

" org-mode
Plug 'jceb/vim-orgmode'
Plug 'tpope/vim-speeddating'

" Vue
Plug 'posva/vim-vue'

" PureScript
Plug 'purescript-contrib/purescript-vim', { 'for': ['purescript', 'purs'] }
Plug 'FrigoEU/psc-ide-vim', { 'for': ['purescript', 'purs'] }

" The trial
Plug 'rhysd/git-messenger.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-repeat'
Plug 'metakirby5/codi.vim'

" Initialize plugin system
call plug#end()
