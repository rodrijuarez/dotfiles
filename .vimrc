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
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
let g:ctrlp_custom_ignore = '\node_modules\|DS_Store\|git\|v[\/]\.(git|hg|svn)$'
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" ../../../swap files
set directory=$HOME/.vim/swapfiles/

" Highlight current line
set cursorline

" Python version
let g:pymode_python = 'python3'

" Resize split
map <c-o> :vertical resize +10<CR>
map <c-_> :vertical resize -10<CR>

" Formatting with Beautify
autocmd FileType html noremap <buffer> <leader>f :call HtmlBeautify()<cr>

" source $MYVIMRC reloads the saved $MYVIMRC
:nmap <Leader>s :source $MYVIMRC<CR>

" opens $MYVIMRC for editing, or use :tabedit $MYVIMRC
:nmap <Leader>v :e $MYVIMRC<CR>
