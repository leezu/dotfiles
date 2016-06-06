" General settings
filetype plugin indent on
syntax on
set number
set showcmd   " Shows the partial command entered so far in the down right corner
set updatetime=250   " Recommended for vim-gitgutter
set hidden   " Only hide buffer when e.g. closing a file. → Undo history persists
set spell

"" Code formatting
set expandtab
set shiftwidth=4

" Mapping
let mapleader = " "
let maplocalleader = " "

"" Window navigation
map <leader><LEFT> :wincmd h<CR>
map <leader><DOWN> :wincmd j<CR>
map <leader><UP> :wincmd k<CR>
map <leader><RIGHT> :wincmd l<CR>

"" Often used commands
map <leader>w :w<CR>


" Plugins
call plug#begin('~/.vim/plugged')

Plug 'lervag/vimtex'
" Plug 'LaTeX-Box-Team/LaTeX-Box'
" Plug 'scrooloose/nerdcommenter'

"" Code formatting
Plug 'rhysd/vim-clang-format'
"{{
nmap <leader>k :<C-u>ClangFormat<CR>
vmap <leader>k :ClangFormat<CR>
autocmd FileType c ClangFormatAutoEnable

let g:clang_format#command = "clang-format-3.8"
let g:clang_format#code_style = "llvm"
let g:clang_format#style_options = {
            \ "IndentWidth" : 4}
let g:clang_format#auto_format = 1
let g:clang_format#auto_format_on_insert_leave = 1
"}}

"" Autocomplete
"" YouCompleteMe
""" After installation it is necessary to compile with ~/.vim/plugged/YouCompleteMe/install.py --all
Plug 'Valloric/YouCompleteMe'

"" C++ Debugger
Plug 'vim-scripts/Conque-GDB'

"" vim-sneak
"" Provides way to move quickly and precisely to locations that would be awkward to reach with built-in Vim motions. 
"" s (sneak forward) or S (sneak backwards), followed by exactly two characters
Plug 'justinmk/vim-sneak'
"{{
let g:sneak#streak = 1
"}}

"" unimpaired.vim: pairs of handy bracket mappings
Plug 'tpope/vim-unimpaired'

"" Automatically create and restore view sessions
Plug 'kopischke/vim-stay'
"{{
set viewoptions=cursor,folds,slash,unix
"}}

"" fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
"{{
nnoremap <leader><space> :Files<CR>
"}}

"" Pandoc
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'


"" LaTeX
"{{
""" Make vimtex work with neovim
""" Requires neovim-remote package to be installed
let g:vimtex_latexmk_progname='nvr'
""" Use Okular for viewing pdfs
let g:vimtex_view_general_viewer = 'okular'
let g:vimtex_view_general_options = '--unique @pdf\#src:@line@tex'
let g:vimtex_view_general_options_latexmk = '--unique'
let g:vimtex_quickfix_open_on_warning = 0
""" Folding
let g:vimtex_fold_enabled = 1
let g:vimtex_fold_manual = 1 " Folds are recomputed with zx / zX
"}}

"" R
Plug 'vim-scripts/Vim-R-plugin'

"" git
""" Integrate git command line
Plug 'tpope/vim-fugitive'
""" Show diff in sign column
Plug 'airblade/vim-gitgutter'

"" colorscheme
Plug 'frankier/neovim-colors-solarized-truecolor-only'

"" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"{{
let g:UltiSnipsExpandTrigger="<c-j>"
"}}

call plug#end()

" Colors
"" Support true colors
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set termguicolors
set background=dark " or light
colorscheme solarized
