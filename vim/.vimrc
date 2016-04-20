" General settings
filetype plugin indent on
syntax on
set number
colorscheme desert
set showcmd   " Shows the partial command entered so far in the down right corner

let mapleader = " "
let maplocalleader = " "

" Plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'lervag/vimtex'
" Plug 'LaTeX-Box-Team/LaTeX-Box'
" Plug 'tpope/vim-unimpaired'
" Plug 'scrooloose/nerdcommenter'
Plug 'ervandew/supertab'
Plug 'justinmk/vim-sneak'

" vimtex
"" Make vimtex work with neovim
"" Requires neovim-remote package to be installed
let g:vimtex_latexmk_progname='nvr'
"" Use Okular for viewing pdfs
let g:vimtex_view_general_viewer = 'okular'
let g:vimtex_view_general_options = '--unique @pdf\#src:@line@tex'
let g:vimtex_view_general_options_latexmk = '--unique'

" Programming in R
Plug 'vim-scripts/Vim-R-plugin'

call plug#end()


