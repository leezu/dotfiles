" Plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'LaTeX-Box-Team/LaTeX-Box'

call plug#end()

" General settings
filetype plugin indent on
syntax on
set number
colorscheme desert

" LaTeX-Box settings
let g:LatexBox_latexmk_preview_continuously = 1
let g:LatexBox_quickfix = 2
