" A minimal vimrc for new vim users to start with.
"
" Referenced here: http://vimuniversity.com/samples/your-first-vimrc-should-be-nearly-empty
"
" Original Author:	     Bram Moolenaar <Bram@vim.org>
" Made more minimal by:  Ben Orenstein
" Modified by :          Ben McCormick
" Last change:	         2014 June 8
"
" To use it, copy it to
"  for Unix based systems (including OSX and Linux):  ~/.vimrc
"  for Windows :  $VIM\_vimrc
"
"  If you don't understand a setting in here, just type ':h setting'.

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Required for Vundle
filetype off

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Let Vundle manage Vungle, required
Plugin 'VundleVim/Vundle.vim'

" Surround.vim: delete, change, and add surroundings in pairs
Plugin 'tpope/vim-surround'

" NERD tree, directory tree for displaying file system
Plugin 'scrooloose/nerdtree'

" NERD commenter, makes commenting easy
Plugin 'scrooloose/nerdcommenter'

" status line plugin
Plugin 'vim-airline/vim-airline'

" Material colorscheme
Plugin 'jdkanani/vim-material-theme'

" All plugins must be added before following line
call vundle#end()
filetype plugin indent on

" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" Set colorscheme to jellybeans
colorscheme material-theme

" other color stuff
if $TERM == "xterm-256color"
	set t_Co=256
endif

" Switch syntax highlighting on
if !exists("g:syntax_on")
	syntax enable 
endif

" Enable file type detection and do language-dependent indenting.
filetype plugin indent on

" Show line numbers
set number

" Allow hidden buffers, don't limit to 1 file per window/split
set hidden

" Increase number of commands vim can save
set history=100

" Use system clipboard for copy paste
set clipboard=unnamedplus

" remap some frame switching
:noremap <C-j> <C-w>j
:noremap <C-k> <C-w>k
:noremap <C-h> <C-w>h
:noremap <C-l> <C-w>l
:noremap <C-w> <C-w>w

" set airline to always appear
set laststatus=2
