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

" YouCompleteMe autocomplete for many languages
Plugin 'Valloric/YouCompleteMe'
let g:ycm_server_python_interpreter = '/usr/bin/python'

" Jellybeans.vim: Colorful, dark color scheme
Plugin 'nanotech/jellybeans.vim'

" Surround.vim: delete, change, and add surroundings in pairs
Plugin 'tpope/vim-surround'

" NERD tree, directory tree for displaying file system
Plugin 'scrooloose/nerdtree'

" NERD commenter, makes commenting easy
Plugin 'scrooloose/nerdcommenter'

" Elm plugin
Plugin 'ElmCast/elm-vim'
let g:ycm_semantic_triggers = { 'elm' : ['.'],}

" All plugins must be added before following line
call vundle#end()
filetype plugin indent on

" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" Set colorscheme to jellybeans
colorscheme jellybeans

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
