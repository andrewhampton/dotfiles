" Variables and Autocommands {{{

let mapleader=" "
set number nowrap hidden shiftround splitright splitbelow incsearch ignorecase smartcase
set encoding=utf-8
set clipboard=unnamedplus
set updatetime=100
set undolevels=1000

" Text file setup
augroup textBuffers
  autocmd!
  autocmd FileType markdown,text setlocal wrap spell list
augroup end

augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END
" }}}
" Plugins {{{
" - ripgrep
" - git
" - universal ctags
" - ruby
" - typescript compiler
" - fira code
" - fzf
" - ~/.editorconfig

call plug#begin()

Plug '/usr/local/opt/fzf'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'andrewradev/splitjoin.vim'
Plug 'brooth/far.vim'
Plug 'junegunn/fzf.vim'
Plug 'lambdalisue/gina.vim'
Plug 'leafgarland/typescript-vim'
Plug 'neomake/neomake'
Plug 'ruanyl/vim-gh-line'
Plug 'scrooloose/nerdtree'
Plug 'sgur/vim-editorconfig'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" }}}
" Plugin configuration {{{
" neomake
call neomake#configure#automake('rw')
let g:neomake_open_list = 2

" Colors
colo Tomorrow-Night-Eighties

" fzf
nnoremap <leader>fs :Rg<Cr>
nnoremap <leader>ff :Files<Cr>
nnoremap <leader>ft :Tags<Cr>
nnoremap <leader>fb :Buffers<Cr>
nnoremap <leader>fr :History<Cr>
nnoremap <silent> gd :call fzf#vim#tags(expand('<cword>'))<Cr>

" fugitive
let g:EditorConfig_exclude_patters = ['fugitive://.*']

" rooter
let g:rooter_change_directory_for_non_project_files = 'home'

" airline
let g:airline_theme='tomorrow'
let g:airline_powerline_fonts = 1
let g:airline#extensions#hunks#enabled = 0

" nerdtree
nnoremap <F3> :NERDTreeToggle<cr>
nnoremap <Shift><F3> :NERDTreeFind<cr>
let g:NERDTreeShowHidden=1

" }}}
" Custom keybindings {{{
" Quickly edit vim config
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Quicklist navigation
nnoremap <leader>co :copen<cr>
nnoremap <leader>cj :cnext<cr>
nnoremap <leader>ck :cprevious<cr>
nnoremap <leader>cc :cclose<cr>

" Location list navigation
nnoremap <leader>lo :lopen<cr>
nnoremap <leader>lj :lnext<cr>
nnoremap <leader>lk :lprevious<cr>
nnoremap <leader>lc :lclose<cr>
