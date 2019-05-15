let mapleader=" "
set number nowrap hidden shiftround
set clipboard=unnamedplus
set updatetime=100

" Quickly edit vim config
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Quality of life mappings
nnoremap H ^
nnoremap L $

" Plugin Dependencies:
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
Plug 'leafgarland/typescript-vim'
Plug 'neomake/neomake'
Plug 'ruanyl/vim-gh-line'
Plug 'sgur/vim-editorconfig'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" neomake
call neomake#configure#automake('rw')
let g:neomake_open_list = 2

" Colors
colo Tomorrow-Night-Eighties

" fzf
nnoremap <leader>s :Rg<Cr>
nnoremap <leader>f :Files<Cr>
nnoremap <leader>t :Tags<Cr>
nnoremap <leader>b :Buffers<Cr>
nnoremap <leader>r :History<Cr>
nnoremap <silent> gd :call fzf#vim#tags(expand('<cword>'))<Cr>

" fugitive
let g:EditorConfig_exclude_patters = ['fugitive://.*']

" rooter
let g:rooter_change_directory_for_non_project_files = 'home'

" airline
let g:airline_theme='tomorrow'
let g:airline_powerline_fonts = 1
let g:airline#extensions#hunks#enabled = 0

" Triger `autoread` when files changes on disk
" https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
" https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
" Notification after file change
" https://vi.stackexchange.com/questions/13091/autocmd-event-for-autoread
autocmd FileChangedShellPost *
  \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
