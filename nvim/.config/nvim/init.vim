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
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" neomake
call neomake#configure#automake('nrwi', 500)

" Colors
colo Tomorrow-Night-Eighties

" fzf
nnoremap <C-g> :Rg<Cr>
nnoremap <C-p> :Files<Cr>
nnoremap <C-t> :Tags<Cr>
nnoremap <C-x>b :Buffers<Cr>
nnoremap <C-x>r :History<Cr>
nnoremap <C-x>g :Rg expand('<cword>')
nnoremap <silent> gd :call fzf#vim#tags(expand('<cword>'))<Cr>

" fugitive
let g:EditorConfig_exclude_patters = ['fugitive://.*']

" rooter
let g:rooter_change_directory_for_non_project_files = 'home' 

" airline
let g:airline_theme='tomorrow'

""""""""""""""""""""""""""""""
"""" misc quality of life """"
""""""""""""""""""""""""""""""
set number nowrap
let mapleader=" "

" Allow switching from unsaved buffers
set hidden

" system clipboard by default
set clipboard=unnamedplus

" Triger `autoread` when files changes on disk
" https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
" https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
" Notification after file change
" https://vi.stackexchange.com/questions/13091/autocmd-event-for-autoread
autocmd FileChangedShellPost *
  \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
