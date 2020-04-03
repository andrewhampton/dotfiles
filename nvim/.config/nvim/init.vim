" Variables and Autocommands

let mapleader=" "
set number nowrap hidden shiftround splitright splitbelow
set undofile noswapfile incsearch ignorecase smartcase
set encoding=utf-8
set clipboard=unnamedplus
set updatetime=100
set undolevels=1000
set signcolumn=yes
set inccommand=nosplit

" Text file setup
augroup textBuffers
  autocmd!
  autocmd FileType markdown,text setlocal wrap spell list
augroup end

" Auto-resize whindows when host window resizes
augroup autoResize
  autocmd!
  autocmd VimResized * wincmd =
augroup END

" Plugins
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
Plug 'airblade/vim-rooter'
Plug 'andrewradev/splitjoin.vim'
Plug 'bounceme/dim-jump'
Plug 'brooth/far.vim'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
" Plug 'lambdalisue/gina.vim'
Plug 'kchmck/vim-coffee-script'
Plug 'leafgarland/typescript-vim'
" Plug 'neoclide/coc.nvim', {'tag': '*', 'do': './install.sh'}
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neomake/neomake'
Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdtree'
Plug 'sgur/vim-editorconfig'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-unimpaired'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" Plugin configuration
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

" quickfix nav
nnoremap <leader>cn :cn<Cr>
nnoremap <leader>cp :cp<Cr>

" rooter
let g:rooter_change_directory_for_non_project_files = 'home'

" airline
let g:airline_theme='tomorrow'
let g:airline_powerline_fonts = 1
let g:airline#extensions#hunks#enabled = 0

" nerdtree
nnoremap <F3> :NERDTreeToggle<cr>
nnoremap <S-F3> :NERDTreeFind<cr>
let g:NERDTreeShowHidden=1
nnoremap <leader>fn :vertical resize 30<cr>

" Custom keybindings
" Quickly edit vim config
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
" "0 shortcut
nnoremap <silent> <leader>p "0

" Reload from disk
nnoremap <leader>ct :checktime<cr>

""""""""""""
"""" coc """
""""""""""""
" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

" Shift tab navigates back
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Open file in github
nnoremap <silent> <leader>gh :<C-u>CocCommand git.browserOpen<cr>

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" augroup GoToDefinition
"   autocmd!
"   nnoremap <silent> gd :call fzf#vim#tags(expand('<cword>'))<Cr>
"   autocmd FileType typescript,javascript nnoremap <silent> gd <Plug>(coc-definition)
" augroup end

" nnoremap <silent> gd <Plug>(coc-definition)
" nnoremap <silent> gy <Plug>(coc-type-definition)
" nnoremap <silent> gi <Plug>(coc-implementation)
" nnoremap <silent> gr <Plug>(coc-references)

" dim jump config
let b:preferred_searcher="rg"
