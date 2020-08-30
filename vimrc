set nocompatible              " be iMproved, required
syntax on
filetype off                  " required

" set the runtime path to include Vundle and initialize
call plug#begin('~/.vim/plugged')
"======================================================
"PLUGINS
"COLOUR THEME
Plug 'dikiaap/minimalist', { 'as': 'minimalist' }
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'crusoexia/vim-monokai', { 'as': 'monokai' }
Plug 'tpope/vim-vividchalk', { 'as': 'vividchalk' }
Plug 'kyoz/purify', { 'rtp': 'vim' }
Plug 'morhetz/gruvbox', { 'as': 'gruvbox' }

" GIT
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'

" GENERAL CODE
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tpope/vim-surround'
Plug 'editorconfig/editorconfig-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
Plug 'sunaku/vim-dasht'
Plug 'Olical/vim-expand'
Plug 'jremmen/vim-ripgrep'
Plug 'isobit/vim-caddyfile'
Plug 'liuchengxu/vista.vim'
Plug 'sheerun/vim-polyglot'
Plug 'neovim/nvim-lsp'
Plug 'nvim-lua/completion-nvim'

" HTML
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'mustache/vim-mustache-handlebars', { 'for': 'html' }
Plug 'lumiliet/vim-twig', { 'for': 'html' }

" GRAPHQL
Plug 'jparise/vim-graphql', { 'for': 'graphql' }

" LATEX
Plug 'lervag/vimtex'
let g:tex_flavor = 'latex'

" PHP
autocmd FileType php setl iskeyword+=$ " Make the $ in php variables part of the word
let g:php_manual_online_search_shortcut = ''

" TYPESCRIPT
Plug 'ianks/vim-tsx'
Plug 'leafgarland/typescript-vim'
au BufNewFile,BufRead *.ts setlocal filetype=typescript
au BufNewFile,BufRead *.tsx setlocal filetype=typescriptreact
autocmd FileType typescript setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType typescriptreact setlocal shiftwidth=2 softtabstop=2 expandtab

" JAVASCRIPT
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'maksimr/vim-jsbeautify', { 'for': 'javascript' }
Plug 'Quramy/vim-js-pretty-template', { 'for': 'javascript' }
Plug 'posva/vim-vue', { 'for': 'vue' }

" MARKDOWN
Plug 'gabrielelana/vim-markdown', { 'for': 'markdown' }

" PURESCRIPT
Plug 'purescript-contrib/purescript-vim'

" PROLOG
Plug 'adimit/prolog.vim'

call plug#end()
filetype plugin indent on    " required

"SETTINGS
set softtabstop=4
set shiftwidth=4
set tabstop=4
set expandtab
set nowrap
set backspace=indent,eol,start
set autoindent
set copyindent
set number
set relativenumber
set shiftround
set showmatch
set ignorecase
set smartcase
set smarttab
set hlsearch
set incsearch
set history=1000
set undolevels=1000
set wildignore=*.swp,*.pyc,*.class
set title
set visualbell
set t_vb=
set noerrorbells
set hidden
set laststatus=2
set scroll=2
set t_Co=256
set clipboard=unnamedplus
set noswapfile
set cursorline
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m

" some things have trouble with backup files
set nobackup
set nowritebackup

" give more message space
set cmdheight=2

set updatetime=300
set shortmess+=c
set signcolumn=yes

filetype plugin indent on
autocmd BufWritePost *.php %s/\$\$/\$/ge
set pastetoggle=<F2>

" LEADER KEYS
nnoremap ; :
let mapleader="\<Space>"
let maplocalleader=","

" STATUS LINE
function! GitBranch()
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

set statusline=
set statusline+=%#PmenuSel#
set statusline+=%{StatuslineGit()}
set statusline+=%#LineNr#
set statusline+=\ %f
set statusline+=%m\
set statusline+=%=
set statusline+=%#CursorColumn#
set statusline+=\ %y
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\[%{&fileformat}\]
set statusline+=\ %p%%
set statusline+=\ %l:%c
set statusline+=\ 

" EASY WINDOW NAVIGATION
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <Leader>v :vsplit<CR>

" BETTER MODES
map <C-c> <Esc>

" Shell
nnoremap <Leader>s :terminal<CR>

" COPY FILE PATH
nmap <Leader>cp :let @+=expand("%")<CR>

" YANK ENTRIRE FILE
nnoremap <silent> <Leader>Y ggVGy

" SEARCH HIGHLIGHTED THING
vnoremap // y/<C-R>"<CR>

" FILE BROWSER
noremap <C-e> :NERDTreeToggle<CR>
let NERDTreeQuitOnOpen = 1
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" NETRW CLEANUP
let g:netrw_banner = 0
let g:netrw_liststyle = 3

" TAB COMPLETION
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
set completeopt=menuone,noinsert,noselect
set shortmess+=c

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ completion#trigger_completion()

" Enter to confirm completion
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" GIT BINDINGS
map <Leader>gs :Gstatus<CR>
map <Leader>gd :Gdiff<CR>
map <Leader>gc :Gcommit<CR>
map <Leader>gw :Gwrite<CR>
map <Leader>gp :Gpush<CR>
map <Leader>gu :Gpull<CR>

" BETTER BEHAVIOUR ON TERMINAL MODE
tnoremap <Esc> <C-\><C-n>

" HANDLEBARS SYNTAX
if has("autocmd")
  au  BufNewFile,BufRead *.mustache,*.hogan,*.hulk,*.hjs set filetype=html.mustache syntax=mustache | runtime! ftplugin/mustache.vim ftplugin/mustache*.vim ftplugin/mustache/*.vim
  au  BufNewFile,BufRead *.handlebar,*.handlebars,*.hbs set filetype=html.handlebars syntax=mustache | runtime! ftplugin/mustache.vim ftplugin/mustache*.vim ftplugin/mustache/*.vim
endif

" FORCE MARKDOWN ON *.md FILES
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
au BufRead,BufNewFile *.md setlocal textwidth=80

" COLOURS
set t_Co=256 
syntax on 
colorscheme gruvbox

" PHP CBF
map <Leader>cbf :! phpcbf --standard=PSR2 %<CR>

" FZF
let g:fzf_nvim_statusline = 0 " disable statusline overwriting
map <Leader>bt :BTags<CR>

nnoremap <silent> <leader><space> :Files<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>A :Windows<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>o :BTags<CR>
nnoremap <silent> <leader>? :History<CR>
nnoremap <silent> <leader>/ :execute 'Rg ' . input('Rg/')<CR>
nnoremap <silent> <leader>. :RgIn 

nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>
nnoremap <silent> <leader>ft :Filetypes<CR>

" SEARCHING WITH RG
function! SearchWordWithRg()
  execute 'Rg' expand('<cword>')
endfunction

function! SearchVisualSelectionWithRg() range
  let old_reg = getreg('"')
  let old_regtype = getregtype('"')
  let old_clipboard = &clipboard
  set clipboard&
  normal! ""gvy
  let selection = getreg('"')
  call setreg('"', old_reg, old_regtype)
  let &clipboard = old_clipboard
  execute 'Rg' selection
endfunction

autocmd VimEnter * command! -bang -nargs=* Rg
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
\ <bang>0)

" WIKI
let g:vimwiki_list = [{'path': '~/.dotfiles/wiki/'}]

nnoremap <silent> <Leader>fj :%!jq .<CR>

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
autocmd BufEnter * lua require'completion'.on_attach()

lua <<EOF
require'nvim_lsp'.ccls.setup{}
require'nvim_lsp'.vimls.setup{}
require'nvim_lsp'.intelephense.setup{}
require'nvim_lsp'.tsserver.setup{}
EOF
