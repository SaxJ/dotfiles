set nocompatible              " be iMproved, required
syntax on
filetype off                  " required

" set the runtime path to include Vundle and initialize
call plug#begin('~/.vim/plugged')
"======================================================
"PLUGINS
"COLOUR THEME
Plug 'embark-theme/vim', { 'as': 'embark' }
Plug 'morhetz/gruvbox', { 'as': 'gruvbox' }

" GIT
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'
Plug 'Xuyuanp/nerdtree-git-plugin'

" GENERAL CODE
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
Plug 'Olical/vim-expand'
Plug 'jremmen/vim-ripgrep'
Plug 'isobit/vim-caddyfile'
Plug 'liuchengxu/vista.vim'
Plug 'sheerun/vim-polyglot'
Plug 'sbdchd/neoformat'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch.vim'
Plug 'neovim/nvim-lspconfig'
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
set inccommand=nosplit

let php_sql_query = 1

" some things have trouble with backup files
set nobackup
set nowritebackup

" give more message space
set cmdheight=2

set updatetime=300
set shortmess+=c
set signcolumn=yes

filetype plugin indent on

" LEADER KEYS
nnoremap ; :
let mapleader="\<Space>"
let maplocalleader=","

" SEARCHING
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)

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

" Commenting
nnoremap <Leader>cm :Commentary<CR>

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
colorscheme embark
let g:lightline = {
      \ 'colorscheme': 'embark',
      \ }

" PHP CBF
map <Leader>cbf :! phpcbf --standard=PSR2 %<CR>
map <localleader> <Plug>(easymotion-prefix)

" FZF
let g:fzf_nvim_statusline = 0 " disable statusline overwriting
map <Leader>bt :BTags<CR>

nnoremap <silent> <leader><leader> :Files<CR>
nnoremap <silent> <leader>ft :Filestypes<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>A :Windows<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>o :BTags<CR>
nnoremap <silent> <leader>? :History<CR>
nnoremap <silent> <leader>/ :Rg<CR>

nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>

autocmd VimEnter * command! -bang -nargs=* Rg
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
\ <bang>0)

" WIKI
let g:vimwiki_list = [{'path': '~/wiki/'}]

nnoremap <silent> <Leader>fj :%!jq .<CR>


map <localleader> <Plug>(easymotion-prefix)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TAB COMPLETION
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
set completeopt=menuone,noinsert,noselect
set shortmess+=c

inoremap <silent><expr> <c-space> coc#refresh()

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

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

nnoremap <silent> ff    <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gI    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.references()<CR>

" Use completion-nvim in every buffer
autocmd BufEnter * lua require'completion'.on_attach()

lua <<EOF
require'nvim_lsp'.ccls.setup{}
require'nvim_lsp'.vimls.setup{}
require'nvim_lsp'.intelephense.setup{}
require'nvim_lsp'.tsserver.setup{}
require'nvim_lsp'.omnisharp.setup{}
require'nvim_lsp'.purescriptls.setup{}
require'nvim_lsp'.pyls.setup{}
require'nvim_lsp'.hls.setup{}
EOF
