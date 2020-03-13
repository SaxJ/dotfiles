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
Plug 'tpope/vim-vividchalk'

" GIT
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'

" GENERAL CODE
Plug 'scrooloose/nerdtree'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
Plug 'tpope/vim-surround'
Plug 'editorconfig/editorconfig-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'itchyny/calendar.vim'
Plug 'vimwiki/vimwiki'
Plug 'vim-vdebug/vdebug'
Plug 'sunaku/vim-dasht'
Plug 'Olical/vim-expand'
Plug 'jremmen/vim-ripgrep'
Plug 'wakatime/vim-wakatime'
Plug 'isobit/vim-caddyfile'
Plug 'jceb/vim-orgmode'
Plug 'liuchengxu/vista.vim'
Plug 'freitass/todo.txt-vim'

" HTML
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'mustache/vim-mustache-handlebars', { 'for': 'html' }
Plug 'lumiliet/vim-twig', { 'for': 'html' }
Plug 'rstacruz/sparkup', { 'for': 'html' }

" GRAPHQL
Plug 'jparise/vim-graphql', { 'for': 'graphql' }

" PYTHON
" Plug 'Shougo/vimproc.vim', {'do' : 'make', 'for': 'python'}
com! FormatJSON %!python -m json.tool

" DOTNET
Plug 'kongo2002/fsharp-vim', { 'for': 'fsharp', }
Plug 'OrangeT/vim-csharp'

" LATEX
Plug 'lervag/vimtex'
let g:tex_flavor = 'latex'

" PHP
Plug 'tobyS/vmustache', { 'for': 'php' }
Plug 'alvan/vim-php-manual',  { 'for': 'php' }
autocmd FileType php setl iskeyword+=$ " Make the $ in php variables part of the word
let g:php_manual_online_search_shortcut = ''

" GO
Plug 'fatih/vim-go'

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

" HASKELL
Plug 'pbrisbin/vim-syntax-shakespeare', { 'for': 'hamlet' }
Plug 'parsonsmatt/intero-neovim'

" MARKDOWN
Plug 'gabrielelana/vim-markdown', { 'for': 'markdown' }

" PURESCRIPT
Plug 'purescript-contrib/purescript-vim'

" ELM
Plug 'elmcast/elm-vim'

" PROLOG
Plug 'adimit/prolog.vim'

call plug#end()
filetype plugin indent on    " required

" LANGUAGE SERVER EXTENSIONS
let g:coc_global_extensions = ['coc-tslint-plugin', 'coc-tsserver', 'coc-emmet', 'coc-css', 'coc-html', 'coc-json', 'coc-yank', 'coc-prettier', 'coc-python', 'coc-omnisharp']

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
filetype plugin indent on
autocmd BufWritePost *.php %s/\$\$/\$/ge
set pastetoggle=<F2>

" Important shit
nnoremap ; :
let mapleader=","

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
nmap <Leader>cl :let @*=expand("%:p")<CR>  
nmap <Leader>cs :let @*=expand("%")<CR>

" SEARCH HIGHLIGHTED THING
vnoremap // y/<C-R>"<CR>

" FILE BROWSER
noremap <C-e> :NERDTreeToggle<CR>
let g:netrw_banner = 0
let g:netrw_liststyle = 3

" TAB-COMPLETE FOR AUTOCOMPLETE
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

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
colorscheme vividchalk

" PHP CBF
map <Leader>cbf :! phpcbf --standard=PSR2 %<CR>

" VDEBUG
nnoremap <silent> <leader>e :VdebugEval

" FZF
let g:fzf_nvim_statusline = 0 " disable statusline overwriting
map <Leader>bt :BTags<CR>

nnoremap <silent> <leader><space> :GFiles<CR>
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

" START SCREEN
function! MyStartscreen()
	let l:fortune = systemlist('fortune -a')
	call append('0', ['', ''] + map(l:fortune, '"        " . v:val'))
	:1
	redraw!

	nnoremap <buffer> <silent> <Return> :enew<CR>:call startscreen#start()<CR>
endfun
let g:Startscreen_function = function('MyStartscreen') 

" COC
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> ne <Plug>(coc-diagnostic-next)
nmap <silent> Ne <Plug>(coc-diagnostic-prev)

" ORG MODE
nnoremap <silent> <leader>o :e ~/Dropbox/org/Todo.org<CR>

" INTERO
augroup interoMaps
  au!
  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.

  " Background process and window management
  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>

  " Open intero/GHCi split horizontally
  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  " Open intero/GHCi split vertically
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>

  " Reloading (pick one)
  " Automatically reload on save
  au BufWritePost *.hs InteroReload
  " Manually save and reload
  au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>

  " Load individual modules
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  " Type-related information
  " Heads up! These next two differ from the rest.
  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
  au FileType haskell map <silent> <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  " Navigation
  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>

  " Managing targets
  " Prompts you to enter targets (no silent):
  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
augroup END

" Intero starts automatically. Set this if you'd like to prevent that.
let g:intero_start_immediately = 1

" Enable type information on hover (when holding cursor at point for ~1 second).
let g:intero_type_on_hover = 1

" Change the intero window size; default is 10.
let g:intero_window_size = 15

" Sets the intero window to split vertically; default is horizontal
let g:intero_vertical_split = 1

let g:intero_backend = {
    \ 'command': 'ghci',
    \ 'cwd': expand('%:p:h'),
    \ }

" OPTIONAL: Make the update time shorter, so the type info will trigger faster.
set updatetime=500

" TODO
nnoremap <silent> <leader>t :e ~/Dropbox/todo/todo.txt<CR>
