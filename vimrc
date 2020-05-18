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

" GIT
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'

" GENERAL CODE
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'neoclide/coc.nvim', {'branch': 'release', 'tag': '*', 'do': { -> coc#util#install()}}
Plug 'tpope/vim-surround'
Plug 'editorconfig/editorconfig-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
Plug 'vim-vdebug/vdebug'
Plug 'sunaku/vim-dasht'
Plug 'Olical/vim-expand'
Plug 'jremmen/vim-ripgrep'
Plug 'wakatime/vim-wakatime'
Plug 'isobit/vim-caddyfile'
Plug 'liuchengxu/vista.vim'
Plug 'sheerun/vim-polyglot'

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

" MARKDOWN
Plug 'gabrielelana/vim-markdown', { 'for': 'markdown' }

" PURESCRIPT
Plug 'purescript-contrib/purescript-vim'

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

" TAB-COMPLETE FOR AUTOCOMPLETE
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Trigger completion with ctrl-space
inoremap <silent><expr> <c-space> coc#refresh()

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
colorscheme purify

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

" WIKI
let g:vimwiki_list = [{'path': '~/.dotfiles/wiki/'}]

" COC
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> dn <Plug>(coc-diagnostic-next)
nmap <silent> dN <Plug>(coc-diagnostic-prev)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for selections ranges.
" NOTE: Requires 'textDocument/selectionRange' support from the language server.
" coc-tsserver, coc-python are the examples of servers that support it.
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

nnoremap <silent> <Leader>fj :%!jq .<CR>
