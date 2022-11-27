lua require('plugins')

" {{{1 global settings
set nocompatible
set vb
set backspace+=indent,eol,start
set expandtab
set shiftwidth=2
set tabstop=2
set softtabstop=2
set fo=tcrqnlv1
set tw=80
set wrap
set linebreak
set ai
set mouse=a
set hidden
set number
set laststatus=2
set termguicolors " should be conditional?
set wildmenu
set wildmode=list:longest
set ignorecase " ignore case when searching
set smartcase  " don't ignore case when there's one captial letter
" set gdefault " /g by default for replace || very easy to forget and be confused
" set rnu " relative line numbers (disabled, they're slow)
set undofile " persistent per-file undo

set incsearch
set showmatch
set hlsearch

set shortmess+=I " don't show intro when starting vim
set dir=~/tmp/vim//,~/tmp//,/var/tmp//,/tmp//" keep swap files in this locations
set bdir=~/tmp/vim//,~/tmp//,/var/tmp//,/tmp//" keep backup files in this locations
set undodir=~/tmp/vim,~/tmp,/var/tmp,/tmp,." keep undo files in this locations
set tags=tags; " look for tags in current folder, then in parent folder etc

" vim-markdown: don't conceal characters on the cursor line
" set cocu=
" let g:vim_markdown_strikethrough = 1
" let g:vim_markdown_toc_autofit = 1
" let g:vim_markdown_folding_disabled = 1
let g:markdown_fenced_languages = ['sh', 'sql', 'go']
let g:markdown_folding = 1
let g:indentLine_concealcursor=''
set concealcursor=

let g:dark = filereadable(expand('~/.dark')) && $TERMINAL_EMULATOR != 'JetBrains-JediTerm'
" }}}

" {{{1 color tweaks

if g:dark
  set background=dark
else
  set background=light
endif

if $TERMINAL_EMULATOR == 'JetBrains-JediTerm'
  colorscheme blue
else
  colorscheme NeoSolarized
endif

hi clear SignColumn
" completion menu is ugly
hi Pmenu ctermbg=gray
hi PmenuSel ctermbg=227

if has("gui_running") || has("gui_macvim") || has("gui_vimr") || exists("g:neovide") || exists("g:fvim_loaded")
  if has("macunix") || has("gui_macvim")
    let g:gui_font_name = 'Hack\ Nerd\ Font\ Mono'
    let g:gui_font_size = 15
    " set guifont=Hack\ Nerd\ Font\ Mono:h15
  else
    let g:gui_font_name = 'FuraCode Nerd Font Mono'
    let g:gui_font_size = 13
    " set guifont=FuraCode\ Nerd\ Font\ Mono:h13
  endif
  function! ResizeFont(delta)
    let g:gui_font_size = g:gui_font_size + a:delta
    silent! execute('set guifont='.g:gui_font_name.':h'.g:gui_font_size)
  endfunction
  call ResizeFont(0)
  noremap <expr><C-=> ResizeFont(1)
  noremap <expr><C--> ResizeFont(-1)
else
  try
    " colorscheme chela_light
    if ! g:dark
      hi LineNr ctermbg=lightgray
    endif
    hi Search ctermbg=227
  catch
  endtry
endif
hi VisualNOS term=reverse ctermbg=220 cterm=none
" set listchars=tab:▸┈,nbsp:␣,trail:•,extends:⟩,precedes:⟨
" if has("gui_running")
  set list
  set showbreak=↪\ 
  set listchars=tab:▸┈,eol:¬,nbsp:␣,trail:•,extends:⟩,precedes:⟨
  hi NonText guifg=grey80
  if g:dark
    hi SpecialKey guifg=red
    " hi NonText guibg=grey20 guifg=grey30
    hi NonText guifg=grey30
  endif
" end
set colorcolumn=120

function Light()
  set background=light
  hi NonText guifg=grey80
endfunction
function Dark()
  set background=dark
  hi NonText guifg=grey30
endfunction
command Light call Light()
command Dark call Dark()
" }}}

" {{{1 frontend config
if exists("g:neovide")
  let g:neovide_remember_window_size = v:true
  let g:neovide_cursor_trail_size = 0.3
  let g:neovide_cursor_vfx_mode = "railgun"
endif

if exists("g:fvim_loaded")
  FVimCursorSmoothMove v:true
  FVimCursorSmoothBlink v:true
endif
" }}}
"
" {{{1 bindings
" {{{2 alt-1 to find current file
if exists("g:NERDTree")
  nnoremap <silent> 1 :NERDTreeFind<CR>
  nnoremap <silent> ! :NERDTreeFind<CR>
  nnoremap <silent> ¡ :NERDTreeFind<CR>
else
  nnoremap <silent> <M-1> :NvimTreeFindFile<CR>
  nnoremap <silent> 1 :NvimTreeFindFile<CR>
  nnoremap <silent> ! :NvimTreeFindFile<CR>
  nnoremap <silent> <M-O>3P :NvimTreeFindFile<CR>
  nnoremap <silent> <M-F1> :NvimTreeFindFile<CR>
  nnoremap <silent> ¡ :NvimTreeFindFile<CR>
endif
" }}}

" {{{2 commenting
nnoremap <silent>  :CommentToggle<CR>j
nnoremap <silent> <C-/> :CommentToggle<CR>j
vnoremap <silent>  :CommentToggle<CR>
vnoremap <silent> <C-/> :CommentToggle<CR>
" }}}

" {{{2 diagnostics
nnoremap <silent> <leader>ld :lua vim.diagnostic.open_float()<CR>
nnoremap <silent> <leader>ne :lua vim.diagnostic.goto_next()<CR>
nnoremap <silent> <F2> :lua vim.diagnostic.goto_next()<CR>
nnoremap <silent> <leader>pe :lua vim.diagnostic.goto_prev()<CR>
" }}}

nnoremap <silent>  :w<CR>
inoremap <silent>  :w<CR>

" nnoremap <silent> <S-Insert> "*p
" inoremap <silent> <S-Insert> <Esc>"*pa
vnoremap <silent> <C-Insert> "*y

nnoremap <silent> <leader>o :SymbolsOutline<CR>

" }}}

" vim:foldmethod=marker:foldlevel=0:
