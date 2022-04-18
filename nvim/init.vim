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

let g:dark = filereadable(expand('~/.dark')) && $TERMINAL_EMULATOR != 'JetBrains-JediTerm'
" }}}

" {{{1 color tweaks
" completion menu is ugly
hi Pmenu ctermbg=gray
hi PmenuSel ctermbg=227
if g:dark
  set background=dark
  if has("gui_running")
    colorscheme evening
  endif
else
  set background=light
  if $TERMINAL_EMULATOR == 'JetBrains-JediTerm'
    colorscheme blue
  else
    colorscheme solarized
  endif
endif
hi clear SignColumn
if has("gui_macvim")
  "set guifont=Source\ Code\ Pro\ for\ Powerline:h14
  set guifont=FiraCode-Regular:h14
elseif has("gui_vimr")
  set guifont=Hack\ Nerd\ Font\ Mono:h18
elseif has("gui_running")
  "set guifont=Source\ Code\ Pro\ for\ Powerline\ 12
  "set guifont=Ubuntu\ Mono\ for\ Powerline\ 12
  "set guifont=Fira\ Code\ weight=453\ 14
  set guifont=Fira\ Code\ weight=453\ 14
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
set listchars=tab:▸┈,nbsp:␣,trail:•,extends:⟩,precedes:⟨
if has("gui_running")
  set list
  set showbreak=↪\ 
  set listchars=tab:▸┈,eol:¬,nbsp:␣,trail:•,extends:⟩,precedes:⟨
  if g:dark
    hi SpecialKey guifg=red
    hi NonText guibg=grey20 guifg=grey30
  else
    hi NonText guifg=grey80
  endif
end
set colorcolumn=120
" }}}

" {{{1 plugins
lua << END
require('lualine').setup()

  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<Tab>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })
END
" }}}

" vim:foldmethod=marker:
