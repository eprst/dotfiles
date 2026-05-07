-- {{{1 lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup("plugins")
-- }}}

-- {{{1 options
vim.o.backspace="indent,eol,start"
vim.o.expandtab=true
vim.o.shiftwidth=2
vim.o.tabstop=2
vim.o.fo="tcrqnlv1"
vim.o.tw=80
vim.o.wrap=true
vim.o.linebreak=true
vim.o.relativenumber=true
vim.o.number=true
vim.o.termguicolors=true
vim.o.ignorecase=true
vim.o.smartcase=true
vim.o.undofile=true
vim.o.incsearch=true
vim.o.showmatch=true
vim.o.hlsearch=true
vim.o.dir="~/tmp/nvim//,~/tmp//,/var/tmp//,/tmp//"
vim.o.bdir="~/tmp/nvim//,~/tmp//,/var/tmp//,/tmp//"
vim.o.undodir="~/tmp/nvim,~/tmp,/var/tmp,/tmp,."
vim.o.completeopt='menuone,noselect'
vim.o.winborder='rounded'
vim.o.mouse='a'
vim.o.list=true
vim.o.showbreak='↪ '
vim.o.listchars='tab:▸┈,eol:¬,nbsp:␣,trail:•,extends:⟩,precedes:⟨'
vim.o.colorcolumn="120"
vim.o.spell=true
-- use system clipboard
vim.opt.clipboard:append { 'unnamed', 'unnamedplus' }
-- Over SSH, route yanks through OSC 52 so the system clipboard on the local
-- machine receives them. Locally (GUI or terminal) the default provider
-- (pbcopy/pbpaste on macOS) handles things correctly, so leave it alone.
-- Paste reads from the unnamed register because most terminals don't reply to
-- OSC 52 read requests, which would otherwise hang.
if vim.env.SSH_TTY or vim.env.SSH_CONNECTION then
  local function paste_from_unnamed()
    return {
      vim.fn.split(vim.fn.getreg(''), '\n'),
      vim.fn.getregtype(''),
    }
  end
  vim.g.clipboard = {
    name = 'OSC 52',
    copy = {
      ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
      ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
    },
    paste = {
      ['+'] = paste_from_unnamed,
      ['*'] = paste_from_unnamed,
    },
  }
end
vim.o.background = 'light' -- default to light; overridden by terminal via OSC 11 if supported
-- }}}

-- {{{1 GUI options
if vim.fn.has('gui_running')==1 or vim.g.neovide or vim.g.fvim_loaded then
  if vim.fn.has('linux')==1 then
    vim.g.gui_font_default_size = 13
    vim.g.gui_font_face = "CommitMono Nerd Font"
  elseif vim.fn.has('macunix')==1 then
    -- vim.g.gui_font_face = "Hack Nerd Font Mono"
    vim.g.gui_font_face = "FiraCode Nerd Font Mono"
    vim.g.gui_font_default_size = 14
  end

  vim.g.gui_font_size = vim.g.gui_font_default_size
  function ResizeFont(delta)
    vim.g.gui_font_size = vim.g.gui_font_size + delta
    vim.opt.guifont = string.format("%s:h%s",vim.g.gui_font_face, vim.g.gui_font_size)
  end
  ResizeFont(0)
  vim.keymap.set({'n', 'i'}, "<C-=>", function() ResizeFont(1)  end, { noremap = true, silent = true })
  vim.keymap.set({'n', 'i'}, "<C-->", function() ResizeFont(-1) end, { noremap = true, silent = true })

  if vim.g.neovide then
    vim.g.neovide_remember_window_size = true
    vim.g.neovide_cursor_trail_size = 0.3
    vim.g.neovide_cursor_vfx_mode = "pixiedust"
  end

  if vim.g.fvim_loaded then
    vim.cmd [[FVimCursorSmoothMove v:true]]
    vim.cmd [[FVimCursorSmoothBlink v:true]]
    vim.cmd [[FVimCustomTitleBar v:true ]]
    vim.cmd [[FVimFontAutoSnap v:true]]
    vim.cmd [[FVimUIPopupMenu v:true]]
    vim.cmd [[FVimUIWildMenu v:true]]
  end
end
-- }}}

-- {{{1 key bindings
vim.keymap.set('n', '<space>', 'za', {silent=true})
vim.keymap.set('n', "<C-/>", 'gccj', {remap=true})
vim.keymap.set('n', "", 'gccj', {remap=true}) -- linux
vim.keymap.set('v', "<C-/>", 'gc', {remap=true})
vim.keymap.set('v', "", 'gc', {remap=true}) -- linux
vim.keymap.set('v', '<C-Insert>', '"+y', {silent=true})
vim.keymap.set('n', '<S-Insert>', '<C-R>+', {})
vim.keymap.set('i', '<S-Insert>', '<C-R>+', {})
vim.keymap.set('n', '<C-S>', '<CMD>w<CR>', {silent=true})
-- MacOS Cmd key bindings (D- is command-key, works in GUI only)
vim.keymap.set('n', '<D-s>', '<CMD>w<CR>', { silent = true }) -- Save
vim.keymap.set('v', '<D-c>', '"+y') -- Copy
vim.keymap.set('n', '<D-v>', '"+P') -- Paste normal mode
vim.keymap.set('v', '<D-v>', '"+P') -- Paste visual mode
vim.keymap.set('c', '<D-v>', '<C-R>+') -- Paste command mode
vim.keymap.set('i', '<D-v>', '<C-R>+') -- Paste insert mode
vim.keymap.set('t', '<D-v>', '<C-R>+', { noremap = true, silent = true }) -- Paste terminal mode

vim.keymap.set('n', '<leader>bl', '<CMD>set background=light<CR>', { silent = true, desc = 'Light theme' })
vim.keymap.set('n', '<leader>bd', '<CMD>set background=dark<CR>', { silent = true, desc = 'Dark theme' })
vim.keymap.set('n', '<leader>st', ':split | lcd %:p:h | terminal<CR>', { silent = true, desc = 'Terminal at file dir' })
-- }}}

-- {{{1 spell highlights
local function set_spell_hl()
  vim.api.nvim_set_hl(0, 'SpellBad',   { undercurl = true, sp = 'Red' })
  vim.api.nvim_set_hl(0, 'SpellCap',   { undercurl = true, sp = 'Red' })
  vim.api.nvim_set_hl(0, 'SpellRare',  { undercurl = true, sp = 'Red' })
  vim.api.nvim_set_hl(0, 'SpellLocal', { undercurl = true, sp = 'Red' })
end
set_spell_hl()
vim.api.nvim_create_autocmd('ColorScheme', { callback = set_spell_hl })
-- }}}

-- vim:foldmethod=marker:foldlevel=0:
