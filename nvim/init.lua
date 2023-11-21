-- {{{1 lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
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

-- {{{1 colorscheme
if not pcall(require, "NeoSolarized") then
  vim.cmd.colorscheme 'gruvbox'
else
  vim.cmd.colorscheme 'NeoSolarized'
end
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
vim.o.termguicolors=true
vim.o.ignore=true
vim.o.smartcase=true
vim.o.undofile=true
vim.o.incsearch=true
vim.o.showmatch=true
vim.o.hlsearch=true
vim.o.dir="~/tmp/nvim//,~/tmp//,/var/tmp//,/tmp//"
vim.o.bdir="~/tmp/nvim//,~/tmp//,/var/tmp//,/tmp//"
vim.o.undodir="~/tmp/nvim,~/tmp,/var/tmp,/tmp,."
vim.o.completeopt='menuone,noselect'
vim.o.mouse='a'
vim.o.list=true
vim.o.showbreak='↪ '
vim.o.listchars='tab:▸┈,eol:¬,nbsp:␣,trail:•,extends:⟩,precedes:⟨'
vim.o.colorcolumn=120
-- }}}

-- {{{1 GUI options
if vim.fn.has('gui_running')==1 or vim.g.neovide or vim.g.fvim_loaded then
  if vim.fn.has('linux')==1 then
    vim.g.gui_font_default_size = 13
    vim.g.gui_font_face = "FuraCode Nerd Font Mono"
  elseif vim.fn.has('macunix')==1 then
    vim.g.gui_font_face = "Hack Nerd Font Mono"
    vim.g.gui_font_default_size = 15
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
    vim.g.neovide_cursor_vfx_mode = "railgun"
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
vim.keymap.set('v', "<C-/>", 'gc', {remap=true})
vim.keymap.set('n', "<leader>bl", '<CMD>:set background=light<CR>', {remap=true, silent=true, desc='light theme'})
vim.keymap.set('n', "<leader>bd", '<CMD>:set background=dark<CR>', {remap=true, silent=true, desc='dark theme'})
vim.keymap.set('v', '<C-Insert>', '"*y', {silent=true})
vim.keymap.set('n', '<S-Insert>', '<C-R>+', {})
-- }}}

-- vim:foldmethod=marker:foldlevel=0:
