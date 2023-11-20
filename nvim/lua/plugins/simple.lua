-- simple plugins without any heavy configs
return {
  { 'ethanholz/nvim-lastplace' },
  { 'tpope/vim-fugitive' },
  { 'tpope/vim-rhubarb' },
  { 'akinsho/bufferline.nvim', version = "*", dependencies = 'nvim-tree/nvim-web-devicons', config = true },
  { 'folke/neoconf.nvim', config = true },
  { 'L3MON4D3/LuaSnip', version = 'v2.*' },
  { 'echasnovski/mini.pairs', version = false, config = true },
  { 'echasnovski/mini.comment', version = false, config = true },
  { 'simrat39/symbols-outline.nvim',  config = true },
  { 'folke/which-key.nvim', opts = {} },
  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    dependencies = 'nvim-treesitter/nvim-treesitter-textobjects',
    build = ':TSUpdate',
    -- TODO need to config it properly
  },

  -- Detect tabstop and shiftwidth automatically
  { 'tpope/vim-sleuth' },

  -- Themes
  -- {
  --   'Tsuzat/NeoSolarized.nvim',
  --   lazy = false, -- make sure we load this during startup if it is your main colorscheme
  --   priority = 1000, -- make sure to load this before all the other start plugins
  -- },
  {
    'morhetz/gruvbox',
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
  },
  {
    'nvim-lualine/lualine.nvim',
    opts = {
      options = {
        -- theme = 'NeoSolarized',
        theme = 'gruvbox',
      },
    },
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function ()
      vim.keymap.set("n", "<leader>xx", "<Cmd>TroubleToggle<CR>", { silent = true, noremap = true })
    end
  },
  {
    "rcarriga/nvim-notify",
    config = function()
      local notify = require("notify")

      notify.setup({
        fps = 60,
        stages = "slide",
        timeout = 2500,
      })

      vim.notify = notify
    end,
  },
  {
    "gbprod/yanky.nvim",
    opts = {
      highlight = {
        on_put = true,
        on_yank = true,
        timer = 500,
      },
      picker = {
        select = {
          action = nil,
        },
        telescope = {
          mappings = nil,
        },
        system_clipboard = {
          sync_with_ring = true,
        },
        preserve_cursor_position = {
          enabled = true,
        },
      }
    },
  }

}

-- vim: ts=2 sts=2 sw=2 et
