-- simple plugins without any heavy configs
return {
  { 'ethanholz/nvim-lastplace', config = true },
  { 'tpope/vim-fugitive' }, -- :Git
  { 'tpope/vim-rhubarb' }, -- addition to vim-fugitive
  { 'akinsho/bufferline.nvim', version = "*", dependencies = 'nvim-tree/nvim-web-devicons', config = true },
  { 'folke/neoconf.nvim', config = true }, -- :Neoconf, do I need it?
  { 'L3MON4D3/LuaSnip', version = 'v2.*', build = "make install_jsregexp" },
  { 'echasnovski/mini.pairs', version = false, config = true },
  { 'echasnovski/mini.comment', version = false, config = true },
  { 'folke/which-key.nvim', opts = {} },
  -- { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} }, -- indendation blank lines
  { 'stevearc/dressing.nvim', opts = {} },
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
    'shaunsingh/solarized.nvim',
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    enabled = false,
  },
  {
    'nvim-lualine/lualine.nvim',
    opts = {
      options = {
        -- theme = 'solarized',
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
      -- local notify = vim.schedule_wrap(require("notify")) -- https://github.com/rcarriga/nvim-notify/issues/205#issuecomment-1755890719
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
  },
  {
    "allaman/emoji.nvim",
    -- version = "1.0.0", -- optionally pin to a tag
    -- ft = "markdown", -- adjust to your needs
    dependencies = {
      -- optional for nvim-cmp integration
      "hrsh7th/nvim-cmp",
      -- optional for telescope integration
      "nvim-telescope/telescope.nvim",
    },
    opts = {
      -- default is false
      enable_cmp_integration = true,
      -- optional if your plugin installation directory
      -- is not vim.fn.stdpath("data") .. "/lazy/
      -- plugin_path = vim.fn.expand("$HOME/plugins/"),
    },
    config = function(_, opts)
      require("emoji").setup(opts)
      -- optional for telescope integration
      require("telescope").load_extension("emoji")
    end,
  }

}

-- vim: ts=2 sts=2 sw=2 et
