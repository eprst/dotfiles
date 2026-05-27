-- simple plugins without any heavy configs
return {
  { 'ethanholz/nvim-lastplace', config = true },
  { 'tpope/vim-fugitive' }, -- :Git
  { 'tpope/vim-rhubarb' }, -- addition to vim-fugitive
  {
    'akinsho/bufferline.nvim',
    version = "*",
    dependencies = 'nvim-tree/nvim-web-devicons',
    opts = {
      options = {
        indicator = { style = "icon", icon = "🔹" },
        diagnostics = "nvim_lsp",
        --diagnostics_indicator = function(count, level, diagnostics_dict, context)
        --  return "("..count..")"
        --end,
        diagnostics_indicator = function(_, _, diag)
          local warn_icon = vim.diagnostic.config().signs.text[vim.diagnostic.severity.WARN]
          local err_icon = vim.diagnostic.config().signs.text[vim.diagnostic.severity.ERROR]
          local ret = (diag.error and err_icon .. diag.error .. " " or "")
          .. (diag.warning and warn_icon .. diag.warning or "")
          return vim.trim(ret)
        end,
      },
    },
  },
  -- neoconf.nvim is loaded as a dependency in lsp.lua
  { 'L3MON4D3/LuaSnip', version = 'v2.*', build = "make install_jsregexp" },
  { 'echasnovski/mini.pairs', version = false, config = true },
  { 'echasnovski/mini.comment', version = false, config = true },
  { 'folke/which-key.nvim', opts = {} },
  -- { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} }, -- indendation blank lines
  { 'stevearc/dressing.nvim', opts = {} },
  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    branch = 'master',
    dependencies = { { 'nvim-treesitter/nvim-treesitter-textobjects', branch = 'master' } },
    build = ':TSUpdate',
    main = 'nvim-treesitter.configs',
    opts = {
      ensure_installed = {
        'bash', 'c', 'go', 'gomod', 'gosum', 'gowork',
        'javascript', 'json', 'lua', 'markdown', 'markdown_inline',
        'python', 'query', 'rust', 'toml', 'tsx', 'typescript',
        'vim', 'vimdoc', 'yaml',
      },
      auto_install = true,
      highlight = { enable = true },
      indent = { enable = true },
    },
  },
  {
    'nvim-treesitter/nvim-treesitter-context',
    dependencies = 'nvim-treesitter/nvim-treesitter',
    event = 'BufReadPost',
    opts = {
      max_lines = 5,
      trim_scope = 'outer',
      mode = 'topline',
    },
    keys = {
      { '<leader>cc', function() require('treesitter-context').go_to_context(vim.v.count1) end, desc = 'Jump to context' },
    },
  },

  -- Detect tabstop and shiftwidth automatically
  { 'tpope/vim-sleuth' },

  {
    'morhetz/gruvbox',
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    enabled = false,
    config = function()
      vim.cmd [[ colorscheme gruvbox ]]
    end
  },
  {
    'shaunsingh/solarized.nvim',
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    enabled = false,
    config = function()
      vim.cmd [[ colorscheme solarized ]]
    end
  },
  {
    "Tsuzat/NeoSolarized.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      local applying = false
      local function subtle_listchars()
        local style = vim.o.background == "dark" and "dark" or "light"
        local subtle = style == "dark" and "#073642" or "#eee8d5"
        vim.api.nvim_set_hl(0, "Whitespace", { fg = subtle })
        vim.api.nvim_set_hl(0, "NonText", { fg = subtle })
      end
      local function apply_neosolarized()
        if applying then return end
        applying = true
        local style = vim.o.background == "dark" and "dark" or "light"
        require('NeoSolarized').setup({ style = style, transparent = false })
        vim.cmd('colorscheme NeoSolarized')
        subtle_listchars()
        applying = false
      end
      apply_neosolarized()
      -- Re-apply override whenever the theme repaints (some plugins trigger
      -- a ColorScheme event after our initial apply, wiping the override).
      vim.api.nvim_create_autocmd("ColorScheme", {
        pattern = "NeoSolarized",
        callback = function()
          if applying then return end
          subtle_listchars()
        end,
      })
      -- Re-apply when background changes (e.g. terminal reports dark/light via OSC 11)
      vim.api.nvim_create_autocmd("OptionSet", {
        pattern = "background",
        callback = apply_neosolarized,
      })
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    opts = {
      options = {
        theme = 'solarized',
        -- theme = 'gruvbox',
        disabled_filetypes = {
          statusline = { 'NvimTree', 'aerial' },
        },
      },
      sections = {
        lualine_c = { { 'filename', path = 1 } }, -- 1 = relative to cwd
      },
    },
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
    keys = {
      { "<leader>xx", "<Cmd>Trouble diagnostics toggle<CR>", desc = "Toggle diagnostics" },
    },
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
        merge_duplicates = true,
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
    'MeanderingProgrammer/render-markdown.nvim',
    dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' },
    ft = { 'markdown', 'codecompanion' },
    opts = {},
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
