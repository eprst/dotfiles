local execute = vim.api.nvim_command
local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '.. install_path)
end

vim.cmd [[packadd packer.nvim]]

require('packer').startup(function()
	use 'wbthomason/packer.nvim'
	use 'ethanholz/nvim-lastplace'
	use 'overcache/NeoSolarized'
	use 'ixru/nvim-markdown'
	use {'akinsho/bufferline.nvim', tag = "*", requires = 'kyazdani42/nvim-web-devicons'}
	use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
	use { 'nvim-treesitter/nvim-treesitter-textobjects' }
	use {
		'nvim-lualine/lualine.nvim',
		after = "NeoSolarized",
		requires = { 'kyazdani42/nvim-web-devicons', opt = true },
		config = function()
			require("lualine").setup {
				options = {
					theme = "solarized"
				}
			}
		end
	}

	use {
		"hrsh7th/nvim-cmp",
		requires = {
			'hrsh7th/vim-vsnip',
			"hrsh7th/cmp-buffer", "hrsh7th/cmp-nvim-lsp",
			'quangnguyen30192/cmp-nvim-ultisnips', 'hrsh7th/cmp-nvim-lua',
			'octaltree/cmp-look', 'hrsh7th/cmp-path', 'hrsh7th/cmp-calc',
			'f3fora/cmp-spell', 'hrsh7th/cmp-emoji'
		}
	}
	use {
	'kyazdani42/nvim-tree.lua',
		requires = { 'kyazdani42/nvim-web-devicons', opt = true }
	}
--	use {
--		'tzachar/cmp-tabnine',
--		run = './install.sh',
--		requires = 'hrsh7th/nvim-cmp'
--	}
	use {
		'nvim-telescope/telescope.nvim',
		requires = { {'nvim-lua/plenary.nvim'} }
	}
	use {
		'nvim-telescope/telescope-project.nvim',
		config = function()
			require"telescope".load_extension("project")
		end
	}

	use({
		"jose-elias-alvarez/null-ls.nvim",
		config = function()
			require("null-ls").setup()
		end,
		requires = { "nvim-lua/plenary.nvim" },
	})

	use {
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("trouble").setup {
				vim.keymap.set("n", "<leader>xx", "<Cmd>TroubleToggle<CR>", { silent = true, noremap = true })
			}
		end
	}

	if vim.fn.executable('gopls') == 1 then
		use({
			"WhoIsSethDaniel/goldsmith.nvim",
			-- run = ":GoInstallBinaries",
			requires = { "antoinemadec/FixCursorHold.nvim" },
		})
	end

	use { 'terrortylor/nvim-comment' }
	use { 'Yggdroot/indentLine' }
	use { 'tpope/vim-fugitive' }
	use { 'junegunn/gv.vim' }
	use { 'lewis6991/gitsigns.nvim' }
	use { 'windwp/nvim-autopairs' }
	use { 'rcarriga/nvim-notify' }
	use({ "neovim/nvim-lspconfig", requires = { "williamboman/nvim-lsp-installer" } })

	if packer_bootstrap then
		vim.notify("Installing plugins...")
		require("packer").sync()
	end
end)

require('nvim_comment').setup{}
require('bufferline').setup{}
require('nvim-lastplace').setup{}
require('nvim-tree').setup{}
require('nvim-autopairs').setup{}
require('nvim-treesitter').setup{}

require('nvim-lsp-installer').on_server_ready(function(server)
	local opts = {}
	server:setup(opts)
end)

require("null-ls").setup({
	sources = {
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.diagnostics.eslint,
		-- require("null-ls").builtins.completion.spell,
	},
})

if vim.fn.executable('gopls') == 1 then
	require("goldsmith").config({
		null = { run_setup = false, revive = false, gofumpt = false, golines = false },
		mappings = { format = {} },
	})
end

require('gitsigns').setup{
	on_attach = function(bufnr)
		local gs = package.loaded.gitsigns

		local function map(mode, l, r, opts)
			opts = opts or {}
			opts.buffer = bufnr
			vim.keymap.set(mode, l, r, opts)
		end

		-- Navigation
		map('n', ']c', function()
			if vim.wo.diff then return ']c' end
			vim.schedule(function() gs.next_hunk() end)
			return '<Ignore>'
		end, {expr=true})

		map('n', '[c', function()
			if vim.wo.diff then return '[c' end
			vim.schedule(function() gs.prev_hunk() end)
			return '<Ignore>'
		end, {expr=true})

		-- Actions
		map({'n', 'v'}, '<leader>hs', ':Gitsigns stage_hunk<CR>')
		map({'n', 'v'}, '<leader>hr', ':Gitsigns reset_hunk<CR>')
		map('n', '<leader>hS', gs.stage_buffer)
		map('n', '<leader>hu', gs.undo_stage_hunk)
		map('n', '<leader>hR', gs.reset_buffer)
		map('n', '<leader>hp', gs.preview_hunk)
		map('n', '<leader>hb', function() gs.blame_line{full=true} end)
		map('n', '<leader>tb', gs.toggle_current_line_blame)
		map('n', '<leader>hd', gs.diffthis)
		map('n', '<leader>hD', function() gs.diffthis('~') end)
		map('n', '<leader>td', gs.toggle_deleted)

		-- Text object
		map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
	end
}

local action_layout = require("telescope.actions.layout")
require('telescope').setup {
	defaults = {
		path_display = { "smart" },


		mappings = {
			n = {
				["<C-p>"] = action_layout.toggle_preview
			},
			i = {
				["<C-p>"] = action_layout.toggle_preview
			},
		},

	},
	extensions = {
		project = {
			base_dirs = {
				'~/observe'
			}
		}
	}
}

local cmp = require'cmp'

cmp.setup({
	formatting = {
		format = function(entry, vim_item)
			-- fancy icons and a name of kind
			-- vim_item.kind = require("lspkind").presets.default[vim_item.kind] ..
			--                    " " .. vim_item.kind

			-- set a name for each source
			vim_item.menu = ({
				buffer = "[Buffer]",
				nvim_lsp = "[LSP]",
				ultisnips = "[UltiSnips]",
				nvim_lua = "[Lua]",
				-- cmp_tabnine = "[TabNine]",
				look = "[Look]",
				path = "[Path]",
				spell = "[Spell]",
				calc = "[Calc]",
				emoji = "[Emoji]"
			})[entry.source.name]
			return vim_item
		end
	},

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
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-e>'] = cmp.mapping.abort(),
		['<Cr>'] = cmp.mapping.confirm({ select = false }),
		['<Tab>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
	}),
	sources = cmp.config.sources({
		{ name = 'nvim_lsp' },
		{ name = 'vsnip' }, -- For vsnip users.
		-- { name = 'luasnip' }, -- For luasnip users.
		-- { name = 'ultisnips' }, -- For ultisnips users.
		-- { name = 'snippy' }, -- For snippy users.
		{ name = 'nvim_lua' },
		-- { name = 'look' },
		{ name = 'path' },
		{ name = 'calc' },
		-- { name = 'spell' }
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

local map = vim.api.nvim_set_keymap
map('n', '<leader>t', [[:NvimTreeToggle<CR>]], {})
map('n', '<leader>fs', [[:Telescope current_buffer_fuzzy_find<CR>]], {})
map('n', '<leader>fb', [[:Telescope buffers<CR>]], {})
map('n', '<leader>fo', [[:Telescope old_files<CR>]], {})
map('n', '<leader>fr', [[:Telescope live_grep<CR>]], {})
map('n', '<leader>fg', [[:Telescope git_files<CR>]], {})
map('n', '<leader>fp', [[:Telescope project<CR>]], {})
map('n', '<leader>ff', [[:Telescope find_files]], {})
map('n', '<leader>fm', [[:Telescope marks<CR>]], {})
map('n', '<leader>lm', [[:Telescope lsp_references<CR>]], {})
map('n', '<leader>lo', [[:Telescope lsp_document_symbols<CR>]], {})

-- vim:noexpandtab
