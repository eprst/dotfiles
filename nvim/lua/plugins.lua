vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
	use 'wbthomason/packer.nvim'
  use 'shaunsingh/solarized.nvim'
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
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
		'tzachar/cmp-tabnine',
		run = './install.sh',
		requires = 'hrsh7th/nvim-cmp'
	}
end)
