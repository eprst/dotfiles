return {
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    init = function()
      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1
      -- When `nvim <dir>` is used, no plugin is yet loaded to handle the
      -- directory buffer (netrw is off, nvim-tree is lazy). Detect a directory
      -- argument on VimEnter, cd into it, and open the tree.
      vim.api.nvim_create_autocmd('VimEnter', {
        callback = function(data)
          if vim.fn.isdirectory(data.file) ~= 1 then return end
          vim.cmd.cd(data.file)
          require('nvim-tree.api').tree.open()
        end,
      })
    end,
    opts = {
      view = { width = 35 },
      renderer = { group_empty = true },
      filters = { dotfiles = false },
      diagnostics = { enable = true, show_on_dirs = true },
      update_focused_file = { enable = true },
    },
    keys = {
      { "<leader>e", "<Cmd>NvimTreeToggle<CR>", desc = "Toggle nvim-tree" },
    },
  },
}
