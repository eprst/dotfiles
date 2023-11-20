return {
  "nvim-tree/nvim-tree.lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  enabled = false, -- this thing is pretty slow.. consider https://gitlab.com/taken-personal/neovim-config/-/blob/main/lua/taken/plugins/neotree.lua
  init = function()
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1
  end,
  config = function()
    local nvimtree = require("nvim-tree")

    nvimtree.setup({
      sync_root_with_cwd = true,
      view = {
        width = 30,
      },
      git = {
        ignore = false,
      },
      filters = {
        dotfiles = false,
        custom = {
          ".git",
        },
        exclude = {
          ".gitignore",
        },
      },
    })

    vim.keymap.set("n", "<leader>t", ":NvimTreeToggle <CR>")
    -- vim.keymap.set("n", "<leader>e", ":NvimTreeFocus <CR>")
    -- vim.keymap.set("n", "<leader>r", ":NvimTreeRefresh <CR>")
    vim.keymap.set("n", "<leader>tr", function()
      require("nvim-tree.api").tree.change_root(vim.fn.getcwd())
    end, { desc = "Change root dir to current dir" })

    -- nvim-tree open_on_setup
    local function open_nvim_tree(data)

      -- buffer is a directory
      local directory = vim.fn.isdirectory(data.file) == 1

      -- if not no_name and not directory then
      if not directory then
        return
      end

      -- change to the directory
      if directory then
        vim.cmd.cd(data.file)
      end

      -- open the tree
      require("nvim-tree.api").tree.open()
    end
    vim.api.nvim_create_autocmd({ "VimEnter" }, { callback = open_nvim_tree })

  end,
}

