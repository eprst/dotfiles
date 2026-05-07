return {
  {
    "echasnovski/mini.files",
    version = false,
    opts = {
      windows = {
        preview = true,
        width_preview = 40,
      },
    },
    keys = {
      { "<leader>1", function()
        local mf = require("mini.files")
        if not mf.close() then
          local path = vim.api.nvim_buf_get_name(0)
          if path == "" or (vim.fn.filereadable(path) == 0 and vim.fn.isdirectory(path) == 0) then
            path = nil -- fall back to cwd for buffers without a real file (nvim-tree, terminals, etc.)
          end
          mf.open(path, true)
        end
      end, desc = "Toggle file explorer at current file" },
    },
  },
}
