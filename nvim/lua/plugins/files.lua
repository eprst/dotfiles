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
          mf.open(vim.api.nvim_buf_get_name(0), true)
        end
      end, desc = "Toggle file explorer at current file" },
    },
  },
}
