return {
  {
    'simrat39/symbols-outline.nvim',
    opts = {
      highlight_hovered_item = false, -- doesn't work anyways
      show_numbers = false,
      show_relative_numbers = false,
      symbols = {
        Array = { icon = " ", hl = "@constant" },
        Boolean = { icon = "󰨙 ", hl = "@boolean" },
        Class = { icon = " ", hl = "@type" },
        Component = { icon = " ", hl = "@function" },
        Constant = { icon = "", hl = "@constant" },
        Constructor = { icon = " ", hl = "@constructor" },
        Enum = { icon = " ", hl = "@type" },
        EnumMember = { icon = " ", hl = "@field" },
        Event = { icon = " ", hl = "@type" },
        Field = { icon = " ", hl = "@field" },
        File = { icon = " ", hl = "@text.uri" },
        Fragment = { icon = "...", hl = "@constant" },
        Function = { icon = "󰊕 ", hl = "@function" },
        Interface = { icon = " ", hl = "@type" },
        Key = { icon = " ", hl = "@type" },
        Method = { icon = "󰊕 ", hl = "@method" },
        Module = { icon = " ", hl = "@namespace" },
        Namespace = { icon = "N ", hl = "@namespace" },
        Null = { icon = " ", hl = "@type" },
        Number = { icon = "󰎠 ", hl = "@number" },
        Object = { icon = " ", hl = "@type" },
        Operator = { icon = " ", hl = "@operator" },
        Package = { icon = " ", hl = "@namespace" },
        Property = { icon = " ", hl = "@method" },
        String = { icon = " ", hl = "@string" },
        Struct = { icon = " ", hl = "@type" },
        TypeParameter = { icon = " ", hl = "@parameter" },
        Variable = { icon = "󰀫 ", hl = "@constant" },
      },
    },
    config = true,
    keys = { { "<leader>o", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" } },
  },
}
