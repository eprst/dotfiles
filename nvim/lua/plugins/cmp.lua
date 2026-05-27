return {
  {
    'hrsh7th/nvim-cmp',
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
      "octaltree/cmp-look",
      "hrsh7th/cmp-calc",
      "f3fora/cmp-spell",
      "hrsh7th/cmp-emoji",
      "hrsh7th/cmp-cmdline",
    },
    opts = function()
      local cmp = require("cmp")
      return {
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
            -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          end,
        },
        window = {
          completion = {
            border = "rounded",
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None",
          },
          documentation = {
            border = "rounded",
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None",
          },
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
          -- { name = 'vsnip' }, -- For vsnip users.
          { name = 'luasnip' }, -- For luasnip users.
          -- { name = 'ultisnips' }, -- For ultisnips users.
          -- { name = 'snippy' }, -- For snippy users.
          { name = 'nvim_lua' },
          -- { name = 'look' },
          { name = 'path', option = { get_cwd = function() return vim.fn.getcwd() end } },
          { name = 'calc' },
          { name = 'emoji' }
          -- { name = 'spell' }
        }, {
          { name = 'buffer' },
        })

      }
    end,
    config = function(_, opts)
      local cmp = require("cmp")
      cmp.setup(opts)
      -- Disable cmp inside telescope prompts so it doesn't swallow <Up>/<Down>.
      -- Has to happen on FileType (before InsertEnter) so cmp never installs
      -- its buffer-local mappings that would overwrite telescope's.
      vim.api.nvim_create_autocmd('FileType', {
        pattern = 'TelescopePrompt',
        callback = function()
          cmp.setup.buffer({ enabled = false })
        end,
      })
      -- cmp-path only previews files; extend resolve so directory completions
      -- show a listing of their contents in the documentation popup.
      local cmp_path = require('cmp_path')
      local original_resolve = cmp_path.resolve
      cmp_path.resolve = function(self, completion_item, callback)
        if completion_item.data and completion_item.data.type == 'directory' then
          local entries = vim.fn.readdir(completion_item.data.path)
          if entries and #entries > 0 then
            table.sort(entries)
            local lines = {}
            for i, name in ipairs(entries) do
              if i > 30 then
                table.insert(lines, ('… %d more'):format(#entries - 30))
                break
              end
              local full = completion_item.data.path .. '/' .. name
              if vim.fn.isdirectory(full) == 1 then name = name .. '/' end
              table.insert(lines, name)
            end
            completion_item.documentation = {
              kind = 'markdown',
              value = '```\n' .. table.concat(lines, '\n') .. '\n```',
            }
          end
          return callback(completion_item)
        end
        return original_resolve(self, completion_item, callback)
      end
      -- Set configuration for specific filetype.
      cmp.setup.filetype('gitcommit', {
        sources = cmp.config.sources({
          { name = 'cmp_git' },
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
        }, {
          { name = 'nvim-lua'}
        })
      })
    end
  },
}
