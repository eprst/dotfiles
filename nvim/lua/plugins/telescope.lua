return {
  {
    'nvim-telescope/telescope.nvim',
    branch = 'master',
    dependencies = {
      'nvim-lua/plenary.nvim',
      "nvim-telescope/telescope-file-browser.nvim",
      "nvim-telescope/telescope-project.nvim",
      "tsakirist/telescope-lazy.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    config = function ()
      local telescope = require("telescope")
      telescope.load_extension("file_browser")
      telescope.load_extension("project")
      telescope.load_extension("lazy")
      telescope.load_extension("fzf")

      local actions = require("telescope.actions")
      local action_layout = require("telescope.actions.layout")
      telescope.setup {
        defaults = {
          path_display = { "smart" },
          cycle_layout_list = { 'horizontal', 'vertical' },

          mappings = {
            n = {
              ["<C-p>"] = action_layout.toggle_preview,
              ["<C-l>"] = action_layout.cycle_layout_next,
            },
            i = {
              ["<C-p>"] = action_layout.toggle_preview,
              ["<C-l>"] = action_layout.cycle_layout_next,
              ["<Down>"] = actions.move_selection_next,
              ["<Up>"] = actions.move_selection_previous,
            },
          },

        },
        extensions = {
          project = {
            base_dirs = {
              '~/observe'
            }
          },
          file_browser = {
            theme = "ivy",
            hijack_netrw = true,
            hidden = {
              file_browser = true,
              folder_browser = true,
            },
          },
          fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
          },
        }
      }


      local map = vim.api.nvim_set_keymap
      map('n', '<leader>pp', [[:Telescope resume<CR>]], {})
      map('n', '<leader>pP', ':lua require("telescope.builtin").buffers()<CR>', {})
      map('n', '<leader>fs', [[:Telescope current_buffer_fuzzy_find<CR>]], {})
      map('n', '<leader>fb', [[:Telescope buffers<CR>]], {})
      map('n', '<leader>fo', [[:Telescope oldfiles<CR>]], {})
      map('n', '<leader>fr', [[:Telescope live_grep<CR>]], {})
      map('n', '<leader>fR', ':lua require("telescope.builtin").live_grep({cwd=require("telescope.utils").buffer_dir()})<CR>', {desc = 'Live grep at the current buffer location'})
      map('n', '<leader>fg', [[:Telescope git_files<CR>]], {})
      map('n', '<leader>fc', [[:Telescope git_commits<CR>]], {})
      vim.keymap.set('n', '<leader>fd', function()
        require('telescope.builtin').git_status({
          attach_mappings = function(_, m)
            local action_state = require('telescope.actions.state')
            local tactions = require('telescope.actions')
            local function open_vdiff(prompt_bufnr)
              local entry = action_state.get_selected_entry()
              if not entry then return end
              tactions.close(prompt_bufnr)
              -- Close any pre-existing fugitive diff panes from a prior call
              -- so we don't pile up windows.
              for _, win in ipairs(vim.api.nvim_list_wins()) do
                local buf = vim.api.nvim_win_get_buf(win)
                if vim.api.nvim_buf_get_name(buf):match('^fugitive://') then
                  vim.api.nvim_win_close(win, true)
                end
              end
              vim.cmd('edit ' .. vim.fn.fnameescape(entry.value))
              vim.cmd('Gvdiffsplit')
            end
            m({ 'i', 'n' }, '<C-y>', open_vdiff)
            return true
          end,
        })
      end, { silent = true, desc = 'Git modified files (Ctrl-y for side-by-side diff)' })
      map('n', '<leader>fp', [[:Telescope project<CR>]], {})
      map('n', '<leader>ff', [[:Telescope find_files<CR>]], {})
      map('n', '<leader>fm', [[:Telescope marks<CR>]], {})
      map('n', '<leader>fn', [[:Telescope notify<CR>]], {})
      map('n', '<leader>fy', [[:Telescope yank_history<CR>]], {})
      map('n', '<leader>ft', [[:Telescope file_browser<CR>]], {})
      map('n', '<C-1>', [[:Telescope file_browser path=%:p:h display_stat=false select_buffer=true<CR>]], {desc = 'Files at the current buffer location'})
      map('n', '<leader>f1', [[:Telescope file_browser path=%:p:h display_stat=false select_buffer=true<CR>]], {desc = 'Files at the current buffer location'})
      map('n', '<leader>fz', [[:Telescope lazy<CR>]], {})

      map('n', '<leader>lr', [[:Telescope lsp_references<CR>]], {})
      map('n', '<leader>ls', [[:Telescope lsp_document_symbols<CR>]], {})
      map('n', '<leader>lw', [[:Telescope lsp_dynamic_workspace_symbols<CR>]], {})
      map('n', '<leader>lc', [[:Telescope lsp_incoming_calls<CR>]], {})
      map('n', '<leader>lo', [[:Telescope lsp_outgoing_calls<CR>]], {})
      map('n', '<leader>le', [[:Telescope diagnostics<CR>]], {})
      map('n', '<leader>li', [[:Telescope lsp_implementations<CR>]], {})
      map('n', '<leader>ld', [[:Telescope lsp_definitions<CR>]], {})
      map('n', '<leader>lt', [[:Telescope lsp_type_definitions<CR>]], {})
    end
  },
}
