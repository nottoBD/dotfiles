local M = {}

M.nvimtree = {
  n = {
    -- Toggle nvim-tree on the right side
    ["<C-n>"] = { "<cmd> NvimTreeToggle <CR>", "Toggle nvimtree" },
  }
}

M.terminal = {
  n = {
    -- Toggle terminal
    ["<A-i>"] = { "<cmd> ToggleTerm direction=horizontal <CR>", "Toggle horizontal terminal" },
  },

  t = {
    -- Exit terminal mode
    ["<A-i>"] = { "<C-\\><C-n><cmd> ToggleTerm <CR>", "Toggle terminal" },
  }
}

return M
