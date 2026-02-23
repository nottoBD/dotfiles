local plugins = {
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        -- Go-related tools
        "gopls",           -- Go LSP
        "golines",         -- Go formatter
        "goimports",       -- Go imports formatter
        "golangci-lint",   -- Go linter
        "delve",          -- Go debugger
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      require "plugins.configs.lspconfig"
      require "custom.configs.lspconfig"
    end,
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    ft = "go",
    opts = function()
      return require "custom.configs.null-ls"
    end,
  },
  {
    "nvim-tree/nvim-tree.lua",
    opts = {
      view = {
        side = "right",
        width = 30,
      },
    },
  },

  {
    "akinsho/toggleterm.nvim",
    cmd = "ToggleTerm",
    opts = {
      size = 20,
      direction = "horizontal",
      shade_terminals = true,
    },
  },
}
return plugins

