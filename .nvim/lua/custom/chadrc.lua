---@type ChadrcConfig
local M = {}

M.ui = {
  theme = "onedark", -- You can keep your current theme

  -- Set Monocraft as the default font
  -- For NvChad, this will affect GUI clients like Neovide
  font = {
    family = "Monocraft Nerd Font",
    size = 11, -- Adjust size as needed
  },
}

return M
