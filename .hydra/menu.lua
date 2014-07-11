
menu.show(function()
    return {
      {title = "Reload Config", fn = hydra.reload},
      {title = "Logs", fn = logger.show},
      {title = "-"},
      {title = "About Hydra", fn = hydra.showabout},
      {title = "Quit", fn = os.exit},
    }
end)
