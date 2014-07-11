dofile(package.searchpath('grid', package.path))
dofile(package.searchpath('menu', package.path))
dofile(package.searchpath('keybindings', package.path))

hydra.alert "Hydra, at your service."

pathwatcher.new(os.getenv("HOME") .. "/.hydra/", hydra.reload):start()
autolaunch.set(true)

updates.check()
