
function checkforupdates()
  hydra.updates.check(function(hasone)
      if hasone then
        notify.show("Hydra update available", "Go download it!", "Click here to see the release notes.", "hasupdate")
      end
  end)
end
notify.register("hasupdate", function() os.execute("open " .. hydra.updates.changelogurl) end)

checkforupdates()

timer.new(timer.days(1), checkforupdates):start()
