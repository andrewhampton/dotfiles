local hyperterm = {}

local logger = hs.logger.new("ht", "debug")

function hyperterm.init()
   -- hyperterm.terminalModeHandler = hs.eventtap.new({hs.eventtap.event.types.keyDown}, terminalMode)
   hyperterm.normalModeHandler = hs.eventtap.new({hs.eventtap.event.types.keyDown}, normalMode)
   hyperterm.normalModeHandler:start()
   hyperterm.watcher = hs.application.watcher.new(hypertermActivationHandler)
   hyperterm.watcher:start()
   -- log("key modifier rewriting started")


   tap = hs.eventtap.new({
     hs.eventtap.event.types.nullEvent,
     hs.eventtap.event.types.keyDown,
     hs.eventtap.event.types.keyUp,
     hs.eventtap.event.types.flagsChanged,
   }, massEventLogger)
   tap:start()
end

function massEventLogger(event)
  log("mass event")
  log(hs.inspect.inspect(event))
end

function hypertermActivationHandler(appName, eventType, app)
   -- log(appName)
   -- log(eventType)
   if appName == "HyperTerm" or appName == "Emacs" or appName == "Terminal" or appName == "kitty" then
      if eventType == hs.application.watcher.activated then
         -- log("terminal mode activated")
         -- hyperterm.terminalModeHandler:start()
         hyperterm.normalModeHandler:stop()
      elseif eventType == hs.application.watcher.deactivated then
         -- log("normal mode activated")
         -- hyperterm.terminalModeHandler:stop()
         hyperterm.normalModeHandler:start()
      end
   end
end

-- function terminalMode(event)
--    log('terminal key press')
--    local terminalMap = {
--       shift = { shift = true },
--       fn    = { fn = true },
--       cmd   = { cmd = true },
--       ctrl  = { ctrl = true },
--       alt   = { alt = true },
--       includeMetaEvent = true
--    }

--    return translate(event, terminalMap)
-- end

function normalMode(event)
   log('normal key press')
   local normalMap = {
      shift = { shift = true },
      fn    = { fn = true },
      cmd   = { alt = true },
      ctrl  = { cmd = true },
      alt   = { ctrl = true },
      includeMetaEvent = false
   }

   return translate(event, normalMap)
end

function translate(event, map)
   local flags = event:getFlags()
   local newFlags = {}

   -- log('tranlating')
   -- log(hs.inspect.inspect(flags))
   if flags.ctrl and flags.shift and flags.cmd then
      return false, {}
   end

   merge(flags.shift, newFlags, map.shift)
   -- log('shift')
   -- log(hs.inspect.inspect(newFlags))

   merge(flags.fn, newFlags, map.fn)
   -- log('fn')
   -- log(hs.inspect.inspect(newFlags))

   merge(flags.cmd, newFlags, map.cmd)
   -- log('cmd')
   -- log(hs.inspect.inspect(newFlags))

   merge(flags.ctrl, newFlags, map.ctrl)
   -- log('ctrl')
   -- log(hs.inspect.inspect(newFlags))

   merge(flags.alt, newFlags, map.alt)
   -- log('alt')
   -- log(hs.inspect.inspect(newFlags))

   event:setFlags(newFlags)

   log('from: ')
   log(hs.inspect.inspect(flags))
   log('to: ')
   log(hs.inspect.inspect(newFlags))
   log('\n\n\n------------------------------------------')
   return true, {event}
end

function merge(enabled, target, source)
   if not enabled then
      return
   end

   for k, v in pairs(source) do target[k] = v end
end

function log(msg)
   --logger:d(msg)
end

return hyperterm
