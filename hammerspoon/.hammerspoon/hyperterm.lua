local hyperterm = {}

local logger = hs.logger.new("ht", "debug")

function hyperterm.init()
   hyperterm.keyDownHandler = hs.eventtap.new({hs.eventtap.event.types.keyDown}, handleKeyDown)
   hyperterm.watcher = hs.application.watcher.new(hypertermActivationHandler)
   hyperterm.watcher:start()
   logger:d("starting")
end

function hypertermActivationHandler(appName, eventType, app)
   if appName == "HyperTerm" then
      if eventType == hs.application.watcher.activated then
         logger:d("activated")
         hyperterm.keyDownHandler:start()
      elseif eventType == hs.application.watcher.deactivated then
         logger:d("deactivated")
         hyperterm.keyDownHandler:stop()
      end
   end
end

function handleKeyDown(event)
   local flags = event:getFlags()
   local newFlags = {}
   local includeMetaEvent = false
   local returnEvents = {}

   if flags.ctrl and flags.shift and flags.cmd then
      return false, {}
   end

   newFlags.shift = flags.shift
   newFlags.fn = flags.fn

   if flags.cmd then
      newFlags.ctrl = true
   end

   if flags.ctrl then
      -- newFlags.alt = true
      includeMetaEvent = true
   end

   if flags.alt then
      newFlags.cmd = true
   end

   event:setFlags(newFlags)

   if includeMetaEvent then
      returnEvents = {hs.eventtap.event.newKeyEvent({}, "escape", true), hs.eventtap.event.newKeyEvent({}, "escape", false), event}
   else
      returnEvents = {event}
   end

   return true, returnEvents
end

return hyperterm
