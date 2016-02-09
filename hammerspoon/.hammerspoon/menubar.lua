local weather = require "weather"
local menubar = {}

local weatherBar = hs.menubar.new()

local apiKey = os.getenv('FORECAST_IO_API_KEY')

function menubar.init()
   updateWeatherBar()

   hs.timer.doEvery(15*60, updateWeatherBar)
end

function updateWeatherBar()
   local w = weather.weather(apiKey)
   weatherBar:setTitle(w.icon .. w.temp .. '/' .. w.max .. '/' .. w.min)
   weatherBar:setMenu(w.toolbarDetails)
end

return menubar
