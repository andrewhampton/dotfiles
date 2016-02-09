local weather = require "weather"
local menubar = {}

local weatherBar = hs.menubar.new()

local apiKey = nil

function menubar.init()
   if file_exists("forecast_io_api_key.lua") then
      apiKey = require "forecast_io_api_key"
      updateWeatherBar()
      hs.timer.doEvery(15*60, updateWeatherBar)
   else
      print('No forecast.io api key found')
   end
end

function updateWeatherBar()
   local w = weather.weather(apiKey)
   weatherBar:setTitle(w.icon .. w.temp .. '/' .. w.max .. '/' .. w.min)
   weatherBar:setMenu(w.toolbarDetails)
end

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then
      io.close(f)
      return true
   else
      return false
   end
end

return menubar
