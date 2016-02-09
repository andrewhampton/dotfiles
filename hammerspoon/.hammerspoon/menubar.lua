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
      hs.alert.show('Weatherbar in use, but no forecast.io api key found!')
   end
end

function updateWeatherBar()
   local w = weather.weather(apiKey, function(status, w)
                                weatherBar:setTitle(weather.getIcon(w.currently.icon) ..
                                                       math.floor(w.currently.temperature + 0.5) ..
                                                       '/' ..
                                                       math.floor(w.daily.data[1].temperatureMin + 0.5) ..
                                                       '/' ..
                                                       math.floor(w.daily.data[1].temperatureMax + 0.5))
                                weatherBar:setMenu(genWeatherBarMenu(w))
   end)
end

function genWeatherBarMenu(w)
   return {
      {title = weather.getIcon(w.minutely.icon) .. ' ' .. w.minutely.summary},
      {title = weather.getIcon(w.hourly.icon) .. ' ' .. w.hourly.summary},
      {title = weather.getIcon(w.daily.icon) .. ' ' .. w.daily.summary,
       menu = {
          {title = os.date('%a', w.daily.data[2].time) .. ':  \t' .. weather.getIcon(w.daily.data[2].icon) .. ' ' .. w.daily.data[2].summary},
          {title = os.date('%a', w.daily.data[3].time) .. ':  \t' .. weather.getIcon(w.daily.data[3].icon) .. ' ' .. w.daily.data[3].summary},
          {title = os.date('%a', w.daily.data[4].time) .. ':  \t' .. weather.getIcon(w.daily.data[4].icon) .. ' ' .. w.daily.data[4].summary},
          {title = os.date('%a', w.daily.data[5].time) .. ':  \t' .. weather.getIcon(w.daily.data[5].icon) .. ' ' .. w.daily.data[5].summary},
          {title = os.date('%a', w.daily.data[6].time) .. ':  \t' .. weather.getIcon(w.daily.data[6].icon) .. ' ' .. w.daily.data[6].summary},
          {title = os.date('%a', w.daily.data[7].time) .. ':  \t' .. weather.getIcon(w.daily.data[7].icon) .. ' ' .. w.daily.data[7].summary},
          {title = os.date('%a', w.daily.data[8].time) .. ':  \t' .. weather.getIcon(w.daily.data[8].icon) .. ' ' .. w.daily.data[8].summary}
       }
      }
   }
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
