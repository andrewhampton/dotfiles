local weather = {}

function weather.weather(apiKey)
   return getWeather(latlong(), apiKey)
end

function getWeather(latlong, apiKey)
   local url = 'https://api.forecast.io/forecast/' .. apiKey .. '/' .. latlong .. "?v=" .. math.floor(math.random() * 100)

   local code, body = hs.http.get(url, nil)
   local w = hs.json.decode(body)

   return {
      icon = getIcon(w.currently.icon),
      temp = math.floor(w.currently.temperature + 0.5),
      min = math.floor(w.daily.data[1].temperatureMin + 0.5),
      max = math.floor(w.daily.data[1].temperatureMax + 0.5),
      toolbarDetails = {
         {title = getIcon(w.minutely.icon) .. ' ' .. w.minutely.summary},
         {title = getIcon(w.hourly.icon) .. ' ' .. w.hourly.summary},
         {title = getIcon(w.daily.icon) .. ' ' .. w.daily.summary,
          menu = {
             {title = os.date('%a', w.daily.data[2].time) .. ':  \t' .. getIcon(w.daily.data[2].icon) .. ' ' .. w.daily.data[2].summary},
             {title = os.date('%a', w.daily.data[3].time) .. ':  \t' .. getIcon(w.daily.data[3].icon) .. ' ' .. w.daily.data[3].summary},
             {title = os.date('%a', w.daily.data[4].time) .. ':  \t' .. getIcon(w.daily.data[4].icon) .. ' ' .. w.daily.data[4].summary},
             {title = os.date('%a', w.daily.data[5].time) .. ':  \t' .. getIcon(w.daily.data[5].icon) .. ' ' .. w.daily.data[5].summary},
             {title = os.date('%a', w.daily.data[6].time) .. ':  \t' .. getIcon(w.daily.data[6].icon) .. ' ' .. w.daily.data[6].summary},
             {title = os.date('%a', w.daily.data[7].time) .. ':  \t' .. getIcon(w.daily.data[7].icon) .. ' ' .. w.daily.data[7].summary},
             {title = os.date('%a', w.daily.data[8].time) .. ':  \t' .. getIcon(w.daily.data[8].icon) .. ' ' .. w.daily.data[8].summary}
          }
         }
      }}
end

function latlong()
   hs.location.start()
   local l = hs.location.get()
   hs.location.stop()
   return l.latitude .. ',' .. l.longitude
end

function getIcon(iconCode)
   local icon = ''
   if iconCode == 'clear-day' then
      icon = '☀️'
   elseif iconCode == 'clear-night' then
      icon = '☀'
   elseif iconCode == 'rain' then
      icon = '🌧'
   elseif iconCode == 'snow' then
      icon = '🌨'
   elseif iconCode == 'sleet' then
      icon = '🌨'
   elseif iconCode == 'wind' then
      icon = '💨'
   elseif iconCode == 'fog' then
      icon = '(FOG)'
   elseif iconCode == 'cloudy' then
      icon = '☁'
   elseif iconCode == 'partly-cloudy-day' then
      icon = '⛅'
   elseif iconCode == 'partly-cloudy-night' then
      icon = '⛅'
   end

   return icon
end


return weather
