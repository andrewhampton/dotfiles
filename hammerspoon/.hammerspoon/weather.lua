local weather = {}
local lastUpdate = 0
local cacheTTL = 600
local cachedWeather = nil
local specificLatLong = nil

function weather.weather(apiKey, cb)
   local latlong = nil

   if specificLatLong ~= nil then
      latlong = specificLatLong
   else
      latlong = findLatLong()
   end

   getWeather(apiKey, latlong, cb)
end

function weather.getIcon(iconCode)
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

function weather.setCacheTTL(ttl)
   cacheTTL = ttl
end

function weather.setSpecificLatLong(latlong)
   specificLatLog = latlong
end

function getWeather(apiKey, latlong, cb)
   if os.time() < lastUpdate + cacheTTL then
      return cachedWeather
   end

   local url = 'https://api.forecast.io/forecast/' .. apiKey .. '/' .. latlong .. "?v=" .. math.floor(math.random() * 100)

   hs.http.asyncGet(url, nil, function(status, body)
                                     local w = hs.json.decode(body)

                                     cachedWeather = w
                                     lastUpdate = os.time()
                                     cb(status, w)
   end)
end

function getWeatherCBHandler(status, body, headers)

end

function findLatLong()
   hs.location.start()
   local l = hs.location.get()
   hs.location.stop()
   return l.latitude .. ',' .. l.longitude
end

return weather
