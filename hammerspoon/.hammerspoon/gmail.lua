local gmail = {}

function gmail.mailCount(username, password, cb)
   local url = "https://mail.google.com/mail/feed/atom"

   hs.http.asyncGet(url, {Authorization = "Basic " .. hs.base64.encode(username .. ":" .. password)}, function(status, body, headers)
                       if status == 200 then
                          local mailCount = string.match(body, "<fullcount>([0-9]*)")
                          cb(tonumber(mailCount))
                       else
                          print("gmail call failed with status code: " .. status)
                          print("Please log an issue with the below information to https://github.com/andrewhampton/dotfiles")
                          print(hs.inspect.inspect(headers))
                          print(body)
                          cb(0)
                       end
   end)
end

return gmail
