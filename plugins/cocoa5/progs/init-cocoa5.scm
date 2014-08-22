(plugin-configure cocoa5
  (:require (url-exists-in-path? "cocoa5"))
  (:launch "cocoa5 --prompt '\x05\x02verbatim:'")
  (:session "CoCoa5"))
