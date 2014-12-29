Config { font = "xft:DejaVu Sans Mono-14"
       , bgColor = "#252525"
       , fgColor = "#BCBCBC"
       , position = Top
       , allDesktops = True
       , lowerOnStart = True
       , commands = [ Run StdinReader
                    , Run Date "%b %d %H:%M " "date" 10
                    , Run Com "/home/anler/.local/bin/wifi.sh" [] "wifi" 30
                    , Run Battery [ "--template", "⚡ <acstatus>"
                                  , "--Low", "10"
                                  , "--High", "80"
                                  , "--low", "darkred"
                                  , "--normal", "darkorange"
                                  , "--high", "darkgreen"
                                  , "--"
                                  , "-o", "<left>% (<timeleft>)"
                                  , "-O", "<fc=#dAA520>Charging</fc>"
                                  , "-i", "<fc=#006000>Charged</fc>"
                                  ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% } %date% { <fc=#ffa72c>%wifi%</fc> %battery%"
       }
