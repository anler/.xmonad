Config { font = "xft:DejaVu Sans Mono-14"
       , bgColor = "#252525"
       , fgColor = "#BCBCBC"
       , border = FullB
       , borderColor = "#333"
       , position = Top
       , pickBroadest = False
       , allDesktops = True
       , lowerOnStart = True
       , commands = [ Run StdinReader
                    , Run Date "%b %d %H:%M " "date" 10
                    , Run Battery [ "--template", "<acstatus> "
                                  , "--Low", "10"
                                  , "--High", "80"
                                  , "--low", "darkred"
                                  , "--normal", "darkorange"
                                  , "--high", "darkgreen"
                                  , "--"
                                  , "-o", "<left>%(<timeleft>)"
                                  , "-O", "<fc=#dAA520>⚡</fc>"
                                  , "-i", "<fc=#006000>⚡⚡</fc>"
                                  ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% } { %date% %battery%"
       }
