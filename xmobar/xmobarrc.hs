Config { font = "xft:DejaVu Sans Mono-10"
       , bgColor = "#000000"
       , fgColor = "#0080ff"
       , border = FullB
       , borderColor = "#000"
       , position = Top
       , pickBroadest = False
       , allDesktops = True
       , lowerOnStart = True
       , commands = [ Run StdinReader
                    , Run Battery [ "--template", "<acstatus> "
                                  , "--Low", "10"
                                  , "--High", "80"
                                  , "--low", "darkred"
                                  , "--normal", "#0080ff"
                                  , "--high", "#0080ff"
                                  , "--"
                                  , "-o", "<left>%(<timeleft>)"
                                  , "-O", "<fc=#0080ff>⚡</fc>"
                                  , "-i", "<fc=#0080ff>⚡⚡</fc>"
                                  ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%battery% } %StdinReader% {"
       }
