
Config { font               = "xft:Source Code Pro:pixelsize=11:antialias=true:hinting=true"
       , additionalFonts  = [ "xft:Mononoki Nerd Font Mono:pixelsize=14:antialias=true:hinting=true"
                            , "xft:Source Code Pro:pixelsize=11:antialias=true:hinting=false:bold"
                            , "xft:Source Code Pro:pixelsize=11:antialias=true:hinting=false:italic" ]
       , borderColor      = "black"
       , border           = NoBorder
       , bgColor          = "#181512"
       , fgColor          = "#817267"
       , alpha            = 200
       , position         = Top
       , textOffset       = -1
       , iconOffset       = -1
       , iconRoot         = "/home/sundish/.xmonad/icons/"
       , lowerOnStart     = True
       , pickBroadest     = False
       , persistent       = False
       , hideOnStart      = False
       , allDesktops      = True
       , overrideRedirect = True
       , commands = [ Run Cpu [ "-L", "3", "-H", "50", "--normal","#0F829D","--high","#B91E2E"
                              , "-t", "<fc=#B87E7E>Cpu</fc>: <total>%"] 10
                    , Run Memory [ "-t","<fc=#B87E7E>Mem</fc>: <usedratio>%"
                                 , "-m", "2", "-c", "0"
                                 , "-L", "3", "-H", "50"
                                 , "--normal", "#0F829D", "--high", "#B91E2E"] 10
                    , Run Com "maaya" [] "maya_date" 600
	            , Run Com "/home/sundish/Documents/scripts/health_rpg" [] "health_rpg" 50
                    , Run Date "<fc=#A5C261>%I:%M %p</fc>" "date" 10
                      -- , Run Volume "default" "Master" [ "-t", "<fc=#B87E7E>Vol</fc>: <volume>%"
                      --                              , "--normal", "#0F829D"
		      --			      , "--high"  , "#B91E2E" ] 10 > %default:Master% <fn=1>|
		      -- , Run $ Battery [ "-t", "<fc=#67b500><acstatus></fc>"
		      --     	    , "-L", "20", "-H", "90"
                      --                 , "-l", "#dc322f"
                      --                 , "-n", "#d4ca17"
                      --                 , "-h", "#2dd417"
		      --     	    , "--"
		      --     	    , "-o", "<fn=1>\xf242</fn> <left>%"
             	      --     	    , "-O", "<fn=1>\xf1e6</fn> <left>%"
		      --     	    , "-i", "<fn=1>\xf1e6</fn> <left>%"
                                      -- , "-a", "dunstify -u critical -a 'Sys' 'IMPORTANT' 'Low Battery! Connect'"
                                      -- , "-A", "91"
		      --     	    , "--off-icon-pattern", "<fn=1>\xf1e6</fn>"
		      --     	    , "--on-icon-pattern", "<fn=1>\xf1e6</fn>"] 10
                      -- , Run CoreTemp [ "-t", "<core0>°/<core1>C"
                      --                , "-L", "40", "-H", "60"
                      --                , "-l", "lightblue", "-n", "gray90", "-h", "red"] 50
                      -- , Run Locks
		    , Run StdinReader]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }<fc=#8C9440>%maya_date%</fc> : %date%{ %cpu% - %memory% | <fn=1>%health_rpg%</fn> "
       }
