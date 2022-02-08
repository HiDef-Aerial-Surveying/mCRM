library(hexSticker)
library(showtext)

font_add_google("Roboto")
showtext_auto()

sticker("C:/TEMP/HexWF.png",package="stochLAB",p_size=20,s_x=1,s_y=0.75,s_width=0.6,
        filename="inst/app/www/hexSticker_stochLAB.png",p_family = "Roboto",
        h_fill = "#A96036",spotlight = TRUE,l_y = 1.2,l_x=1.5,h_color = "black")
