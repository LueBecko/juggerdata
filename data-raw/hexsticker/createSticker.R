# just a aux script that generated the current hexsticker of the package
library(hexSticker)

sticker("data-raw/hexsticker/juggerdatalogo_contour.png",
        package = "juggerdata",
        p_size=14, p_x = 1, p_y = 1.5, p_color = "#FFFFFF",
        s_x=1, s_y=0.85, s_width=.65,
        h_color = "#8b8bf3", h_fill = "#dcdcf3",
        filename = "juggerdata.png")
