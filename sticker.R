

library(ggplot2)
library(emojifont)
library(scales)
library(hexSticker)

load.fontawesome()

icon <- ggplot() +
  geom_text(aes(x = c(3), y = c(1), label = fontawesome('fa-table')),
            family = "fontawesome-webfont",
            size = 70) +
  geom_text(aes(x = c(0), y = 1, label = c("b")),
            size = 70, family = "Aller_Rg") +
  xlim(-1.5,5) +
  theme_void() + theme_transparent()

s <- sticker(icon, package="",
             s_x=1, s_y=1.15, s_width=1.8, s_height=2,
             filename="sticker.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "btable",
             u_size = 12,
             u_x = 1,
             u_y = 0.15
)
