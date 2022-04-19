remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)
library(ggplot2)

colors <- c("bird"="#00A0B0","cat"="#CC333F", "frog"="#CBE86B", "mouse"="#EDC951")
spaghetti0(animals_q, x=x_new, y=value, by=taxa) +
  scale_color_manual(values=colors) +
  guides(col="none") +
  theme_void() + theme_transparent() -> p

sticker(p, package="pataqu", p_color ="grey90", p_size=24,
        h_color = "#EDC951", h_fill="grey10",  s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/pataqu.png") %>% print()
