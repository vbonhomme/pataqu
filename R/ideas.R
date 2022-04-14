animals100 %>%
  dplyr::group_by(group, x_new) %>%
  dplyr::summarise(y=IQR(y))

spaghetti(alpha=0.5, size=0.1) +
  scale_color_manual(values=colors) +
  xlab("year") + ylab("value of interest") +
  guides(colour=guide_legend("taxa", override.aes=list(alpha=1, size=2)))

q_out = 0.05
probs <- c(0+q_out/2, 0.5, q_out/2)

colors <- c("bird"="#00A0B0","cat"="#CC333F", "frog"="#CBE86B", "mouse"="#EDC951")
animals100 %>%
  dplyr::group_by(group, x_new) %>%
  dplyr::summarize(q_min = quantile(y, q_out/2),
                   q_med = median(y),
                   q_max = quantile(y, 1-q_out/2),
                   .groups="drop"
  ) %>%
  ggplot() +
  aes(x=x_new, col=group) +
  # geom_ribbon would have been an alternative but in the end
  # its shorter not to remove guides for fill, etc.
  geom_line(aes(y=q_min), size=0.25) +
  geom_line(aes(y=q_max), size=0.25) +
  geom_line(aes(y=q_med), size=0.5,) +
  scale_color_manual(values=colors) +
  theme_minimal()

animals100 %>%
  ggplot() +
  aes(x=x_new, y=y, fill=group, group=interaction(group, x_new)) +
  geom_boxplot(position = position_dodge(width = 20))


animals100 %>%
  dplyr::filter(x_new==animals100$x_new[9]) %>%
  aov(y~group, data=.) %>% summary()

p_after_aov <- function(x){
  aov(y~group, data=x) %>%
    summary %>% unlist %>%
    `[`("Pr(>F)1") %>% as.numeric()
}

animals100 %>%
  dplyr::group_by(x_new) %>%
  tidyr::nest() %>%
  dplyr::mutate(aov=purrr::map_dbl(data, p_after_aov))
