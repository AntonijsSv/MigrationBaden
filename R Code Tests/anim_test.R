library(gganimate)

p <- ggplot(data.frame(val_x = seq(5, -4.9, -0.1), val_y = seq(5, -4.9, -0.1), time = 1:100)) +
  geom_segment(arrow = arrow(length = unit(0.5, "cm")), 
               color = "#ffc559", linewidth = 1.2, alpha = 0.6,
               aes(x = 5, y = 5, xend = val_x, yend = val_y)) +
  #geom_hline(color = "#ffc559", linewidth = 1.2, yintercept = 0) +
  #geom_vline(color = "#ffc559", linewidth = 1.2, xintercept = 0) +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#11141a"),
        plot.background = element_rect(fill = "#11141a")) +
  transition_manual(time)

animate(p, fps = 100)
