library(tidyverse)
library(patchwork)

set.seed(184)

make_mc_plot <- function(n) {
  mc_df <- tibble(
    x = runif(n, -4, 4),
    y = runif(n, 0, 0.4)
  ) |>
    mutate(
      density_y = dnorm(x, mean = 0, sd = 1),
      flag = if_else(y <= density_y, "on/below", "above"),
      hit = if_else(flag == "on/below", 1, 0)
    )
  
  est_int <- mean(mc_df$hit) * (4 - (-4)) * (0.4 - 0)
  
  ggplot(mc_df, aes(x = x, y = y, color = flag)) +
    geom_point(alpha = 0.6, size = 1.5) +
    stat_function(
      fun = dnorm,
      args = list(mean = 0, sd = 1),
      xlim = c(-4, 4),
      linewidth = 1.2,
      color = "blue"
    ) +
    scale_color_manual(values = c("above" = "#F8766D", "on/below" = "#00BFC4")) +
    scale_x_continuous(limits = c(-4, 4)) +
    scale_y_continuous(limits = c(0, 0.4)) +
    labs(
      title = paste("Monte Carlo Integration Example, n =", n),
      subtitle = paste("Est. Numerical Integration:", round(est_int, 4)),
      x = "x",
      y = "y",
      color = "flag"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}

plot10 <- make_mc_plot(10)
plot100 <- make_mc_plot(100)
plot1000 <- make_mc_plot(1000)
plot10000 <- make_mc_plot(10000)

(plot10 + plot100) / (plot1000 + plot10000) 