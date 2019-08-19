library(ggwaterfall)
library(data.table)
library(magrittr)

DT <- data.table(d = "sdfs", id = rep(letters[1:10], each = 2), x = rep(1:2, times = 10), value = rnorm(20))

DT

waterfall_data(
  data = DT,
  detail_var = "id",
  base_var = "x",
  by_var = "d",
  value_var = "value"
) %>%
  waterfall_plot(
    color_palette = ggwaterfall::waterfall_colors
  )
