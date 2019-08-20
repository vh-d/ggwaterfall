
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggwaterfall

<!-- badges: start -->

<!-- badges: end -->

ggwaterfall provides tools to make waterfall charts based on ggplot2

## Example

Generate some random data.

``` r
library(ggwaterfall)
library(data.table)
library(magrittr)
library(ggplot2)

# simulate data
set.seed(1L)

nitems <- 5
ntime <- 2

DT <-
  data.table(
    group = rep(c("Group 1", "Group 2"), each = nitems),
    item  = paste0("item ", rep(letters[1:nitems], each = ntime)),
    time  = rep(1:ntime, times = nitems),
    value = 6 + rnorm(nitems * ntime)
  )

DT
#>       group   item time    value
#>  1: Group 1 item a    1 5.373546
#>  2: Group 1 item a    2 6.183643
#>  3: Group 1 item b    1 5.164371
#>  4: Group 1 item b    2 7.595281
#>  5: Group 1 item c    1 6.329508
#>  6: Group 2 item c    2 5.179532
#>  7: Group 2 item d    1 6.487429
#>  8: Group 2 item d    2 6.738325
#>  9: Group 2 item e    1 6.575781
#> 10: Group 2 item e    2 5.694612
```

``` r

waterfall(
  data       = DT,
  detail_var = "item",
  base_var   = "time",
  value_var  = "value"
) %>%
  plot()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Plot a more advanced waterfall charts with multiple periods.

``` r
# simulate new data
set.seed(1L)

nitems <- 4
ntime <- 3

DT <-
  data.table(
    group = rep(c("Group 1", "Group 2"), each = nitems),
    item  = paste0("item ", rep(letters[1:nitems], each = ntime)),
    time  = rep(1:ntime, times = nitems),
    value = 6 + rnorm(nitems * ntime)
  )
#> Warning in data.table(group = rep(c("Group 1", "Group 2"), each =
#> nitems), : Item 1 is of size 8 but maximum size is 12 (recycled leaving
#> remainder of 4 items)

DT
#>       group   item time    value
#>  1: Group 1 item a    1 5.373546
#>  2: Group 1 item a    2 6.183643
#>  3: Group 1 item a    3 5.164371
#>  4: Group 1 item b    1 7.595281
#>  5: Group 2 item b    2 6.329508
#>  6: Group 2 item b    3 5.179532
#>  7: Group 2 item c    1 6.487429
#>  8: Group 2 item c    2 6.738325
#>  9: Group 1 item c    3 6.575781
#> 10: Group 1 item d    1 5.694612
#> 11: Group 1 item d    2 7.511781
#> 12: Group 1 item d    3 6.389843
```

Plot a simple waterfall chart.

``` r

waterfall(
  data       = DT,
  detail_var = "item",
  base_var   = "time",
  value_var  = "value"
) %>%
  plot()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

You can make use of facetting with `by_var` arguments:

``` r

waterfall_data(
  data       = DT,
  detail_var = "item",
  base_var   = "time",
  by_var     = "group",
  value_var  = "value"
) %>%
  waterfall_plot()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Or flip the chart with flip = TRUE:

``` r

waterfall_data(
  data       = DT,
  detail_var = "item",
  base_var   = "time",
  by_var     = "group",
  value_var  = "value"
) %>%
  waterfall_plot(
    select = (time > 1 | is_aggr),
    flip = TRUE,
  )
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Use can still adapt the chart with comomn ggplot2 API:

``` r

DTwf <- 
  waterfall_data(
    data       = DT,
    detail_var = "item",
    base_var   = "time",
    by_var     = "group",
    value_var  = "value"
  )

DTwf %>% 
  waterfall_plot(
    flip = TRUE,
  ) + 
  ggtitle("Add this title here")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

\`\`\`
