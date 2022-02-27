# Common packages
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("scales")
  library("patchwork")
  library("bayesplot")
  library("ggExtra")
  library("ggpubr")
  library("viridis")
  library("ggokabeito")
})

set.seed(84735)
SEED <- 84735 # set random seed for reproducibility

theme_set(bayesplot::theme_default(base_size = 12))
bayesplot::color_scheme_set("gray")

# knitr chunk options ----------------------------------------------------------

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618, # 1 / phi
  # fig.show = "hold",
  # dpi = 300,
  # fig.pos = "h", # pdf mode
  # cache.extra = knitr::rand_seed,
  # tidy.opts = list(width.cutoff = 76),
  tidy = "styler"
)

# dplyr options ----------------------------------------------------------------

options(dplyr.print_min = 8, dplyr.print_max = 8)
