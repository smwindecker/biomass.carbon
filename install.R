purrr::walk2(.x = c("dplyr", "magrittr", "ape", "stringr", "minpack.lm", "phytools", , .y = c("0.7.8", "1.5", "5.1", "1.3.1", "1.2-1", "0.6-44", "2.5-2", , ~devtools::install_version(package = .x, version = .y))
devtools::install_github(c("/@", "smwindecker/mixchar@7dc30e0380e924e4bc595cc272609d1d4ada15e2", "/@"))
purrr::walk2(.x = "vegan", "tidyr", "xtable", "reshape2", "adephylo", "ade4"), .y = "0.8.2", "1.8-2", "1.4.3", "1.1-11", "1.7-11"), ~devtools::install_version(package = .x, version = .y))
devtools::install_github(c("/@", "smwindecker/mixchar@7dc30e0380e924e4bc595cc272609d1d4ada15e2", "/@"))
