
<!-- README.md is generated from README.Rmd. Please edit that file -->

# injurytools <img src="man/figures/logo.png" align="right" height ="142" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/lzumeta/injurytools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lzumeta/injurytools?branch=master)
[![R-CMD-check](https://github.com/lzumeta/injurytools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lzumeta/injurytools/actions/workflows/R-CMD-check.yaml)
[![MIT
license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

<!-- - Shiny: -->
<!-- - Authors: -->
<!-- - Version: -->

## Overview

**`injurytools`** is a package aimed at Sports Medicine that facilitates
the data analysis workflow by providing convenience functions and handy
tools for sports injury data.

The functions can be classified into (sports injury) data preparation,
descriptive analyses and data visualization routines. Further analyses,
such as the estimation of the risk of injury with other covariate
effects, can be performed outside of **`injurytools`**, whether the
event of injury (outcome variable) is seen as count or time-to-event
data.

To get an overview of the package, see the
[Articles](https://lzumeta.github.io/injurytools/articles/) section.

In practice, the package can help automate certain descriptive reports
that are routinely made for sports injury surveillance.

## Installation

<!-- To install from [CRAN](https://CRAN.R-project.org): -->
<!-- ``` {r} -->
<!-- install.packages("injurytools") -->
<!-- ``` -->

To install the most current version from GitHub:

``` r
devtools::install_github("lzumeta/injurytools")
```

We are also planning to release the package to
[CRAN](https://CRAN.R-project.org). We expect it to be available soon.

## Usage

Most functions contain or start with `inj*()` which stands for
**injury**. Functions for data preparation start with `prepare_*()`; and
those for data visualization with `gg_inj*()`.

Below is a quick look at how **`injurytools`** can help to give a
comprehensive picture of injury data:

``` r
library(injurytools)
library(ggplot2)

gg_injphoto(injd, 
            title   = "Overview of injuries:\nLiverpool FC 1st male team during 2017-2018 and 2018-2019 seasons",
            by_date = "2 month") +
  ## plus some lines of ggplot2 code..
  xlab("Follow-up date") + ylab("Players") + 
  labs(caption = "source: transfermarkt.com") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
        axis.text.x.bottom = element_text(size = 13, angle = 20, hjust = 1),
        axis.text.y.left   = element_text(size = 12),
        axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
        axis.title.y = element_text(size = 20, face = "bold", vjust = 1.8),
        legend.text  = element_text(size = 20),
        plot.caption = element_text(face = "italic", size = 12, colour = "gray10"))
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

## How to contribute

If you have problems with the package, find bugs or have suggestions for
improvements, please feel free to open a [GitHub
issue](https://github.com/lzumeta/injurytools/issues) or touch us
directly via email. Also if you want to give us your feedback.

<!-- ## References -->
