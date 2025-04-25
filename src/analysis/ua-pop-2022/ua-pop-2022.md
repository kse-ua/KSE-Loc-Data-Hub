---
title: "UA population in 2020"
author: "First Last"  
date: "last Updated: 2022-09-02"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: show
    theme: simplex
    highlight: tango
editor_options: 
  chunk_output_type: console
---

This report ( add a brief description and the purpose of the report)
Demonstration of the __ISOLATED__ template

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->




# Environment

<!-- Load packages, or at least verify they're available on the local machine.  Suppress the output when loading packages. -->

```r
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings, but consider `stringi` as more general
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)     # data wrangling
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# for asserting conditions meet expected patterns.
requireNamespace("scales"   )# formatting
```

<!-- Load the sources.  Suppress the output when loading sources. --> 

```r
base::source("./scripts/common-functions.R") # project-level
```

<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 

```r
Sys.setlocale("LC_CTYPE", "ukr")
```

```
[1] "Ukrainian_Ukraine.1251"
```

```r
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/ua-pop-2022/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_data_input <- "./data-private/raw/ua-pop-2022.xlsx"
```

# Data


```r
ds0 <- readxl::read_xlsx(
  path_data_input
  , sheet = "12-47"
  , skip = 5
  , col_names = c("unit_ua","pop_count","unit_en" ) 
)
ds0 %>% glimpse()
```

```
Rows: 1,754
Columns: 3
$ unit_ua   [3m[38;5;246m<chr>[39m[23m "Україна", "Міське населення", "Сільське населення", NA, "Автономна Республіка К~
$ pop_count [3m[38;5;246m<chr>[39m[23m "41167335", "28693708", "12473627", NA, "…", NA, "1509515", "789588", "719927", ~
$ unit_en   [3m[38;5;246m<chr>[39m[23m "Ukraine", "Urban population", "Rural population", NA, "Autonomous Republic of C~
```

```r
ds0
```

```
# A tibble: 1,754 x 3
   unit_ua                   pop_count unit_en                      
   <chr>                     <chr>     <chr>                        
 1 Україна                   41167335  Ukraine                      
 2 Міське населення          28693708  Urban population             
 3 Сільське населення        12473627  Rural population             
 4 <NA>                      <NA>      <NA>                         
 5 Автономна Республіка Крим …         Autonomous Republic of Crimea
 6 <NA>                      <NA>      <NA>                         
 7 Вінницька область         1509515   Vinnytsya oblast             
 8 Міське населення          789588     Urban population            
 9 Сільське населення        719927     Rural population            
10 Вінницький район          647966    Vinnytskyi district          
# ... with 1,744 more rows
# i Use `print(n = ...)` to see more rows
```



# Analysis 








Session Information {#session-info}
===========================================================================

For the sake of documentation and reproducibility, the current report was rendered in the following environment.  Click the line below to expand.

<details>
  <summary>Environment <span class="glyphicon glyphicon-plus-sign"></span></summary>

```
- Session info -----------------------------------------------------------------------------------
 setting  value
 version  R version 4.1.2 (2021-11-01)
 os       Windows 10 x64 (build 22000)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  English_United States.1252
 ctype    Ukrainian_Ukraine.1251
 tz       America/Denver
 date     2022-09-02
 rstudio  2021.09.1+372 Ghost Orchid (desktop)
 pandoc   2.16.2 @ C:/Users/Andriy/AppData/Local/Programs/Quarto/bin/ (via rmarkdown)

- Packages ---------------------------------------------------------------------------------------
 package     * version date (UTC) lib source
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.1.2)
 backports     1.4.1   2021-12-13 [1] CRAN (R 4.1.2)
 bit           4.0.4   2020-08-04 [1] CRAN (R 4.1.2)
 bit64         4.0.5   2020-08-30 [1] CRAN (R 4.1.2)
 broom         0.7.10  2021-10-31 [1] CRAN (R 4.1.2)
 bslib         0.3.1   2021-10-06 [1] CRAN (R 4.1.2)
 cachem        1.0.6   2021-08-19 [1] CRAN (R 4.1.2)
 callr         3.7.0   2021-04-20 [1] CRAN (R 4.1.2)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.1.2)
 cli           3.1.0   2021-10-27 [1] CRAN (R 4.1.2)
 colorspace    2.0-2   2021-06-24 [1] CRAN (R 4.1.2)
 crayon        1.5.1   2022-03-26 [1] CRAN (R 4.1.3)
 DBI           1.1.1   2021-01-15 [1] CRAN (R 4.1.2)
 dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.1.2)
 desc          1.4.0   2021-09-28 [1] CRAN (R 4.1.2)
 devtools      2.4.3   2021-11-30 [1] CRAN (R 4.1.2)
 digest        0.6.29  2021-12-01 [1] CRAN (R 4.1.2)
 dplyr       * 1.0.7   2021-06-18 [1] CRAN (R 4.1.2)
 DT            0.20    2021-11-15 [1] CRAN (R 4.1.2)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.2)
 evaluate      0.14    2019-05-28 [1] CRAN (R 4.1.2)
 explore       0.7.1   2021-06-04 [1] CRAN (R 4.1.2)
 fansi         0.5.0   2021-05-25 [1] CRAN (R 4.1.2)
 fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.1.2)
 forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.1.2)
 fs            1.5.2   2021-12-08 [1] CRAN (R 4.1.2)
 generics      0.1.3   2022-07-05 [1] CRAN (R 4.1.3)
 ggplot2     * 3.3.6   2022-05-03 [1] CRAN (R 4.1.3)
 glue          1.5.1   2021-11-30 [1] CRAN (R 4.1.2)
 gridExtra     2.3     2017-09-09 [1] CRAN (R 4.1.2)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 4.1.2)
 haven         2.4.3   2021-08-04 [1] CRAN (R 4.1.2)
 hms           1.1.1   2021-09-26 [1] CRAN (R 4.1.2)
 htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.1.2)
 htmlwidgets   1.5.4   2021-09-08 [1] CRAN (R 4.1.2)
 httpuv        1.6.5   2022-01-05 [1] CRAN (R 4.1.3)
 httr          1.4.2   2020-07-20 [1] CRAN (R 4.1.2)
 import        1.2.0   2020-09-24 [1] CRAN (R 4.1.2)
 janitor       2.1.0   2021-01-05 [1] CRAN (R 4.1.2)
 jquerylib     0.1.4   2021-04-26 [1] CRAN (R 4.1.2)
 jsonlite      1.8.0   2022-02-22 [1] CRAN (R 4.1.3)
 knitr       * 1.36    2021-09-29 [1] CRAN (R 4.1.2)
 labelled    * 2.9.0   2021-10-29 [1] CRAN (R 4.1.2)
 later         1.3.0   2021-08-18 [1] CRAN (R 4.1.2)
 lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.2)
 lubridate   * 1.8.0   2021-10-07 [1] CRAN (R 4.1.2)
 magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.1.2)
 memoise       2.0.1   2021-11-26 [1] CRAN (R 4.1.2)
 mime          0.12    2021-09-28 [1] CRAN (R 4.1.1)
 modelr        0.1.8   2020-05-19 [1] CRAN (R 4.1.2)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 4.1.2)
 pillar        1.8.0   2022-07-18 [1] CRAN (R 4.1.3)
 pkgbuild      1.3.0   2021-12-09 [1] CRAN (R 4.1.2)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.2)
 pkgload       1.2.4   2021-11-30 [1] CRAN (R 4.1.2)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.1.2)
 processx      3.5.2   2021-04-30 [1] CRAN (R 4.1.2)
 promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.1.2)
 ps            1.6.0   2021-02-28 [1] CRAN (R 4.1.2)
 purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.1.2)
 R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.2)
 Rcpp          1.0.8.3 2022-03-17 [1] CRAN (R 4.1.3)
 readr       * 2.1.1   2021-11-30 [1] CRAN (R 4.1.2)
 readxl        1.3.1   2019-03-13 [1] CRAN (R 4.1.2)
 remotes       2.4.2   2021-11-30 [1] CRAN (R 4.1.2)
 reprex        2.0.1   2021-08-05 [1] CRAN (R 4.1.2)
 rlang         1.0.4   2022-07-12 [1] CRAN (R 4.1.3)
 rmarkdown     2.11    2021-09-14 [1] CRAN (R 4.1.2)
 rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.1.2)
 rsconnect     0.8.25  2021-11-19 [1] CRAN (R 4.1.2)
 rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.2)
 rvest         1.0.2   2021-10-16 [1] CRAN (R 4.1.2)
 sass          0.4.1   2022-03-23 [1] CRAN (R 4.1.3)
 scales        1.2.0   2022-04-13 [1] CRAN (R 4.1.3)
 sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.2)
 shiny         1.7.1   2021-10-02 [1] CRAN (R 4.1.2)
 snakecase     0.11.0  2019-05-25 [1] CRAN (R 4.1.2)
 stringi       1.7.6   2021-11-29 [1] CRAN (R 4.1.2)
 stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.1.2)
 testit        0.13    2021-04-14 [1] CRAN (R 4.1.2)
 testthat      3.1.1   2021-12-03 [1] CRAN (R 4.1.2)
 tibble      * 3.1.6   2021-11-07 [1] CRAN (R 4.1.2)
 tidyr       * 1.1.4   2021-09-27 [1] CRAN (R 4.1.2)
 tidyselect    1.1.2   2022-02-21 [1] CRAN (R 4.1.3)
 tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.1.2)
 tzdb          0.2.0   2021-10-27 [1] CRAN (R 4.1.2)
 usethis       2.1.5   2021-12-09 [1] CRAN (R 4.1.2)
 utf8          1.2.2   2021-07-24 [1] CRAN (R 4.1.2)
 vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.1.2)
 vroom         1.5.7   2021-11-30 [1] CRAN (R 4.1.2)
 withr         2.5.0   2022-03-03 [1] CRAN (R 4.1.3)
 xfun          0.29    2021-12-14 [1] CRAN (R 4.1.2)
 xml2          1.3.3   2021-11-30 [1] CRAN (R 4.1.2)
 xtable        1.8-4   2019-04-21 [1] CRAN (R 4.1.2)
 yaml          2.2.1   2020-02-01 [1] CRAN (R 4.1.1)

 [1] C:/Users/Andriy/OneDrive/Documents/R/win-library/4.1
 [2] C:/Program Files/R/R-4.1.2/library

--------------------------------------------------------------------------------------------------
```
</details>



Report rendered by Andriy at 2022-09-02, 08:11 -0600 in 1 seconds.
