#The script was adapted from [Will Beasley's illustrations](https://github.com/OuhscBbmc/DeSheaToothakerIntroStats/blob/master/CommonCode/BookTheme.R)



#For fonts, see Chang (2013) Recipe 14.6.  Install ghostscript (http://www.ghostscript.com/download/gsdnld.html),
# before installing the `extrafont` package.
#Run the following three lines of code once per machine (not once per session).
# install.packages("extrafont")
# library(extrafont)
# extrafont::font_import()
# extrafont::fonts() #This just lists the available fonts for you to read; similar to extrafont::fonttable()

# library(extrafont)
library(grid)
library(ggplot2)
library(dichromat)
library(RColorBrewer)
# devtools::install_github(repo="Melinae/TabularManifest")
# library(TabularManifest) # custom histographs for quick EDA

#########################################################
### Define theme elements for ggplot2 graphs
#########################################################
# Documentation for modifiable theme elements can be found at http://docs.ggplot2.org/current/theme.html
baseSize <- 10
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
      ,panel.grid = element_line(color = "grey95")
      ,panel.border = element_rect(color = "grey80")
      ,axis.ticks = element_blank()
      ,text=element_text(size=baseSize)
    )
)

# NoGridOrYLabelsTheme <- main_theme  +
#   theme(axis.ticks.y = element_blank()) +
#   theme(panel.grid = element_blank()) +
#   theme(plot.margin=unit(c(.1,.2,.2,0), "lines"))

#########################################################
### Define palettes and colors
#########################################################
# for variable sets, so they're consistent across graphs & chapters

# High contrast colorblind safe palette for binary variable:
# https://colorbrewer2.org/#type=diverging&scheme=PuOr&n=3
binary_colors <- c(
  "TRUE"   = "#f1a340" # orange
  ,"FALSE" = "#998ec3" # purple
)

# main palette of the Government of Alberta visual identity (edition 23)
# https://open.alberta.ca/publications/government-of-alberta-visual-identity-manual
# the sequence reflects recommended frequency of use
abcol <- c(
  "grey"        = "#5f6a72" # stone   - grey
  ,"magenta"    = "#d40072" # dusk    - magenta
  ,"brown"      = "#ff7900" # sunset  - brown
  ,"green"      = "#77b800" # pasture - green
  ,"blue"       = "#00aad2" # sky     - blue
  ,"yellow"     = "#edb700" # prairie - yellow
)

# standard palette to be used across IHACRU reports
# qualitative, 9 categories (max), printer friendly
# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
# add colors in sequential order
acru_colors_9 <- c(
  "red"     = "#e41a1c" # red
  ,"blue"   = "#377eb8" # blue
  ,"green"  = "#4daf4a" # green
  ,"purple" = "#984ea3" # purple
  ,"orange" = "#ff7f00" # orange
  ,"yellow" = "#ffff33" # yellow
  ,"brown"  = "#a65628" # brown
  ,"pink"   = "#f781bf" # pink
  ,"grey"   = "#999999" # grey
)

# to color the effects in a glm model
pal_direction_significance <-  c(
  "Increase (99%)"   = "#2b8cbe"
  ,"Increase (95%)"  = "#7bccc4"
  ,"Increase (90%)"  = "#bae4bc"
  ,"Not Significant" = "NA"
  ,"Decrease (90%)"  = "#fdcc8a"
  ,"Decrease (95%)"  = "#fc8d59"
  ,"Decrease (99%)"  = "#d7301f"
)

#
# transformColor <- function( palette ) {
#   return( palette )
# #   return( dichromat(palette, "deutan") )
# #   return( dichromat(palette, "protan") )
# #   return( dichromat(palette, "tritan") )
# # Also see The Color Oracle application (http://colororacle.org/)
# }
#
# PalettePregancyDelivery <- transformColor(adjustcolor(brewer.pal(3, "Accent"), alpha.f=1)[1:2])
# PalettePregancyDeliveryBad <- transformColor( c("#FF0000CC", "#00FFFFCC")) #Translucent red & cyan
#
# PalettePregancyGroup <- transformColor(adjustcolor(brewer.pal(3, "Dark2"), alpha.f=1)[1:2])
# PalettePregancyGroupLight <- adjustcolor(PalettePregancyGroup, alpha.f=.2)
# PalettePregancyGroupBad <- transformColor(adjustcolor(c("blue", "maroon"), alpha.f=.7))
#
# PaletteObesityState <- transformColor(adjustcolor(brewer.pal(5, "Set1"))[c(1,2)])
# PaletteObesityState <- transformColor(adjustcolor(brewer.pal(5, "Dark2"))[c(2,3)])
#
# PaletteWorldDeathsRestricted <- transformColor(c("#497862", "#A54891")) #Hand-picked
# PaletteWorldDeathsRestrictedFaint <- transformColor(adjustcolor(PaletteWorldDeathsRestricted, alpha.f=.2))
#
# PaletteControlPsqiLight <- transformColor(c("#1A7F7C", "#1595B2")) #From http://colrd.com/palette/22521/; http://colrd.com/palette/18981/
# PaletteControlPsqiDark <- transformColor(c("#215f5c", "#225a88")) #From http://colrd.com/palette/22521/; http://colrd.com/palette/18981/
# #
#
# #Use the same palette as the crit graphs in Chapters 10-12.
# #  * Purple is the distribution line
# #  * Blue corresponds to the observed values
# #  * Red corresponds to a 5% alpha
# #  * Orange corresponds to a 1% alpha
# bluish <- "#1d00b2" #"http://colrd.com/color/0xff1d00b2/;  Others I tried: #230ca2" #"#000066" #"#0868ac" ##5698c4"
# greenish <- "#097168" #http://colrd.com/palette/22444/
# PaletteCritical <- c("#544A8C", "#ce2b18", "#F37615", bluish, greenish, "gray60") #Adapted from http://colrd.com/palette/17511/ (I made the purple lighter, the orange darker, and added the blue.)
# PaletteCriticalLight <- adjustcolor(PaletteCritical, alpha.f=.5)
# rm(bluish, greenish)
# # palettePregancy <- RColorBrewer::brewer.pal(n=4, name="Set2")[3:4]
# # PaletteObesityState <-  adjustcolor(brewer.pal(4, "Set2"))[3:4]
# # PaletteObesityStateBad <- adjustcolor(c("green", "red"), alpha.f=.7)
#
# #Named colors in R:
# # http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
#

#########################################################
### Declare functions used in multiple documents
#########################################################
# #This function is directly from Recipe 13.3 in Chang (2013).
# LimitRange <- function( fun, min, max ) {
#   function( x ) {
#     y <- fun(x)
#     y[(x < min) | (max < x)] <- NA
#     return( y )
#   }
# }
# DrawWithoutPanelClipping <- function( g ) {
#   gt <- ggplot_gtable(ggplot_build(g))
#   gt$layout$clip[gt$layout$name == "panel"] <- "off"
#   grid.draw(gt)
# }
# TukeyBoxplot <- function(y, width=.9, na.rm = FALSE, coef = 1.5, ...) {
#   #Adapted from https://github.com/hadley/ggplot2/blob/master/R/stat-boxplot.r
#   qs <- c(0, 0.25, 0.5, 0.75, 1)
#   stats <- as.numeric(quantile(y, qs, type=5))
#   names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
#
#   iqr <- diff(stats[c(2, 4)])
#
#   outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)
#   if (any(outliers)) stats[c(1, 5)] <- range(c(stats[2:4], y[!outliers]), na.rm=TRUE)
#
#   df <- as.data.frame(as.list(stats))
#   df$outliers <- I(list(y[outliers]))
#
#   n <- sum(!is.na(y))
#
#   df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
#   df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
#   df$width <- width
#   return( df )
# } # TukeyBoxplot(dsPregnancy$BabyWeightInKG)
#
RemoveLeadingZero <- function( x ) {
  #   g <- grep("\\A\\b(?<=0)(\\.\\d{1,})$", x, perl=TRUE, value=TRUE);
  g <- gsub("\\b(0)(\\.\\d{1,})$", "\\2", x, perl=TRUE);
  return( g )
} #
# RemoveLeadingZero(0.444)
# # RemoveLeadingZero(431.444)
# WrapColumns <- function( d, wrapCount=3L ) {
#   rowCountOriginal <- nrow(d)
#   columnCountOriginal <- ncol(d)
#   pad <- ((rowCountOriginal %% wrapCount) > 0)
#   rowCount <- (rowCountOriginal %/% wrapCount) + as.integer(pad)
#
#   dt <- matrix(NA, nrow=rowCount, ncol=columnCountOriginal*wrapCount)
#   for( wrapIndex in seq_len(wrapCount) ) {
#     columnIndices <- (wrapIndex-1)*columnCountOriginal + seq_len(columnCountOriginal)
#     rowIndices <- (wrapIndex-1)*rowCount + seq_len(rowCount)
#     dt[, columnIndices] <- as.matrix(d[rowIndices, ])
#   }
#   dt <- ifelse(is.na(dt), "", dt)
#   dWide <- as.data.frame(dt, stringsAsFactors=F)
#   colnames(dWide) <- rep(colnames(d), times=wrapCount)
#   return( dWide )
# }

# remove leading zero and remove decimals
numformat <- function(val, decimal_count = 2){
  # val <- 0.003
  # decimal_count <- 2
  # (rounded_value = round(val,.01))
  format_expression <- paste0("%.",decimal_count,"f")

  sub("^(-?)0.", "\\1.", sprintf(format_expression, val))

}


#########################################################
### Establish the font
#########################################################
## These three lines will use a nondefault font.
# extrafont::loadfonts() #Run this once per session.
# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.10/bin/gswin64c.exe")
# main_theme <- main_theme +  theme(text = element_text(family="Times New Roman"))




#########################################################
### Internal notes
#########################################################
# * The Pre-Press Manager said the dimensions of the images cannot exceed these dimensions: 33 picas wide x 51 picas tall. (5.5" x 8.5")
# *Physical Page width 7"x10"

# list.files(system.file("enc", package="grDevices"))




#########################################################
### Palettes to consider for future graphs
#########################################################
# https://github.com/jrnold/ggthemes
# c("#043227", "#097168", "#ffcc88", "#fa482e", "#f4a32e") #http://colrd.com/palette/22444/
# c("#fea3aa","#f8b88b","#faf884","#baed91","#b2cefe","#f2a2e8") #http://colrd.com/palette/22780/





