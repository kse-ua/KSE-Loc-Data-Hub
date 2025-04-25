#' ---
#' title: "Ellis UA Admin"
#' author: "KSE"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./manipulation/ellis-ua-admin.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
opts_knit$set(root.dir='../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("\014") # Clear the console
#+ echo=FALSE, results="asis" --------------------------------------------------
cat("Report's native working directory: `", getwd(),"`") # Must be set to Project Directory
#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 1.Environment")
#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "russian")
#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages -----------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------

path_rda_heads<- "./data-private/raw/rda_heads.xlsx"


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- openxlsx::read.xlsx(path_rda_heads) %>% select(text,date)


#+ tweak-data, eval=eval_chunks ------------------------------------------------
ds1 <- 
  ds0 %>% 
  mutate(
    text = str_squish(text)
    ,name = str_extract(text, "((?<=(З|з)вільнити |відсторонити ).*(?= з посади| від ))|((?<=Призначити ).*(?= головою| тимчасово))")
    ,raion = str_extract(text, "(?<= головою |голови |обов'язки ).*(?= районно)")
    ,raion = str_replace(raion, "ої", "ий")
    ,oblast = str_extract(text, "(?<=адміністрації ).*(?= області)")
    ,oblast = str_replace(oblast, "ої", "а")
    ,action = case_when(
      str_detect(text, "(З|з)вільнити") ~ "dismiss"
      ,str_detect(text, "відсторонити") ~ "withdraw"
      ,str_detect(text, "(П|п)ризначити") ~ "appoint"
    )
  ) %>% 
  separate(name, c("last_name", "first_name", "family_name"), sep = " ")
#TO-DO - fix some names scraped unpropriately


ds1 %>% 
  mutate(
    last_name2 = str_to_title(last_name)
    ,last_name2 = case_when(
      str_detect(last_name2, "енка") ~ str_replace(last_name2, "енка", "енко")
      ,str_detect(last_name2, "єнка") ~ str_replace(last_name2, "єнка", "єнко")
      ,str_detect(last_name2, "ого") ~ str_replace(last_name2, "ого", "ий")
      ,str_detect(last_name2, "ева") ~ str_replace(last_name2, "ева", "ев")
      ,str_detect(last_name2, "єва") ~ str_replace(last_name2, "єва", "єв")
      ,str_detect(last_name2, "ова") ~ str_replace(last_name2, "ова", "ов")
      ,str_detect(last_name2, "ука") ~ str_replace(last_name2, "ука", "ук")
      ,str_detect(last_name2, "юка") ~ str_replace(last_name2, "юка", "юк")
      ,str_detect(last_name2, "іна") ~ str_replace(last_name2, "іна", "ін")
      ,str_detect(last_name2, "ина") ~ str_replace(last_name2, "ина", "ин")
      ,str_detect(last_name2, "ика") ~ str_replace(last_name2, "ика", "ик")
      ,str_detect(last_name2, "іка") ~ str_replace(last_name2, "іка", "ік")
      ,str_detect(last_name2, "евську") ~ str_replace(last_name2, "евську", "евська")
      ,str_detect(last_name2, "євську") ~ str_replace(last_name2, "євську", "євська")
      ,str_detect(last_name2, "овську") ~ str_replace(last_name2, "овську", "овська")
      ,str_detect(last_name2, "ову") ~ str_replace(last_name2, "ову", "ова")
      ,str_detect(last_name2, "йову") ~ str_replace(last_name2, "йову", "йова")
      
      
    )
  ) %>% View()

str_detect(str_to_title("ПАВЛЕНКА"),"eнка")

"ПАВЛЕНКА" == "ПАВЛЕНКА"

writexl::write_xlsx(ds1 %>% count(first_name), "./data-private/rda-names-coding.xlsx")

?write_xlsx

# # ,date = case_when(
# #   is.na(data) == F ~ str_remove(date, " року")
# #   ,is.na(data) ~ str_extract(text, "\\d{1,2}.*\\d{4}")
# # )
# ,date = str_extract(text, "\\d{1,2}.*\\d{4}")
# ,date = str_replace_all(date, c("січня" = ".01.", "лютого" = ".02.", "березня" = ".03.",
#                          "квітня" = ".04.", "травня" = ".05.", "червня" = ".06.",
#                          "липня" = ".07.", "серпня" = ".08.", "вересня" = ".09."
#                          ,"жовтня" =".10.", "листопада" = ".11.", "грудня" = ".12."))


ds1 %>% filter(is.na(name)) %>% View
#+ load-coded-data, eval=eval_chunks -------------------------------------------------

