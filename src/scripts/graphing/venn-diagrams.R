library(VennDiagram)
# ls_venn  <- list("IS" = ids_is, "EA" = ids_ea)
#
# display_venn <- function(x, ...){
#   grid.newpage()
#   venn_object <- VennDiagram::venn.diagram(x, filename = NULL, ...)
#   grid.draw(venn_object)
# }
# ls_venn %>% display_venn()
# ls_venn %>% ggvenn::ggvenn()



view_venn_two <- function(
  # Dependency - VennDiagram package
  d1       # vector of values in the         first data frame
  ,d1_label # label for the variable from the first data frame
  ,d2       # vector of values in the         second data frame
  ,d2_label # label for the variable from the second data frame
  ,...      # additional arguments passed to VennDiagram package
){
  # values for testing and development
  # d1 <- dto$record$sgs$id
  # d2 <- dto$record$encounter$id
  # d1_label = "SGS"
  # d2_label = "Encounter"

  # compute the quantities needed for graphing
  (d1_n     = d1 %>% unique() %>% length())
  (d2_n     = d2 %>% unique() %>% length())
  # (unique_to_d1 <- setdiff(d1, d2) %>% length())
  # (unique_to_d2 <- setdiff(d2, d1) %>% length())
  # (union_d1_d2  <- union(d1,d2)    %>% length())
  (intersect_d1_d2  <- dplyr::intersect(d1,d2)    %>% length())

  # Format labels
  area1_label <- paste0(d1_label,"\n(n = ", d1_n," )" )
  area2_label <- paste0(d2_label,"\n(n = ", d2_n," )" )

  grid.newpage()
  plot <-
    VennDiagram::draw.pairwise.venn(
      area1 = d1_n
      ,area2 = d2_n
      ,cross.area = intersect_d1_d2
      ,category = c(area1_label, area2_label)
      # ,lty = rep("blank", 2)
      ,fill = c("lightblue", "pink")
      ,alpha = rep(0.5, 2)
      # ,cat.pos = c(0, 0)
      # ,cat.dist = rep(0.025, 2)
      ,print.mode = c("percent","raw")
      , ...
    )
  grid.draw(plot)
  # return(plot)
}
# usage
# view_venn_two(
#   dto$record$sgs$id
#   ,"SGS"
#   ,dto$record$encounter$id
#   ,"Encounters"
#   ,print.mode = c("percent","raw")
# )




# funtion displaying Venn diagram for three categories

# functions to support basic explorations of the data model

view_venn_three <- function(
  # Dependency - VennDiagram package
  d1       # vector of values in the         first data frame
  ,d1_label # label for the variable from the first data frame
  ,d2       # vector of values in the         second data frame
  ,d2_label # label for the variable from the second data frame
  ,d3       # vector of values in the         second data frame
  ,d3_label # label for the variable from the second data frame
  ,...      # additional arguments passed to VennDiagram package
){
  # browser()
  # values for testing and development
  # d1 <- dto$record$sgs$id
  # d2 <- dto$record$encounter$id
  # d1_label = "SGS"
  # d2_label = "Encounter"

  # compute the quiantities needed for graphing
  (d1_n     = d1 %>% unique() %>% length())
  (d2_n     = d2 %>% unique() %>% length())
  (d3_n     = d3 %>% unique() %>% length())

  (intersect_d1_d2  <- dplyr::intersect(d1,d2)    %>% length())
  (intersect_d1_d3  <- dplyr::intersect(d1,d3)    %>% length())
  (intersect_d2_d3  <- dplyr::intersect(d2,d3)    %>% length())
  # (intersect_d1_d2_d3  <- dplyr::intersect(d1, d2, d3)    %>% length())
  # (intersect_d1_d2_d3  <- base::intersect(base::intersect(d1, d2), d3)    %>% length())
  (intersect_d1_d2_d3  <- base::Reduce(base::intersect, list(d1,d2,d3))  %>% length())


  # Format labels
  area1_label <- paste0(d1_label,"\n(n = ", d1_n," )" )
  area2_label <- paste0(d2_label,"\n(n = ", d2_n," )" )
  area3_label <- paste0(d3_label,"\n(n = ", d3_n," )" )

  # grid.newpage()
  plot <-
    VennDiagram:: draw.triple.venn(
      area1 = d1_n
      ,area2 = d2_n
      ,area3 = d3_n

      ,n12 = intersect_d1_d2
      ,n23 = intersect_d2_d3
      ,n13 = intersect_d1_d3
      ,n123 = intersect_d1_d2_d3
      ,category = c(area1_label, area2_label, area3_label)
      ,category        = c('A', 'B', 'C')
      ,fill            = c('red', 'blue', 'green')
      ,cat.col         = c('red', 'blue', 'green')
      ,cex             = c(1/2,2/2,3/2,4/2,5/2,6/2,7/2)
      ,cat.cex         = c(1,2,3)
      # ,euler           = TRUE
      ,scaled          = FALSE
      ,print.mode = c("percent","raw")
      ,...
    )
  return(plot)

}
# usage
# view_venn_two(
#   dto$record$sgs$id
#   ,"SGS"
#   ,dto$record$encounter$id
#   ,"Encounters"
#   ,print.mode = c("percent","raw")
# )


# # quick demo
# A <- c(1,2,3          )
# B <- c(    3,4,5,6    )
# C <- c(1,2,3,4,5,6,7,8)
#
# (N <- setdiff(C, c(A,B)))
#
# view_venn_two(A,"A",B,"B", euler.d =T) %>% grid::grid.draw()
# view_venn_two(N, "N", c(A,B),"A+B", euler.d =T)
