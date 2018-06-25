
#' A get_metabolites_group_by_PCA function
#'
#' This function allows you to see metabolites group using PCA.
#' @param input_df, input data frame.
#' @return a r baseplot scatter plot graph
#' @export
#'

get_metabolites_group_by_PCA= function( input_df) {

  sample_df = input_df %>%
    dplyr::filter(!is.na(subjectId)) %>%
    dplyr::select(-subjectId, -year)

  rownames(sample_df) = sample_df$plate_well

  sample_df$plate_well = NULL

  # replace NA with 0.25 of min
  NA2mean <- function(x) replace(x, is.na(x), min(x, na.rm = TRUE)*0.25)
  sample_df[]<- lapply(sample_df, NA2mean)


  list= c("Eico_m", "FFA", "BA","FAH", "Other")

  p_vector =sapply(list, function(x)  length(grep(x, names(sample_df)))  )

  group_name = unlist(mapply(rep, list, p_vector))

  group_name =as.factor(group_name)


  res.pca = prcomp(sample_df, center = TRUE, scale = TRUE)

  dim(sample_df)

  p1=factoextra::fviz_pca_var(res.pca,
                              axes = c(1, 2),
                              habillage = group_name,
                              geom.var = c("arrow"),
                              palette = c("#999999", "#E69F00", "red", "green", "cyan"),
                              title = "dim1 vs dim2"
  )


  p2=factoextra::fviz_pca_var(res.pca,
                              axes = c(1, 3),
                              habillage = group_name,
                              geom.var = c("arrow"),
                              palette = c("#999999", "#E69F00", "red", "green", "cyan"),
                              title = "dim1 vs dim3"
  )


  p3=factoextra::fviz_pca_var(res.pca,
                              axes = c(2, 3),
                              habillage = group_name,
                              geom.var = c("arrow"),
                              palette = c("#999999", "#E69F00", "red", "green", "cyan"),
                              title ="dim2 vs dim3"
  )

  g=gridExtra::grid.arrange(p1,p2,p3 )

  ggplot2::ggsave("PCA for fullset metabolites group.png",
                  g, width = 15, height = 30, units = "cm")






}
