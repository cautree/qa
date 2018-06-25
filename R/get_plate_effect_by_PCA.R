
#' A get_plate_effect_by_PCA function
#'
#' This function allows you to see plate effect using PCA.
#' @param input_df, input data frame.
#' @return a r baseplot scatter plot graph
#' @export
#'

get_plate_effect_by_PCA= function( input_df) {

  sample_df = input_df %>%
    dplyr::filter(!is.na(subjectId)) %>%
    dplyr::select(-subjectId, -year)

  rownames(sample_df) = sample_df$plate_well

  sample_df$plate_well = NULL


  # replace NA with 0.25 of min
  NA2mean <- function(x) replace(x, is.na(x), min(x, na.rm = TRUE)*0.25)
  sample_df[]<- lapply(sample_df, NA2mean)


  plates = sapply (input_df$plate_well, function(x) {strsplit(x, "_")[[1]][[1]]} )
  plate_count =length(unique(plates))

  list= paste("000", (1:plate_count),"_", sep="")



  p_vector =sapply(list, function(x)  length(rownames(sample_df)[grep(x, rownames(sample_df))])  )

  group_name = unlist(sapply( 1:plate_count, function(x){   rep(x, p_vector[x])}))


  plate = as.factor(group_name)

  pca = prcomp(sample_df, center = TRUE, scale = TRUE)


  scores <- data.frame(group_name, pca$x[,1:3])
  pc1.2 <- ggplot2::qplot(x=PC1, y=PC2, data=scores, colour=plate)
  pc1.3 <- ggplot2::qplot(x=PC1, y=PC3, data=scores, colour=plate)
  pc2.3 <- ggplot2::qplot(x=PC2, y=PC3, data=scores, colour=plate)


  g=gridExtra::grid.arrange(pc1.2,pc1.3,pc2.3 )

  ggplot2::ggsave("PCA plate effect.png",
         g, width = 15, height = 30, units = "cm")


}
