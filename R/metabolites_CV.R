
#' A get_CV function
#'
#' This function allows you to get metabolite CV.
#' @param pp_meta_df input data frame
#' @return a ggplot line graph
#' @export

get_CV = function(pp_meta_df, df_name="Vital"){

  input_df = pp_meta_df %>%
    dplyr::select( -plate_well )


  cv_res = as.data.frame(sapply(input_df, raster::cv))
  names(cv_res) = c("CV")
  cv_res = cv_res[order(cv_res$CV),,drop=FALSE]


  cv_res$percentage = round((1:dim(cv_res)[1])*100/dim(cv_res)[1], 4)

  write.csv( cv_res, paste(df_name, "metabolites_CV.csv", sep = "_"))

  cv_res %>%
    ggplot2::ggplot(aes(percentage, CV)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("all PP metabolites CV vs rank")+
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "red") +

    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red") +

    ggplot2::geom_hline(yintercept = 20, linetype = "dashed", color = "red") +

    ggplot2::geom_hline(yintercept = 30, linetype = "dashed", color = "red")


  ggplot2::ggsave(paste(df_name, "metabolites_CV.pdf", sep = " ") )

}
