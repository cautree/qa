
#' A get_CV function
#'
#' This function allows you to get metabolite CV.
#' @param input_df, input data frame.
#' @return a ggplot line graph
#' @export

get_CV = function(input_df, df_name){



  pp_cv = subset(input_df, is.na(subjectId))


  if(dim(pp_cv)[1]==0){
    pp_cv = input_df
  }

  pp_cv = pp_cv %>%
    dplyr::select(-subjectId, -year, -plate_well)

  cv_res = as.data.frame(sapply(pp_cv, raster::cv))

  names(cv_res) = c("CV")

  cv_res = cv_res[order(cv_res$CV),,drop=FALSE]


  cv_res$percentage = round((1:dim(cv_res)[1])*100/dim(cv_res)[1], 4)

  write.csv( cv_res, paste(df_name, "metabolites_CV.csv", sep = "_"))

  cv_res %>%
    ggplot2::ggplot(aes(percentage, CV)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("all PP metabolites CV vs rank")+
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
    geom_text(aes( -5, 5, label = 5, vjust = -1), size = 3)+
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
    geom_text(aes(-5, 10, label = 10, vjust = -1), size = 3)+
    ggplot2::geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
    geom_text(aes(-5, 20, label = 20, vjust = -1), size = 3)+
    ggplot2::geom_hline(yintercept = 30, linetype = "dashed", color = "red")
    geom_text(aes(-5, 30, label = 30, vjust = -1), size = 3)

  ggplot2::ggsave(paste(df_name, "metabolites_CV.pdf", sep = " ") )



}
