


#' A get_CV function
#'
#' This function allows you to get metabolite CV.
#' @param input_df, input data frame.
#' @export
#' cat_function()
#'
get_CV = function(input_df){


  pp_cv = subset(input_df, is.na(subjectId))

  pp_cv = pp_cv %>%
    dplyr::select(-subjectId, -year, -plate_well)

  pp_cv_t = as.data.frame(t(pp_cv))
  meta_names = rownames(pp_cv_t)

  pp_cv_t_M = data.matrix(pp_cv_t)
  pp_cv_t = matrixStats::transform(pp_cv_t_M, SD=rowSds(pp_cv_t_M, na.rm=TRUE))

  pp_cv_t$avg = matrixStats::rowMeans(pp_cv_t_M, na.rm = TRUE)

  pp_cv_t = pp_cv_t %>%
    dplyr::mutate(CV = 100*coefficient.variation(SD, avg)) %>%
    dplyr::mutate(ID= meta_names)

  rownames(pp_cv_t) = meta_names

  pp_cv_t = pp_cv_t %>%
    dplyr::arrange(CV) %>%
    dplyr::mutate(percentage = round((1:dim(.)[1])*100/dim(.)[1], 4))

  pp_cv_t %>%
    ggplot2::ggplot(aes(percentage, CV)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("all PP metabolites CV vs rank")+
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 30, linetype = "dashed", color = "red")

  ggplot2::ggsave("metabolites_CV.pdf")

}
