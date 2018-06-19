



#' A median_vs_running_order function
#'
#' This function allows you to get median vs running order
#' @param input_df, input data frame.
#' @return a ggplot scatter graph
#' @export

get_median_vs_running_order = function(df_input) {

  df_sample = df_input %>%
    dplyr::select(-year, -subjectId)

  row_median = apply(df_sample, 1, median, na.rm = TRUE)

  df_sample$plate_well = rownames(df_sample)

  df_sample = df_sample %>%
    tidyr::separate(plate_well, c("plate", "well"))

  df_sample$order = 1:dim(df_sample)[1]
  df_sample$row_median = row_median


  df_sample %>%
    ggplot2::ggplot(aes(order, row_median, color = plate)) +
    ggplot2::geom_point()+
    ggplot2::ylab("metabolite_median") +
    ggplot2::ggtitle("metabolite_median vs analysis order of samples")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  ggsave("median_vs_running_order.pdf")

}
