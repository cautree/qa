#' A get_median_vs_running_order_comparision  function
#'
#' This function allows you to get median vs running order comparision between sample and pp
#' @param sample_meta_data, input data frame.
#' @param pp_meta_data, input data frame.
#' @param df_name, string for data set
#' @return a ggplot scatter graph
#' @export

get_median_vs_running_order_comparision = function(sample_meta_data, pp_meta_data, df_name="Vital") {

  df_sample <- sample_meta_data[,colSums(is.na(sample_meta_data))<nrow(sample_meta_data)]

  rownames(df_sample) =df_sample$plate_well
  df_sample$plate_well= NULL



  df_sample = as.data.frame(t(df_sample))

  row_median_s = as.data.frame( sapply(df_sample, median, na.rm = TRUE))

  names(row_median_s) ="row_median"

  row_median_df_s = row_median_s %>%
    dplyr::mutate(plate_well = rownames(.)) %>%
    tidyr::separate(plate_well, c("plate", "well"), convert=TRUE) %>%
    dplyr::mutate(order =(plate-1)*96+well )%>%
    dplyr::mutate(plate = as.factor(plate)) %>%
    dplyr::mutate(group = "sample")


  df_pp <- pp_meta_data[,colSums(is.na(pp_meta_data))<nrow(pp_meta_data)]

  rownames(df_pp) =df_pp$plate_well
  df_pp$plate_well= NULL




  df_pp = as.data.frame(t(df_pp))

  row_median_p = as.data.frame( sapply(df_pp, median, na.rm = TRUE))

  names(row_median_p) ="row_median"

  row_median_df_p = row_median_p %>%
    dplyr::mutate(plate_well = rownames(.)) %>%
    tidyr::separate(plate_well, c("plate", "well"), convert=TRUE) %>%
    dplyr::mutate(order =(plate-1)*96+well )%>%
    dplyr::mutate(plate = as.factor(plate)) %>%
    dplyr::mutate(group = "pp")



  median_df = rbind(row_median_df_s, row_median_df_p)


  median_df %>%
    ggplot2::ggplot(aes(order, row_median, color = group, shape = plate)) +
    ggplot2::geom_point()+
    ggplot2::ylab("metabolite_median") +
    ggplot2::ggtitle("metabolite_median vs analysis order")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))


  ggsave(paste(df_name, "sample_median_running_order_pp_median_running_order_comparision.pdf", sep=" ") )



}
