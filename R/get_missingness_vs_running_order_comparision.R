#' A get_missingness_vs_running_order_comparision function
#'
#' This function allows you to get see the missingness of samples and pp
#' @param sample_meta_data, pp_meta_data, input data frame.
#' @return a ggplot line graph
#' @export

get_missingness_vs_running_order_comparision = function(sample_meta_data, pp_meta_data, df_name="Vital" ) {

  sample_df= sample_meta_data %>%
    dplyr::select(-plate_well)


  na_count_samples  = as.data.frame(sapply(sample_df, function(y) sum(length(which(is.na(y))))))

  names(na_count_samples) ="miss"

  na_count_samples = na_count_samples %>%
    dplyr::mutate(meta_name = rownames(.)) %>%
    dplyr::arrange(miss) %>%
    dplyr::mutate(rank= 1: dim(.)[1]) %>%
    dplyr::mutate(percentage = 100*round(miss/dim(sample_df)[1], 3)) %>%
    dplyr::mutate(subgroup = sapply(strsplit(meta_name, "_"), `[`, 1)) %>%
    dplyr::mutate(group = "sample")


  pp_df= pp_meta_data %>%
    dplyr::select(-plate_well)

  na_count_pp = as.data.frame(sapply(pp_df, function(y) sum(length(which(is.na(y))))))

  names(na_count_pp) ="miss"

  na_count_pp = na_count_pp %>%
    dplyr::mutate(meta_name = rownames(.)) %>%
    dplyr::arrange(miss) %>%
    dplyr::mutate(rank= 1: dim(.)[1]) %>%
    dplyr::mutate(percentage = 100*round(miss/dim(df_pp)[1], 3)) %>%
    dplyr::mutate(subgroup = sapply(strsplit(meta_name, "_"), `[`, 1)) %>%
    dplyr::mutate(group="pp")


  na_count = rbind(na_count_samples, na_count_pp)





  na_count_samples %>%
    ggplot2::ggplot(aes(rank, miss, color= subgroup, shape = group)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("rank vs number of missing and missing percentage") +
    ggplot2::scale_y_continuous(name = expression("missing percentage out of total"),
                                sec.axis = sec_axis(~ . * 1 / dim(sample_df)[1] ,
                                                    name = "missing percentage out of total"), limits = c(0, dim(sample_df)[1]))


  ggplot2::ggsave(paste(df_name, "missingness compararision between sample and pp.pdf", sep = " "))



  }





