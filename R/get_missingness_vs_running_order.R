#' A get_missingness_vs_running_order function
#'
#' This function allows you to get see the missingness of samples and pp
#' @param meta_data, input data frame.
#' @return a ggplot line graph
#' @export

get_missingness_vs_running_order = function(meta_data, is_sample= TRUE, df_name="Vital" ) {

 rownames(meta_data) = meta_data$plate_well


 meta_data$plate_well = NULL


  na_count_samples  = as.data.frame(sapply(sample_df, function(y) sum(length(which(is.na(y))))))

  names(na_count_samples) ="miss"

  na_count = na_count_samples %>%
    dplyr::mutate(meta_name = rownames(.)) %>%
    dplyr::arrange(miss) %>%
    dplyr::mutate(rank= 1: dim(.)[1],
                  percentage = 100*round(miss/dim(sample_df)[1], 3),
                  subgroup = sapply(strsplit(meta_name, "_"), `[`, 1))


  na_count %>%
    ggplot2::ggplot(aes(rank, miss, color= subgroup)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("SAMPLE rank vs number of missing and missing percentage") +
    ggplot2::scale_y_continuous(name = expression("missing percentage out of total"),
                                sec.axis = sec_axis(~ . * 1 / dim(sample_df)[1] ,
                                                    name = "missing percentage out of total"), limits = c(0, dim(sample_df)[1]))



if(is_sample){


    write.csv(na_count,  paste(df_name, "sample missingness.csv", sep = "_"))

    ggplot2::ggsave(paste(df_name, "sample missingness.pdf", sep = " "))


}else{

  write.csv(na_count,  paste(df_name, "pp missingness.csv", sep = "_"))

  ggplot2::ggsave(paste(df_name, "pp missingness.pdf", sep = " "))


}


}
