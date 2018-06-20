#' A get_missingness_vs_running_order function
#'
#' This function allows you to get see the missingness of samples and pp
#' @param input_df, input data frame.
#' @return a ggplot line graph
#' @export

get_missingness_vs_running_order = function(input_df ) {

  rownames(input_df) = input_df$plate_well

  df_pp = subset(input_df, is.na(subjectId))

  df_pp = df_pp %>%
    dplyr::select(-subjectId, -year, -plate_well)

  df_pp <- df_pp[,colSums(is.na(df_pp))<nrow(df_pp)]


  sample_df = subset(input_df, !is.na(subjectId))

  sample_df = sample_df %>%
    dplyr::select(-year, -subjectId, -plate_well)

  sample_df <- sample_df[,colSums(is.na(sample_df))<nrow(sample_df)]

  common_metabolites = intersect(names(sample_df), names(df_pp))


  sample_df = sample_df[, names(sample_df) %in% common_metabolites]
  df_pp = df_pp[, names(df_pp) %in% common_metabolites]


  na_count_samples  = as.data.frame(sapply(sample_df, function(y) sum(length(which(is.na(y))))))

  names(na_count_samples) ="miss"

  na_count_samples = na_count_samples %>%
    dplyr::mutate(meta_name = rownames(.)) %>%
    dplyr::arrange(miss) %>%
    dplyr::mutate(rank= 1: dim(.)[1]) %>%
    dplyr::mutate(percentage = 100*round(miss/dim(sample_df)[1], 3)) %>%
    dplyr::mutate(subgroup = sapply(strsplit(meta_name, "_"), `[`, 1)) %>%
    dplyr::mutate(group = "sample")




  na_count_pp = as.data.frame(sapply(df_pp, function(y) sum(length(which(is.na(y))))))

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
    ggplot2::ggplot(aes(rank, miss, color= subgroup)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("SAMPLE rank vs number of missing and missing percentage") +
    ggplot2::scale_y_continuous(name = expression("missing percentage out of total"),
                                sec.axis = sec_axis(~ . * 1 / dim(sample_df)[1] ,
                                                    name = "missing percentage out of total"), limits = c(0, dim(sample_df)[1]))


  ggplot2::ggsave("sample missingness.pdf")

  na_count_pp %>%
    ggplot(aes(rank, miss, color= subgroup)) +
    geom_point() +
    ggtitle("pp rank vs number of missing and missing percentage") +
    scale_y_continuous(name = expression("missing percentage out of total"),
                       sec.axis = sec_axis(~ . * 1 / dim(df_pp)[1] ,
                                           name = "missing percentage out of total"), limits = c(0, dim(df_pp)[1]))

  ggplot2::ggsave("pp missingness.pdf")



}
