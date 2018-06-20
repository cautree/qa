

#' create get_median_vs_SD scatter plot
#'
#' This function loads a file as a dataframe, create scatter plot for median vs SD
#' @param infile Path to the input file
#' @return a ggplot scatter plot
#' @export
#'

get_median_vs_SD = function(input_df) {

  rownames(input_df) = input_df$plate_well

  df_pp = subset(input_df, is.na(subjectId))

  df_pp = df_pp %>%
    dplyr::select(-subjectId, -year, -plate_well)

  df_pp <- df_pp[,colSums(is.na(df_pp))<nrow(df_pp)]


  df_sample = subset(input_df, !is.na(subjectId))

  df_sample = df_sample %>%
    dplyr::select(-year, -subjectId, -plate_well)

  df_sample <- df_sample[,colSums(is.na(df_sample))<nrow(df_sample)]



  common = intersect(names(df_sample), names(df_pp))

  df_sample = df_sample[ , colnames(df_sample) %in%  common]

  df_pp = df_pp[ , colnames(df_pp) %in%  common]


  df_sample_t = as.data.frame(t(df_sample))

  df_pp_t = as.data.frame(t(df_pp))




  sample_df_order_M = data.matrix(df_sample_t)
  sample_df_order = transform(sample_df_order_M, SD=matrixStats::rowSds(sample_df_order_M, na.rm=TRUE))

  sample_df_order$median = matrixStats::rowMedians(as.matrix(df_sample_t), na.rm = TRUE)

  sample_df_order$group = "sample"

  meta_sd_median_df_sample = sample_df_order %>%
    dplyr::select(SD, median, group)


  pp_df_order_M = data.matrix(df_pp_t)
  pp_df_order = transform(pp_df_order_M, SD=matrixStats::rowSds(pp_df_order_M, na.rm=TRUE))

  pp_df_order$median = matrixStats::rowMedians(as.matrix(df_pp_t), na.rm = TRUE)

  pp_df_order$group = "PP"

  meta_sd_median_df_pp = pp_df_order %>%
    dplyr::select(SD, median, group)


  sd_median_df_all =rbind(meta_sd_median_df_sample, meta_sd_median_df_pp )


  eico_sd_median_df=sd_median_df_all[ grep("Eico_mzid", rownames(sd_median_df_all)), ]

  eico_sd_median_df = eico_sd_median_df %>%
    dplyr::mutate(subgroup = "eico")


  ffa_sd_median_df= sd_median_df_all[ grep("FFA_mzid", rownames(sd_median_df_all)), ]

  ffa_sd_median_df = ffa_sd_median_df %>%
    dplyr::mutate(subgroup = "ffa")

  ba_sd_median_df= sd_median_df_all[ grep("BA_mzid", rownames(sd_median_df_all)), ]

  ba_sd_median_df = ba_sd_median_df %>%
    dplyr::mutate(subgroup = "ba")

  fah_sd_median_df= sd_median_df_all[ grep("FAH_mzid", rownames(sd_median_df_all)), ]

  fah_sd_median_df = fah_sd_median_df %>%
    dplyr::mutate(subgroup = "fah")


  sd_median_df_with_subgroup = rbind(eico_sd_median_df,
                                     ffa_sd_median_df,
                                     ba_sd_median_df,
                                     fah_sd_median_df )


  sd_median_df_with_subgroup %>%
    ggplot2::ggplot(aes(median, SD, color= subgroup)) +
    ggplot2::geom_point(size=0.5)+
    ggplot2::ggtitle("all metabolites median vs SD") +
    ggplot2::facet_wrap(~group) +
    ggplot2::geom_abline(intercept = 0, slope = 0.2) +
    ggplot2::xlim(0, 500000) +
    ggplot2::ylim(0, 500000) +
    ggplot2::theme(aspect.ratio=1)

  ggplot2::ggsave("SD_vs_Median scatter plot.pdf")


}
