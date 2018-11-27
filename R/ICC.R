icc_model <- function(df_g) {

  reading_bl = subset(df_g, year ==0)$reading

  reading_y1 = subset(df_g, year ==1)$reading

  m=cbind( reading_bl,reading_y1 )

  res=irr::icc(m)

  re = list(
    p= res$p.value,
    icc=res$value)
  return(re)
}






#' An ICC function
#'
#' This function allows you to get ICC.
#' @param df_input, input data frame: metabolites data
#' @param infor, input data frame: metabolites infor
#' @param is_full_set, boolean
#' @param df_name, name for the input data
#' @return a r baseplot scatter plot graph
#' @export
#'
ICC = function(meta, infor, is_full_set=TRUE, df_name="Vital") {

if(!is_full_set){
  stop("This is for full data set, for treatment, please use ICC_trt function")
}

to_keep = infor %>%
  dplyr::group_by(subjectId) %>%
  dplyr::summarise( count = n()) %>%
  dplyr::filter(count ==2) %>%
  dplyr::left_join( infor, by = "subjectId") %>%
  dplyr::select(-count)


meta_keep = meta %>%
  dplyr::right_join( to_keep, by = "plate_well") %>%
  dplyr::arrange(subjectId) %>%
  dplyr::select(-subjectId, - plate_well)

meta_keep_long = meta_keep %>%
  tidyr::gather( key = "meta", value = "reading", - year)

icc_long = meta_keep_long %>%
  dplyr::group_by(meta) %>%
  tidyr::nest()

iccs <- purrr::map(icc_long$data, icc_model)

df_res = icc_long %>%
  dplyr::mutate( iccs = purrr::map(icc_long$data, icc_model)) %>%
  dplyr::mutate( p = purrr::map_dbl(iccs, 1)) %>%
  dplyr::mutate(icc_value = purrr::map_dbl(iccs,2)) %>%
  dplyr::select(-data, - iccs) %>%
  dplyr::arrange(-icc_value)

write.csv(df_res, paste(df_name,"icc_full_data.csv", sep = " ") )

}





