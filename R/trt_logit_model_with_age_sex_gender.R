#' A trt_logit_model_with_age_gender  function
#'
#' This function allows you to get median vs running order comparision between sample and pp
#' @param sample_meta_data, input data frame with metabolites reading.
#' @param sample_clin_data, input data frame with clinical info.
#' @param sample_trt_data, input data frame with trt info.
#' @param trt, string showing trtment, which is a var i nthe sample trt data
#' @param df_name, string for data set
#' @return a cvs files with beta and p value for treatment effects
#' @export

trt_logit_model_with_age_gender = function(sample_delta_data, sample_trt_data, sample_clin_data, trt="fishoilactive", df_name="Vital") {


  if(! "subjectId" %in% names(sample_delta_data)){

    sample_delta_data = sample_delta_data %>%
      dplyr::rename(subjectId = plate_well)

    sample_trt_data = sample_trt_data %>%
      dplyr::rename(subjectId = plate_well)

    sample_clin_data =sample_clin_data %>%
      dplyr::rename(subjectId = plate_well)

  }



  if(!trt %in% names(sample_trt_data)){
    stop("there is no such treatment!")
  }

  if(! "gender" %in% names(sample_clin_data)){
    sample_clin_data = sample_clin_data %>%
      dplyr::rename(gender = sex)

  }


  if( race %in% names(sample_clin_data)){

  sample_clin_data_1 =sample_clin_data %>%
    dplyr::select(subjectId, age, gender, race)}

  else{
    sample_clin_data_1 =sample_clin_data %>%
      dplyr::select(subjectId, age, gender)}



  sample_trt_data = sample_trt_data %>%
    dplyr::select(subjectId, trt) %>%
    dplyr::left_join(sample_clin_data_1, by = "subjectId")



  sample_delta_data_with_trt = sample_delta_data %>%
    dplyr::left_join( sample_trt_data, by ="subjectId") %>%
    dplyr::mutate( gender = as.factor(gender),
                   race = as.factor(race)) %>%
    dplyr::select(-subjectId)

  meta_reading = "meta_reading"
  age = "age"
  gender = "gender"
  race = "race"

  if( race %in% names(sample_clin_data)){
  logit_report = sample_delta_data_with_trt %>%
    tidyr::gather(key="meta", value="meta_reading",  -trt, - age, -sex, - race) %>%
    dplyr::group_by(meta) %>%
    do(broom::tidy( glm(  paste(trt, paste(meta_reading, age, gender, race, sep="+" ), sep="~"), data= .,  family = binomial))) %>%
    dplyr::filter( term == "meta_reading") %>%
    dplyr::select(estimate, p.value, meta) %>%
    dplyr::rename( beta = estimate,
                   p_value = p.value,
                   meta_group = meta) %>%
    dplyr::select( meta_group, beta, p_value) %>%
    dplyr::arrange(p_value)
  } else{

    logit_report = sample_delta_data_with_trt %>%
      tidyr::gather(key="meta", value="meta_reading",  -trt, - age, -sex) %>%
      dplyr::group_by(meta) %>%
      do(broom::tidy( glm( paste(trt, paste(meta_reading, age, gender, sep="+" ), sep="~"), data= .,  family = binomial))) %>%
      dplyr::filter( term == "meta_reading") %>%
      dplyr::select(estimate, p.value, meta) %>%
      dplyr::rename( beta = estimate,
                     p_value = p.value,
                     meta_group = meta) %>%
      dplyr::select( meta_group, beta, p_value) %>%
      dplyr::arrange(p_value)



  }

  write.csv(logit_report, paste( df_name,trt, "logit_with_age_gender_(race)_beta_p_value.csv", sep= "_"))

}
