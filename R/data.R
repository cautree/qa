#' sample metabolites reading with plate_well as id
#'
#' @source modified from https://stattarget.github.io/docs/demo/
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{plate_well}{string in the "0001_0001 format, the last well for plate one will be "0001_0096}
#'  \item{Eico_mzid_383.182643_4.2555}{ a metabolites produced from the experiment, it is a combination of group, "mzid", mz and retension time}

#' }
#' @examples
#' \dontrun{
#'  sample_meta_data
#' }
"sample_meta_data"



#' pool plasma metabolites reading with plate_well as id
#'
#' @source modified from https://stattarget.github.io/docs/demo/
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{plate_well}{string in the "0001_0001 format, the last well for plate one will be "0001_0096}
#'  \item{Eico_mzid_383.182643_4.2555}{ a metabolites produced from the experiment, it is a combination of group, "mzid", mz and retension time}

#' }
#' @examples
#' \dontrun{
#'  pp_meta_data
#' }
"pp_meta_data"





#' metabolites information with plate_well as id
#'
#' @source modified from https://stattarget.github.io/docs/demo/
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{plate_well}{string in the "0001_0001 format, the last well for plate one will be "0001_0096}
#'  \item{subjectId}{ an interger}
#'  \item{year}{ 0 or 1, an interger}

#' }
#' @examples
#' \dontrun{
#'  sample_info_data
#' }
"sample_info_data"




#' metabolites information with subjectId and year as id
#'
#' @source radomn generated
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{subjectId}{an interger}
#'  \item{year}{ 0 or 1, an interger}
#'  \item{fishoilactive}{ 0 or 1, an interger}
#'  \item{vitdactive}{ 0 or 1, an interger}
#' }
#' @examples
#' \dontrun{
#'  sample_trt_data
#' }
"sample_trt_data"
