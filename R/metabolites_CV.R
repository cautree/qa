
#' A get_CV function
#'
#' This function allows you to get metabolite CV.
#' @param pp_meta_df input data frame
#' @return a ggplot line graph
#' @export

get_CV = function(pp_meta_df, df_name="Vital"){
  
  input_df = pp_meta_df %>%
    dplyr::select( -plate_well )
  
  
  cv_res = as.data.frame(sapply(input_df, raster::cv))
  names(cv_res) = c("CV")
  cv_res = cv_res[order(cv_res$CV),,drop=FALSE]
  
  
  cv_res$percentage = round((1:dim(cv_res)[1])*100/dim(cv_res)[1], 4)
  
  write.csv( cv_res, paste(df_name, "metabolites_CV.csv", sep = "_"))
  
  
  p5<-round(100*sum(as.numeric(cv_res$CV<=5.0), na.rm = TRUE)/sum(!is.na(cv_res$CV), na.rm = TRUE), 0)
  p10<-round(100*sum(as.numeric(cv_res$CV<=10.0), na.rm = TRUE)/sum(!is.na(cv_res$CV), na.rm = TRUE), 0)
  p20<-round(100*sum(as.numeric(cv_res$CV<=20.0), na.rm = TRUE)/sum(!is.na(cv_res$CV), na.rm = TRUE), 0)
  p30<-round(100*sum(as.numeric(cv_res$CV<=30.0), na.rm = TRUE)/sum(!is.na(cv_res$CV), na.rm = TRUE), 0)
  
  xmax<-round(max(cv_res$CV, na.rm=T), 0)
  
  cvplot<-cv_res %>%
    ggplot2::ggplot(aes(CV, percentage))+labs(x="CV %", y="cumulative % of metabolites") + xlim(-15, xmax+15)+
    ggplot2::geom_point() +
    ggplot2::ggtitle("% of PP metabolites with CVs less than given threshold") +
    ggplot2::geom_segment(aes(x = -15, y = p5, xend = 5.0, yend = p5), linetype = "dashed", color = "blue") +
    ggplot2::geom_segment(aes(x = -15, y = p10, xend = 10.0, yend = p10), linetype = "dashed", color = "blue") +
    ggplot2::geom_segment(aes(x = -15, y = p20, xend = 20.0, yend = p20), linetype = "dashed", color = "blue") +
    ggplot2::geom_segment(aes(x = -15, y = p30, xend = 30.0, yend = p30), linetype = "dashed", color = "blue") +
    ggplot2::geom_segment(aes(x = 5.0, y = -7, xend = 5.0, yend = p5), linetype = "dashed", color = "red") +
    ggplot2::geom_segment(aes(x = 10.0, y = -7, xend = 10.0, yend = p10), linetype = "dashed", color = "red") +
    ggplot2::geom_segment(aes(x = 20.0, y = -7, xend = 20.0, yend = p20), linetype = "dashed", color = "red") +
    ggplot2::geom_segment(aes(x = 30.0, y = -7, xend = 30.0, yend = p30), linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    ggplot2::scale_y_continuous(limits=c(-7, 107), expand = c(0, 0), breaks = sort(c(seq(0, 100, length.out=5), p5, p10, p20, p30)))+ 
    ggplot2::scale_x_continuous(limits=c(-15, xmax+15), expand = c(0, 0), breaks = sort(c(seq(0, trunc(xmax/100)*100, length.out=5), 5, 10, 20, 30, trunc(xmax/100)*100, 0)))+
    
    ggplot2::ggsave(paste(df_name, "metabolites_CV.pdf", sep = " ") )
  cvplot
}
