get_beta_and_p = function( index){

  meta_name = names(delta_fish_vitD)[index]

  multi.fit = lm(delta_fish_vitD[,index]~ fishoilactive, data=delta_fish_vitD)

  beta=summary(multi.fit)$coefficients[,1]

  p=summary(multi.fit)$coefficients[,4]

  res=as.data.frame(cbind(beta,p))

  res$meta_group  = meta_name

  res$group = rownames(res)
  rownames(res)=NULL
  return(res)
}





#' A get_waterfall_plots function
#'
#' This function allows you to see waterfall plots for fishoil treatment.
#' @param input_df, input data frame.
#' @return a r baseplot scatter plot graph
#' @export
#'

get_waterfall_plots = function(input_df, clinical_df) {

  input_df = input_df %>%
    dplyr::filter(!is.na(subjectId))

  #rownames(input_df) = input_df$plate_well

  input_df = input_df %>%
    dplyr::arrange(subjectId)

  clinical_df = clinical_df %>%
    dplyr::arrange(subjectId)

  df = merge(input_df, clinical_df, by = c("subjectId", "year"))


  df_baseline = df %>%
    dplyr::filter(year==0) %>%
    dplyr::select(-plate_well)

  df_year1 = df %>%
    dplyr::filter(year==1) %>%
    dplyr::select(-plate_well)

  rownames(df_baseline)  =df_baseline$subjectId

  rownames(df_year1)  =df_year1$subjectId

  common = intersect(df_baseline$subjectId, df_year1$subjectId)

  df_baseline = df_baseline[df_baseline$subjectId  %in% common,]
  df_baseline=df_baseline[order(df_baseline$subjectId),]


  df_year1 = df_year1[df_year1$subjectId %in% common,]
  rownames(df_year1) = df_year1$subjectId

  df_year1=df_year1[order(df_year1$subjectId),]

  df_reference = df_baseline %>%
    dplyr::select(fishoilactive, vitdactive)

  df_baseline = df_baseline %>%
    dplyr::select(-subjectId, -year, -fishoilactive, -vitdactive)

  df_year1 = df_year1 %>%
    dplyr::select(-subjectId, -year, -fishoilactive, -vitdactive)

  delta = df_year1- df_baseline

  delta=delta[colSums(!is.na(delta)) > 0]

  delta_fish_vitD = merge(delta, df_reference, by=0)

  rownames(delta_fish_vitD) = delta_fish_vitD$Row.names
  delta_fish_vitD$Row.names =NULL
  c_length =dim(delta_fish_vitD)[2]-2

  results=lapply(1:c_length, get_beta_and_p)

   result_df =data.frame(Reduce(rbind, results))

  result_df$logp = round(-log10(result_df$p),1)


  # length(unique(fishoil_res$meta_group))
  vitaminD_res = result_df %>%
    dplyr::filter(group == "fishoilactive") %>%
    dplyr::arrange(-beta)

  # length(unique(vitaminD_res$meta_group))
  df_eico_vitD = vitaminD_res [grep("Eico_mzid", vitaminD_res$meta_group), ]

  df_eico_vitD$subgroup = "Eico"

  df_ffa_vitD = vitaminD_res [grepl("FFA_mzid", vitaminD_res$meta_group), ]
  df_ffa_vitD$subgroup = "FFA"

  df_ba_vitD = vitaminD_res [grep("BA_mzid", vitaminD_res$meta_group), ]
  df_ba_vitD$subgroup = "BA"

  df_fah_vitD = vitaminD_res [grep("FAH_mzid", vitaminD_res$meta_group), ]
  df_fah_vitD$subgroup = "FAH"

  df_other_vitD = vitaminD_res [grep("Other_mzid", vitaminD_res$meta_group), ]
  df_other_vitD$subgroup = "Other"


  df_vitD = rbind(df_eico_vitD, df_ffa_vitD, df_ba_vitD, df_fah_vitD, df_other_vitD )



  eico_graph=df_vitD %>%
    ggplot2::ggplot(aes(x= reorder(meta_group, -beta), y=beta, fill=subgroup))+
    ggplot2::geom_bar(stat ="identity")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))+
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, size=2))+
    ggplot2::ylab("beta1 vitaminD") +
    ggplot2::xlab("Individual metabolites") +
    ggplot2::ggtitle("waterfall plot for beta1(vitaminD), linear model no interaction with logp, all metabolites") +
    ggplot2::geom_text(aes(label=logp), position=position_dodge(width=0.9), vjust=-0.25, size = 0.5)


  ggplot2::ggsave("all_metabolites_beta1(vitaminD)_no_interaction with logp2.pdf", width = 100, height = 50, units = "cm")



}




























