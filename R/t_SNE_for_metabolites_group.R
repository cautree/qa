
#' A make_t_SNE_graph_for_metabolites_group function
#'
#' This function allows you to see metabolites group.
#' @param input_df, input data frame.
#' @return a r baseplot scatter plot graph
#' @export
#'
make_t_SNE_graph_for_metabolites_group = function(input_df) {

  rownames(input_df) = input_df$plate_well

  sample_df = input_df %>%
    dplyr::select(-year, -subjectId, -plate_well)


  sample_df_t = as.data.frame(t(sample_df))

  list= c("Eico_m","FFA","BA","FAH")

  p_vector =sapply(list, function(x)  length(rownames(sample_df_t)[grep(x, rownames(sample_df_t))])  )

  group_name = unlist(mapply(rep, list, p_vector))

  sample_df_t$labels = as.factor(group_name)


  ## for plotting
  colors = rainbow(length(unique(group_name)))
  names(colors) = unique(group_name)

  tsne <- Rtsne::Rtsne(sample_df_t[-dim(sample_df_t)[2]], dims = 2, theta =0, pca=TRUE,
                initial_dims = 200, perplexity=30, eta=100, verbose=TRUE,
                max_iter = 500 , pca_scale=TRUE)

  plot(tsne$Y, t='n', main="tsne for metabolites group (each label is a metabolite)")
  text(tsne$Y, labels=sample_df_t$labels, col=colors[sample_df_t$labels])




}
