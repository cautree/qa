
#' A make_t_SNE_graph_for_plate_effects function
#'
#' This function allows you to see plate effect.
#' @param input_df, input data frame.
#' @return a r baseplot scatter plot graph
#' @export
#'
make_t_SNE_graph_for_plate_effects = function(input_df) {

  rownames(input_df) = input_df$plate_well

  sample_df = input_df %>%
    dplyr::select(-year, -subjectId, -plate_well)


  sample_df_t = as.data.frame(t(sample_df))

  list= c("Eico_m","FFA","BA","FAH", "Other")

  p_vector =sapply(list, function(x)  length(rownames(sample_df_t)[grep(x, rownames(sample_df_t))])  )

  group_name = unlist(mapply(rep, list, p_vector))

  sample_df_t$labels = as.factor(group_name)

  plates = sapply (input_df$plate_well, function(x) {strsplit(x, "_")[[1]][[1]]} )
  plate_count =length(unique(plates))

  list= paste("000", (1:plate_count),"_", sep="")

  p_vector =sapply(list, function(x)  length(rownames(sample_df)[grep(x, rownames(sample_df))])  )

  group_name = unlist(sapply( 1:plate_count, function(x){   rep(x, p_vector[x])}))


  sample_df$labels = as.factor(group_name)


  ## for plotting
  colors = rainbow(length(unique(group_name)))
  names(colors) = unique(group_name)

  ## Executing the algorithm on curated data
  tsne <- Rtsne(sample_df[-dim(sample_df)[2]], dims = 3, theta =0, pca=TRUE,
                initial_dims = 200, perplexity=30, eta=100, verbose=TRUE,
                max_iter = 500 , pca_scale=TRUE)


  par(mfrow=c(3,1))

  pdf('tsne for plate effect.pdf')
  plot(tsne$Y[,1], tsne$Y[,2], t='n', main="tsne for plate effect, dim1 vs dim2(each label is a well)")
  text(tsne$Y[,1], tsne$Y[,2], labels=sample_df$labels, col=colors[sample_df$labels])



  plot(tsne$Y[,2], tsne$Y[,3], t='n', main="tsne for plate effect, dim2 vs dim3(each label is a well)")
  text(tsne$Y[,2], tsne$Y[,3], labels=sample_df$labels, col=colors[sample_df$labels])


  plot(tsne$Y[,1], tsne$Y[,3], t='n', main="tsne for plate effect, dim1 vs dim3(each label is a well)")
  text(tsne$Y[,1], tsne$Y[,3], labels=sample_df$labels, col=colors[sample_df$labels])


  dev.off()


}
