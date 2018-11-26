
#' A make_t_SNE_graph_for_plate_effects function
#'
#' This function allows you to see plate effect.
#' @param df_input, input data frame.
#' @param is_sample, boolean
#' @param df_name, name for the input data
#' @return a r baseplot scatter plot graph
#' @export
#'
make_t_SNE_graph_for_plate_effects = function(df_input,is_sample=TRUE, df_name="Vital") {




  plates = sapply (df_input$plate_well, function(x) {strsplit(x, "_")[[1]][[1]]} )
  plate_count =length(unique(plates))

  list= paste("000", (1:plate_count),"_", sep="")

  p_vector =sapply(list, function(x)  length(df_input$plate_well[grep(x, df_input$plate_well)])  )

  group_name = unlist(sapply( 1:plate_count, function(x){   rep(x, p_vector[x])}))

  df_input$plate_well =NULL

  # replace NA with 0.25 of min
  NA2mean <- function(x) replace(x, is.na(x), min(x, na.rm = TRUE)*0.25)
  df_input<- lapply(df_input, NA2mean)


  df_input$labels = as.factor(group_name)


  ## for plotting
  colors = rainbow(length(unique(group_name)))
  names(colors) = unique(group_name)

  ## Executing the algorithm on curated data
  tsne <- Rtsne(df_input[-dim(df_input)[2]], dims = 3, theta =0, pca=TRUE,
                initial_dims = 200, perplexity=30, eta=100, verbose=TRUE,
                max_iter = 500 , pca_scale=TRUE)


  par(mfrow=c(3,1))

  pdf(paste( df_name, 'tsne for plate effect.pdf', sep=" " ) )
  plot(tsne$Y[,1], tsne$Y[,2], t='n', main="tsne for plate effect, dim1 vs dim2(each label is a well)")
  text(tsne$Y[,1], tsne$Y[,2], labels=df_input$labels, col=colors[df_input$labels])



  plot(tsne$Y[,2], tsne$Y[,3], t='n', main="tsne for plate effect, dim2 vs dim3(each label is a well)")
  text(tsne$Y[,2], tsne$Y[,3], labels=df_input$labels, col=colors[df_input$labels])


  plot(tsne$Y[,1], tsne$Y[,3], t='n', main="tsne for plate effect, dim1 vs dim3(each label is a well)")
  text(tsne$Y[,1], tsne$Y[,3], labels=df_input$labels, col=colors[df_input$labels])


  dev.off()


}
