
#' A make_t_SNE_graph_for_plate_effects function
#'
#' This function allows you to see plate effect.
#' @param df_input, input data frame.
#' @param is_sample, boolean, default is TRUE
#' @param df_name, name for the input data, default is "Vital"
#' @param perplexity_num, int for perplexity, default is 30
#' @return a r baseplot scatter plot graph
#' @export
#'
make_t_SNE_graph_for_plate_effects = function(df_input,is_sample=TRUE, perplexity_num=30, df_name="Vital") {


  plates = sapply (df_input$plate_well, function(x) {strsplit(x, "_")[[1]][[1]]} )
  plate_count =length(unique(plates))

  list= paste("000", (1:plate_count),"_", sep="")

  p_vector =sapply(list, function(x)  length(df_input$plate_well[grep(x, df_input$plate_well)])  )

  group_name = unlist(sapply( 1:plate_count, function(x){   rep(x, p_vector[x])}))

  df_input$plate_well =NULL


  # replace NA with 0.25 of min
  NA2mean <- function(x) replace(x, is.na(x), min(x, na.rm = TRUE)*0.25)
  df_input[]<- lapply(df_input, NA2mean)

  df_input$labels = as.factor(group_name)


  ## for plotting
  colors = rainbow(length(unique(group_name)))
  names(colors) = unique(group_name)


  ## Executing the algorithm on curated data
  tsne <- Rtsne(df_input[-dim(df_input)[2]], dims = 3, theta =0, pca=TRUE,
                initial_dims = 200, perplexity=perplexity_num, eta=100, verbose=TRUE,
                max_iter = 500 , pca_scale=TRUE)


  if(is_sample){
    pdf(paste( df_name, 'tsne for sample plate effect.pdf', sep=" " ) ) }
  else{
    pdf(paste( df_name, 'tsne for pp plate effect.pdf', sep=" " ) ) }


  layout(matrix(c(1,2,3,0,4,0), ncol=3, byrow=TRUE), heights=c(4, 1), widths=c(4,4,4), respect=T)
  par(mai=rep(0.6, 4))
  plot(tsne$Y[,1], tsne$Y[,2], t='p', col=colors[df_input$labels], pch=20)
  plot(tsne$Y[,2], tsne$Y[,3], t='p', col=colors[df_input$labels], pch=20)
  plot(tsne$Y[,1], tsne$Y[,3], t='p', col=colors[df_input$labels], pch=20)
  title(main="t-SNE Clustering of Wells by Plate", outer=T, line=-17)

  par(mai=c(0,0,0,0))
  plot.new()
  legend(x="center", legend=names(colors), title="Plates", horiz=T, pch=20, col=colors, bty="n")

  dev.off()

}
