
#' @export
plotInterAtt <- function(tab1, tab2, fontsize.nodes = 14, fontsize.edges = fontsize.nodes,
  alpha = 0.1)
{
  stopifnot(require(graph))
  stopifnot(require(Rgraphviz))
  
  ### filter by `alpha`
  df1 <- subset(tab1, pval < alpha)
  df2 <- subset(tab2, pval < alpha)  
  
  # case: interaction significant, but marginal effect not
  Var12 <- unique(c(df2$Var1, df2$Var2))
  df1 <- subset(tab1, pval < alpha | Var %in% Var12)
  
  ### construct graph
  g <- new("graphNEL", nodes = df1$Var, edgemode = "undirected")
  g <- addEdge(from = df2$Var1, to = df2$Var2, g)
  
  ### prepare layoutGraph
  # node shape
  nshape <- rep("box", numNodes(g))
  names(nshape) <- df1$Var 
 
  # node label
  nlabel <- paste0(df1$Var, "\n", paste0(round(100*df1$IGrel, 2), "%"), " ", df1$pval.code)
  names(nlabel) <- df1$Var

  nlwd.min <- 1
  nlwd.max <- 7
  w <- abs(df1$IGrel)
  nlwd <- (w - min(w)) / (max(w) - min(w))
  nlwd <- nlwd.min + nlwd * (nlwd.max - nlwd.min)
  names(nlwd) <- df1$Var
  
  # edge label
  elabel <- paste0(paste0(round(100*df2$IGrel, 2), "%"), " ", df2$pval.code)
  names(elabel) <- paste0(df2$Var1, "~", df2$Var2)
  
  # edge lwd
  elwd.min <- 1
  elwd.max <- 7
  w <- abs(df2$IGrel)
  elwd <- (w - min(w)) / (max(w) - min(w))
  elwd <- elwd.min + elwd * (elwd.max - elwd.min)

  names(elwd) <- paste0(df2$Var1, "~", df2$Var2)

  # edge lty
  elty <- ifelse(df2$IGrel < 0, "dotted", "solid")
  names(elty) <- paste0(df2$Var1, "~", df2$Var2)

  # layoutType = "neato"
  graph.par(list(edges = list(fontsize = fontsize.edges), nodes = list(fontsize = fontsize.nodes, lty = "solid", fill = "transparent")))
  
  edgeRenderInfo(g) <- list(lwd = elwd, lty = elty)
  nodeRenderInfo(g) <- list(lwd = nlwd)  

  g <- layoutGraph(g, 
    nodeAttrs = list(label = nlabel, shape = nshape, lwd = nlwd),
    edgeAttrs = list(label = elabel))#, layoutType = "neato")
    
  renderGraph(g)
}
