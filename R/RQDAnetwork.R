#' RQDAnetwork
#'
#' Build network analysis
#' @param adj adjacency matrix
#' @param group logical. Indicating whether to attempt to identify groups
#' @keywords RQDA Shiny Network Cloud
#' @export

RQDAnetwork<-function(adj,group=TRUE){
  require(igraph)
  #make network

g <- graph.adjacency(adj, weighted = T, mode = "undirected")
g <- simplify(g)
## Network Graphs
V(g)$name <- gsub(" ", "\n", V(g)$name)
if(group==FALSE){
par(mar = rep(0, 4))
plot(g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = 1,
     vertex.size = degree(g),
     vertex.label.color = "black",
     vertex.frame.color = "white",
     vertex.color = "Dodgerblue",
     edge.width = E(g)$weight * 1,
     edge.color = "darkgray"
)
}
else{
  #make groupings and then plot
comms <- spinglass.community(g, spins = 100)
par(mar = rep(0, 4))
plot(comms, g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = .7,
     vertex.size = degree(g),
     vertex.label.color = "black",
     vertex.frame.color = NA,
     edge.color = "black",
     vertex.label.family = "sanserif",
     mark.border = NA
)
}
}
