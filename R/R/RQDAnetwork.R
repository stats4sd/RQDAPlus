#' RQDAnetwork
#'
#' Build network analysis
#' @param adj adjacency matrix
#' @param group logical. Indicating whether to attempt to identify groups
#' @keywords RQDA Shiny Network Cloud
#' @export

RQDAnetwork<-function(adj,group=TRUE,maxnodes=999){
  require(igraph)
  #make network


  diag(adj)<-0

  maxnodes[maxnodes>nrow(adj)]<-nrow(adj)

  adj<-adj[order(colSums(adj),decreasing = TRUE)[1:maxnodes],order(colSums(adj),decreasing = TRUE)[1:maxnodes]]


g <- graph.adjacency(adj, weighted = T, mode = "undirected")
g <- simplify(g)
## Network Graphs
V(g)$name <- gsub(" ", "\n", V(g)$name)
if(group==FALSE){
par(mar = rep(0, 4))
plot(g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = 1,
     vertex.size = 10*degree(g)/max(degree(g)),
     vertex.label.color = "black",
     vertex.frame.color = "white",
     vertex.color = "Dodgerblue",
     edge.width = 10*E(g)$weight/max(E(g)$weight),
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
     vertex.size = 10*degree(g)/max(degree(g)),
     vertex.label.color = "black",
     vertex.frame.color = NA,
     edge.color = "black",
     edge.width = 10*E(g)$weight/max(E(g)$weight),
        mark.border = NA
)
}
}
