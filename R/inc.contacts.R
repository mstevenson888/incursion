inc.contacts <- function(graph, nodes, order = 2, mode = "in"){
  
  requireNamespace(package = "igraph", quietly = TRUE)
  
  if(mode == "in"){ 
     vids <- igraph::neighborhood(graph = graph, order = order, nodes = nodes, mode = "in")
     rval.edg <- igraph::induced.subgraph(graph = graph, vids = unlist(vids), impl = "create_from_scratch")
     }

  if(mode == "out"){ 
    vids <- igraph::neighborhood(graph = graph, order = order, nodes = nodes, mode = "out")
    rval.edg <- igraph::induced.subgraph(graph = graph, vids = unlist(vids), impl = "create_from_scratch")
  }

  if(mode == "all"){ 
    vids <- igraph::neighborhood(graph = graph, order = order, nodes = nodes, mode = "all")
    rval.edg <- igraph::induced.subgraph(graph = graph, vids = unlist(vids), impl = "create_from_scratch")
  }  

  rval.edg
}   

