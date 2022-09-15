#' Dijkastra Algorithm implementation
#' @description 
#' Find shortest path of a given node to other nodes in a graph 
#' 
#' @param graph,initialNode a dataframe containing columns v1,v2,w and a starting node 
#' @return The vector containing shortest path from starting node to other nodes in a graph
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' @export
#' 
dijkstra <- function(graph, init_node)
{
  
  if (class(init_node)!="numeric")
  {
    stop("Input type is not correct")
  }
  if (colnames(graph[1])!= "v1" || colnames(graph[2])!= "v2" || colnames(graph[3])!= "w" || ncol(graph) != 3)
  {
    stop("Graph does not contain v1,v2,w format")
  }
  
  Result_Vec<-c()
  unvisited_nodes <- unique(graph[ ,1])
  if(init_node>max(unvisited_nodes))
  {
    stop("Initial node is out of range")
  }
  for(i in 1:length(unvisited_nodes)){
    Result_Vec <-  c(Result_Vec,Inf) 
  }
  names(Result_Vec) <- unvisited_nodes
  Result_Vec[init_node] <- 0   #distance of node from itself
  for(i in 1:length(unvisited_nodes))
  {
    
    DistancesToOtherNodes <- Result_Vec[names(Result_Vec) %in% unvisited_nodes]
    node <- DistancesToOtherNodes[which.min(DistancesToOtherNodes)]
    
    WeightOfEdges <- graph[which(graph[ ,1] == names(node)), ]
    
    for (i in 1:nrow(WeightOfEdges)) {
      
      if(Result_Vec[WeightOfEdges[i, 2]] > node + WeightOfEdges[i, 3]){
        Result_Vec[WeightOfEdges[i, 2]] <- node + WeightOfEdges[i, 3]
      }
    }
    
    unvisited_nodes <- unvisited_nodes[ -which(unvisited_nodes == names(node))]
  }
  vc<-c()
  for(i in 1:length(Result_Vec)){
    vc<-append(vc,unname(Result_Vec[i]))
    
  }
  return(vc)
  }


