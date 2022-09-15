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

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph,1)
