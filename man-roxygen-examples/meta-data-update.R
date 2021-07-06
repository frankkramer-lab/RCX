## prepare RCX object:
nodes = createNodes(name = c("a","b","c","d","e","f"))
edges = createEdges(source=c(1,2,0,0,0,2), 
                    target=c(2,3,1,2,5,4))
rcx = createRCX(nodes, edges)
cySubNetworks = createCySubNetworks(
  id = c(1,2),
  nodes = list("all", c(1,2,3)),
  edges = list("all", c(0,2))                    
)
rcx = updateCySubNetworks(rcx, cySubNetworks)

## update meta-data manually
rcx = updateMetaData(rcx)

## update meta-data with some values
rcx = updateMetaData(rcx,
                     version=c(edges="2.0"),
                     consistencyGroup=c(nodes=3),
                     properties=list(cySubNetworks=list(some="value",
                                                        another="VALUE"),
                                     edges=list(some="edge",
                                                another="EDGE")))

## remove all properties for edges
rcx = updateMetaData(rcx, properties=list(edges=list()))
