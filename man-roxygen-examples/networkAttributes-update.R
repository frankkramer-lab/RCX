## For NetworkAttributesAspects: 
## prepare some aspects:
networkAttributes1 = createNetworkAttributes(
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)

## A is updated, C is new 
networkAttributes2 = createNetworkAttributes(
  name=c("A","A","C"),
  value=list("new a",
             "new a with subnetwork",
             c(1,2)),
  subnetworkId=c(NA,1,NA)
)

## Simply update with new values
networkAttributes3 = updateNetworkAttributes(networkAttributes1, networkAttributes2)

## Ignore already present keys
networkAttributes3 = updateNetworkAttributes(networkAttributes1, networkAttributes2, 
                                             replace=FALSE)

## Raise an error if duplicate keys are present
try(updateNetworkAttributes(networkAttributes1, networkAttributes2, 
                            stopOnDuplicates=TRUE))
## =>ERROR: 
## Provided IDs (name, subnetworkId) countain duplicates!

## For RCX
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

## add the network attributes
rcx = updateNetworkAttributes(rcx, networkAttributes1)

## add additional network attributes and update existing
rcx = updateNetworkAttributes(rcx, networkAttributes2)

## create a relation with a not existing subnetwork...
networkAttributes3 = createNetworkAttributes(
  name="X",
  value="new x",
  subnetworkId=9
)

## ...and try to add them
try(updateNetworkAttributes(rcx, networkAttributes3))
## =>ERROR: 
## NetworkAttributesAspect$subnetworkId IDs don't exist in CySubNetworksAspect