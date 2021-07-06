## For CyNetworkRelationsAspects: 
## prepare some aspects:
cyNetworkRelations1 = createCyNetworkRelations(
  child = c(1,2),
  parent = c(NA,1),
  name = c("Network A",
           "View A"),
  isView = c(FALSE, TRUE)
)

cyNetworkRelations2 = createCyNetworkRelations(
  child = 2,
  name = "View B",
  isView = TRUE
)

## update the duplicated child
cyNetworkRelations3 = updateCyNetworkRelations(cyNetworkRelations1, 
                                               cyNetworkRelations2)

## keep old child values
cyNetworkRelations3 = updateCyNetworkRelations(cyNetworkRelations1, 
                                               cyNetworkRelations2,
                                               replace=FALSE)

## Raise an error if duplicate keys are present
try(updateCyNetworkRelations(cyNetworkRelations1, 
                             cyNetworkRelations2,
                             stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "child" (in updateCyNetworkRelations) 
## must not contain duplicates!

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

## add a network relation
rcx = updateCyNetworkRelations(rcx, cyNetworkRelations1)

## add an additional relation (View A is replaced by B)
rcx = updateCyNetworkRelations(rcx, cyNetworkRelations2)

## create a relation with a not existing subnetwork...
cyNetworkRelations3 = createCyNetworkRelations(
  child = 9
)

## ...and try to add them
try(updateCyNetworkRelations(rcx, cyNetworkRelations3))
## =>ERROR: 
## Provided IDs of "additionalNetworkRelations$child" (in addCyNetworkRelations) 
## don't exist in "rcx$cySubNetworks$id"

## create a relation with a not existing parent subnetwork...
cyNetworkRelations4 = createCyNetworkRelations(
  child = 1,
  parent = 9
)

## ...and try to add them
try(updateCyNetworkRelations(rcx, cyNetworkRelations4))
## =>ERROR: 
## Provided IDs of "additionalNetworkRelations$parent" (in addCyNetworkRelations) 
## don't exist in "rcx$cySubNetworks$id"
