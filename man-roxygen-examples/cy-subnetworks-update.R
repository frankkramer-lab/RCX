## For CySubNetworksAspects: 
## prepare some aspects:
cySubNetworks1 = createCySubNetworks(
  id = c(0,1),
  nodes = list("all",
               c(1,2,3)),
  edges = list("all",
               c(0,2))                    
)

cySubNetworks2 = createCySubNetworks(
  nodes = c(0,3),
  edges = c(1)
)

## subnetwork ids will be kept
cySubNetworks3 = updateCySubNetworks(cySubNetworks1, cySubNetworks2)

## old subnetwork ids will be omitted
cySubNetworks3 = updateCySubNetworks(cySubNetworks1, cySubNetworks2,
                                     keepOldIds=FALSE)

## Raise an error if duplicate keys are present
try(updateCySubNetworks(cySubNetworks1, cySubNetworks2,
                        stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "id" (in updateCySubNetworks) must not contain duplicates!

## For RCX
## prepare RCX object:
nodes = createNodes(name = c("a","b","c","d","e","f"))
edges = createEdges(source=c(1,2,0,0,0,2), 
                    target=c(2,3,1,2,5,4))
rcx = createRCX(nodes, edges)

## add the subnetwork
rcx = updateCySubNetworks(rcx, cySubNetworks1)

## add additional subnetwork
rcx = updateCySubNetworks(rcx, cySubNetworks2)

## create a subnetwork with a not existing node...
cySubNetworks3 = createCySubNetworks(
  nodes = list(9)
)

## ...and try to add them
try(updateCySubNetworks(rcx, cySubNetworks3))
## =>ERROR: 
## Provided IDs of "additionalSubNetworks$nodes" (in addCySubNetworks) 
## don't exist in "rcx$nodes$id"

## create a group with a not existing edge...
cySubNetworks4 = createCySubNetworks(
  nodes = c(0,1),
  edges = 9
)

## ...and try to add them
try(updateCySubNetworks(rcx, cySubNetworks4))
## =>ERROR: 
## Provided IDs of "additionalSubNetworks$edges" (in addCySubNetworks) 
## don't exist in "rcx$edges$id"
