## For CyHiddenAttributesAspects: 
## prepare some aspects:
hiddenAttributes1 = createCyHiddenAttributes(
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)

## A is updated, C is new 
hiddenAttributes2 = createCyHiddenAttributes(
  name=c("A","A","C"),
  value=list("new a",
             "new a with subnetwork",
             c(1,2)),
  subnetworkId=c(NA,1,NA)
)

## Simply update with new values
hiddenAttributes3 = updateCyHiddenAttributes(hiddenAttributes1, 
                                             hiddenAttributes2)

## Ignore already present keys
hiddenAttributes3 = updateCyHiddenAttributes(hiddenAttributes1, 
                                             hiddenAttributes2, 
                                             replace=FALSE)

## Raise an error if duplicate keys are present
try(updateCyHiddenAttributes(hiddenAttributes1, hiddenAttributes2, 
                             stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "name" and "subnetworkId" (in updateCyHiddenAttributes) 
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
rcx = updateCyHiddenAttributes(rcx, hiddenAttributes1)

## add an additional relation (update with new values)
rcx = updateCyHiddenAttributes(rcx, hiddenAttributes2)

## create a relation with a not existing subnetwork...
hiddenAttributes3 = createCyHiddenAttributes(
  name="X",
  value="new x",
  subnetworkId=9
)

## ...and try to add them
try(updateCyHiddenAttributes(rcx, hiddenAttributes3))
## =>ERROR: 
## Provided IDs of "additionalAttributes$subnetworkId" (in updateCyHiddenAttributes) 
## don't exist in "rcx$cySubNetworks$id"
