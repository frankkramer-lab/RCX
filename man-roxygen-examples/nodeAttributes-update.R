## For NodeAttributesAspects: 
## prepare some aspects:
nodeAttributes1 = createNodeAttributes(
  propertyOf=c(1,1,1,1),
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)

## A is updated, C is new 
nodeAttributes2 = createNodeAttributes(
  propertyOf=c(1,1,1),
  name=c("A","A","C"),
  value=list("new a",
             "new a with subnetwork",
             c(1,2)),
  subnetworkId=c(NA,1,NA)
)

## Simply update with new values
nodeAttributes3 = updateNodeAttributes(nodeAttributes1, nodeAttributes2)

## Ignore already present keys
nodeAttributes4 = updateNodeAttributes(nodeAttributes1, nodeAttributes2, 
                                       replace=FALSE)

## Raise an error if duplicate keys are present
try(updateNodeAttributes(nodeAttributes1, nodeAttributes2, 
                         stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "propertyOf", "name" and "subnetworkId" (in addNodeAttributes)
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

## add the node attributes, even if no subnetworks are present
rcx = updateNodeAttributes(rcx, nodeAttributes1, checkReferences=FALSE)

## add the node attributes
rcx = updateNodeAttributes(rcx, nodeAttributes1)

## add additional node attributes and update existing
rcx = updateNodeAttributes(rcx, nodeAttributes2)

## create node attributes for a not existing node...
nodeAttributes3 = createNodeAttributes(propertyOf=9, 
                                       name="A", 
                                       value="a")
## ...and try to add them
try(updateNodeAttributes(rcx, nodeAttributes3))
## =>ERROR: 
## Provided IDs of "additionalAttributes$propertyOf" (in addNodeAttributes)
## don't exist in "rcx$nodes$id"
