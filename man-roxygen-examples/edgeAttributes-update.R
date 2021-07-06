## For EdgeAttributesAspects: 
## prepare some aspects:
edgeAttributes = createEdgeAttributes(
  propertyOf=c(0,0,0,0),
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)

## A is updated, C is new 
edgeAttributes2 = createEdgeAttributes(
  propertyOf=c(0,0,0),
  name=c("A","A","C"),
  value=list("new a",
             "new a with subnetwork",
             c(1,2)),
  subnetworkId=c(NA,1,NA)
)

## Simply update with new values
edgeAttributes3 = updateEdgeAttributes(edgeAttributes, edgeAttributes2)

## Ignore already present keys
edgeAttributes3 = updateEdgeAttributes(edgeAttributes, edgeAttributes2, 
                                       replace=FALSE)

## Raise an error if duplicate keys are present
try(updateEdgeAttributes(edgeAttributes, edgeAttributes2, 
                         stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "propertyOf", "name" and "subnetworkId" (in updateEdgeAttributes)
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

## add the edge attributes
rcx = updateEdgeAttributes(rcx, edgeAttributes)

## add additional edge attributes and update existing
rcx = updateEdgeAttributes(rcx, edgeAttributes2)

## create edge attributes for a not existing edge...
edgeAttributes3 = createEdgeAttributes(propertyOf=9, 
                                       name="A", 
                                       value="a")
## ...and try to add them
try(updateEdgeAttributes(rcx, edgeAttributes3))
## =>ERROR: 
## Provided IDs of "additionalAttributes$propertyOf" (in updateEdgeAttributes)
## don't exist in "rcx$edges$id"
