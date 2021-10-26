## create some edges
edges1 = createEdges(source=c(1,1,0), target=c(2,0,1))
edges2 = createEdges(id=c(3,2,4),
                     source=c(0,0,1), 
                     target=c(1,2,2),
                     interaction=c("activates","inhibits", NA))

## simply add the edges and keep old ids
edges3 = updateEdges(edges1, edges2)

## add the edges
edges4 = updateEdges(edges1, edges2, keepOldIds=FALSE)

## force an error because of duplicated ids
try(updateEdges(edges1, edges2, stopOnDuplicates=TRUE))
## =>Error:
## Elements of "id" (in updateEdges) must not contain duplicates!

## Prepare an RCX object
rcx = createRCX(createNodes(name = c("EGFR","AKT1","WNT")))

## add edges to the RCX object
rcx = updateEdges(rcx, edges1)

## add new edges and don't keep old ids
rcx = updateEdges(rcx, edges2, keepOldIds=FALSE)

## force an error because of duplicated ids
try(updateEdges(rcx, edges2, stopOnDuplicates=TRUE))
## =>Error:
## Elements of "id" (in updateEdges) must not contain duplicates!
