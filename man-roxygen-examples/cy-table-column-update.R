## For CyTableColumnssAspects: 
## prepare some aspects:
tableColumn1 = createCyTableColumn(
  appliesTo=c("nodes","edges","networks"),
  name=c("weight","weight","collapsed"),
  dataType=c("numeric","double","logical"),
  isList=c(FALSE,FALSE,TRUE),
  subnetworkId=c(NA,NA,1)
)

## nodes is updated, networks is new
tableColumn2 = createCyTableColumn(
  appliesTo=c("nodes","networks"),
  name=c("weight","collapsed"),
  dataType=c("double","character")
)

## Simply update with new values
tableColumn3 = updateCyTableColumn(tableColumn1, tableColumn2)

## Ignore already present keys
tableColumn3 = updateCyTableColumn(tableColumn1, tableColumn2, 
                                replace=FALSE)

## Raise an error if duplicate keys are present
try(updateCyTableColumn(tableColumn1, tableColumn2, 
                     stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "appliesTo", "name" and "subnetworkId" (in updateCyTableColumn) 
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

## add a table column property
rcx = updateCyTableColumn(rcx, tableColumn1)

## add an additional property (update with new values)
rcx = updateCyTableColumn(rcx, tableColumn2)

## create a prpperty with a not existing subnetwork...
tableColumn3 = createCyTableColumn(
  appliesTo="nodes",
  name="weight",
  subnetworkId=9
)

## ...and try to add them
try(updateCyTableColumn(rcx, tableColumn3))
## =>ERROR: 
## Provided IDs of "additionalColumns$subnetworkId" (in addCyTableColumn) 
## don't exist in "rcx$cySubNetworks$id"
