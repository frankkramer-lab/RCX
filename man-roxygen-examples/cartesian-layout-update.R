## For CartesianLayoutAspects: 
## prepare some aspects:
cartesianLayout = createCartesianLayout(
  node=c(0, 1),
  x=c(5.5, 110.1), 
  y=c(200.3, 210.2),
  z=c(-1, 3.1),
)

## node 0 is updated, new view is added 
cartesianLayout2 = createCartesianLayout(
  node=c(0, 0),
  x=c(5.7, 7.2), 
  y=c(98, 13.9),
  view=c(NA, 1476)
)

## Simply update with new values
cartesianLayout3 = updateCartesianLayout(cartesianLayout, cartesianLayout2)

## Ignore already present keys
cartesianLayout3 = updateCartesianLayout(cartesianLayout, cartesianLayout2, 
                                         replace=FALSE)

## Raise an error if duplicate keys are present
try(updateCartesianLayout(cartesianLayout, cartesianLayout2, 
                          stopOnDuplicates=TRUE))
## =>ERROR: 
## Provided IDs (node, view) countain duplicates!

## For RCX:
## prepare RCX object:
nodes = createNodes(name = c("a","b"))
edges = createEdges(source = 0, target = 1)
cySubNetworks = createCySubNetworks(
  id = 1476,
  nodes = "all",
  edges = "all"                    
)
rcx = createRCX(nodes,
                edges = edges,
                cySubNetworks=cySubNetworks)

## add the network attributes
rcx = updateCartesianLayout(rcx, cartesianLayout)

## add additional network attributes and update existing
rcx = updateCartesianLayout(rcx, cartesianLayout2)
