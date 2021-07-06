## Prepare used properties
## Visual property: Properties
vpPropertyP1 = createCyVisualPropertyProperties(c(NODE_BORDER_STROKE="SOLID"))
vpPropertyP2 = createCyVisualPropertyProperties(c(NODE_BORDER_WIDTH="1.5"))
vpPropertyP3 = createCyVisualPropertyProperties(c(NODE_BORDER_WIDTH="999"))

## Visual property: Dependencies
vpPropertyD1 = createCyVisualPropertyDependencies(c(nodeSizeLocked="false"))
vpPropertyD2 = createCyVisualPropertyDependencies(c(arrowColorMatchesEdge="true"))
vpPropertyD3 = createCyVisualPropertyDependencies(c(arrowColorMatchesEdge="false"))

## Visual property: Mappings
vpPropertyM1 = createCyVisualPropertyMappings(c(NODE_FILL_COLOR="CONTINUOUS"), 
                                              "COL=directed,T=boolean,K=0=true,V=0=ARROW")
vpPropertyM2 = createCyVisualPropertyMappings(c(EDGE_TARGET_ARROW_SHAPE="DISCRETE"), 
                                              "TRIANGLE")
vpPropertyM3 = createCyVisualPropertyMappings(c(EDGE_TARGET_ARROW_SHAPE="DISCRETE"), 
                                              "NONE")

## Create visual property object 
vpProperty1 = createCyVisualProperty(properties=list(vpPropertyP1,
                                                     vpPropertyP1), 
                                     dependencies=list(vpPropertyD1,
                                                       NA), 
                                     mappings=list(vpPropertyM1,
                                                   NA),
                                     appliesTo = c(NA,
                                                   1),
                                     view = c(NA,
                                              1))
vpProperty2 = createCyVisualProperty(properties=vpPropertyP2, 
                                     dependencies=vpPropertyD2, 
                                     mappings=vpPropertyM2)
vpProperty3 = createCyVisualProperty(properties=vpPropertyP3, 
                                     dependencies=vpPropertyD3, 
                                     mappings=vpPropertyM3)

## Create a visual properties aspect
## (using the same visual property object for simplicity)
visProp1 = createCyVisualProperties(network=vpProperty1, 
                                    nodes=vpProperty1, 
                                    edges=vpProperty1, 
                                    defaultNodes=vpProperty1, 
                                    defaultEdges=vpProperty1)

visProp2 = createCyVisualProperties(network=vpProperty2, 
                                    nodes=vpProperty2, 
                                    edges=vpProperty2, 
                                    defaultNodes=vpProperty2, 
                                    defaultEdges=vpProperty2)

visProp3 = createCyVisualProperties(network=vpProperty3, 
                                    nodes=vpProperty3, 
                                    edges=vpProperty3, 
                                    defaultNodes=vpProperty3, 
                                    defaultEdges=vpProperty3)

## Adding a different visual property (Properties, Dependencies, Mappings)
## (e.g. "NODE_BORDER_WIDTH", which is not present before)
visProp4 = updateCyVisualProperties(visProp1, visProp2)

## Update a existing visual property
visProp5 = updateCyVisualProperties(visProp4, visProp3)

## Raise an error if duplicate keys are present
try(updateCyVisualProperties(visProp4, visProp3,
                             stopOnDuplicates=TRUE))
## =>ERROR:
##   Elements of name (in VisualProperties$network$properties<appliesTo=NA,view=NA>) 
##   must not contain duplicates!

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

## Adding visual properties to an RCX object
rcx = updateCyVisualProperties(rcx, visProp1)

## Adding a different visual property (Properties, Dependencies, Mappings)
## (e.g. "NODE_BORDER_WIDTH", which is not present before)
rcx = updateCyVisualProperties(rcx, visProp2)

## Update a existing visual property
rcx = updateCyVisualProperties(rcx, visProp3)

## Raise an error if duplicate keys are present
try(updateCyVisualProperties(rcx, visProp3,
                             stopOnDuplicates=TRUE))
## =>ERROR:
## Elements of "name" (in VisualProperties$network$properties<appliesTo=NA,view=NA>) 
## must not contain duplicates! 
