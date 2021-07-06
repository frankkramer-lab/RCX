## Prepare used properties
## Visual property: Properties
vpPropertyP1 = createCyVisualPropertyProperties(c(NODE_BORDER_STROKE="SOLID"))

## Visual property: Dependencies
vpPropertyD1 = createCyVisualPropertyDependencies(c(nodeSizeLocked="false"))

## Visual property: Mappings
vpPropertyM1 = createCyVisualPropertyMappings(c(NODE_FILL_COLOR="CONTINUOUS"), 
                                              "COL=directed,T=boolean,K=0=true,V=0=ARROW")

## Create visual property object 
vpProperty1 = createCyVisualProperty(properties=vpPropertyP1, 
                                     dependencies=vpPropertyD1, 
                                     mappings=vpPropertyM1)

## Create a visual properties aspect
## (using the same visual property object for simplicity)
createCyVisualProperties(network=vpProperty1, 
                         nodes=vpProperty1, 
                         edges=vpProperty1, 
                         defaultNodes=vpProperty1, 
                         defaultEdges=vpProperty1)
