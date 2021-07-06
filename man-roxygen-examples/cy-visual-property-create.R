## Prepare used properties
## Visual property: Properties
vpPropertyNamedValue = c(NODE_BORDER_STROKE="SOLID", 
                         NODE_BORDER_WIDTH="1.5")
vpPropertyP = createCyVisualPropertyProperties(vpPropertyNamedValue)

## Visual property: Dependencies
vpDependencyNamedValue = c(nodeSizeLocked="false", 
                           arrowColorMatchesEdge="true")
vpPropertyD = createCyVisualPropertyDependencies(vpDependencyNamedValue)

## Visual property: Mappings
vpMappingNamedType = c(NODE_FILL_COLOR="CONTINUOUS",
                       EDGE_TARGET_ARROW_SHAPE="DISCRETE")
vpMappingDefinition = c("COL=gal1RGexp,T=double,...",
                        "COL=directed,T=boolean,K=0=true,V=0=ARROW")
vpPropertyM = createCyVisualPropertyMappings(vpMappingNamedType, 
                                             vpMappingDefinition)

## Create visual property object 
createCyVisualProperty(properties=vpPropertyP, 
                       dependencies=vpPropertyD, 
                       mappings=vpPropertyM)

## Create visual property object with different subnetworks
createCyVisualProperty(properties=list(vpPropertyP, 
                                       vpPropertyP), 
                       dependencies=list(vpPropertyD,
                                         NA),
                       mappings=list(NA,
                                     vpPropertyM),
                       appliesTo = c(NA,
                                     1),
                       view = c(1,
                                NA))
