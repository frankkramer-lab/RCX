## Prepare used properties
## Visual property: Properties
vpPropertyP1 = createCyVisualPropertyProperties(c(NODE_BORDER_STROKE="SOLID"))
vpPropertyP2 = createCyVisualPropertyProperties(c(NODE_BORDER_WIDTH="1.5"))
vpPropertyP3 = createCyVisualPropertyProperties(c(NODE_BORDER_WIDTH="999"))

## Add two properties:
vpPropertyP4 = updateCyVisualProperty(vpPropertyP1,vpPropertyP2)
vpPropertyP4 = updateCyVisualProperty(vpPropertyP4,vpPropertyP3)

## Visual property: Dependencies
vpPropertyD1 = createCyVisualPropertyDependencies(c(nodeSizeLocked="false"))
vpPropertyD2 = createCyVisualPropertyDependencies(c(arrowColorMatchesEdge="true"))
vpPropertyD3 = createCyVisualPropertyDependencies(c(arrowColorMatchesEdge="false"))

## Add two dependencies:
vpPropertyD4 = updateCyVisualProperty(vpPropertyD1,vpPropertyD2)
vpPropertyD4 = updateCyVisualProperty(vpPropertyD4,vpPropertyD3)

## Visual property: Mappings
vpPropertyM1 = createCyVisualPropertyMappings(c(NODE_FILL_COLOR="CONTINUOUS"), 
                                              "COL=directed,T=boolean,K=0=true,V=0=ARROW")
vpPropertyM2 = createCyVisualPropertyMappings(c(EDGE_TARGET_ARROW_SHAPE="DISCRETE"), 
                                              "TRIANGLE")
vpPropertyM3 = createCyVisualPropertyMappings(c(EDGE_TARGET_ARROW_SHAPE="DISCRETE"), 
                                              "NONE")

## Add two mappings:
vpPropertyM4 = updateCyVisualProperty(vpPropertyM1,vpPropertyM2)
vpPropertyM4 = updateCyVisualProperty(vpPropertyM4,vpPropertyM3)

## Create visual property object 
vpProperty1 = createCyVisualProperty(properties=list(vpPropertyP1,
                                                     vpPropertyP1,
                                                     vpPropertyP1), 
                                     dependencies=list(vpPropertyD1,
                                                       vpPropertyD1,
                                                       NA), 
                                     mappings=list(vpPropertyM1,
                                                   NA,
                                                   vpPropertyM1),
                                     appliesTo = c(NA,
                                                   NA,
                                                   1),
                                     view = c(NA,
                                              1,
                                              NA))
vpProperty2 = createCyVisualProperty(properties=vpPropertyP2, 
                                     dependencies=vpPropertyD2, 
                                     mappings=vpPropertyM2)
vpProperty3 = createCyVisualProperty(properties=vpPropertyP3, 
                                     dependencies=vpPropertyD3, 
                                     mappings=vpPropertyM3)

## add two visual property objects
vpProperty4 = updateCyVisualProperty(vpProperty1, vpProperty2)

## update values
updateCyVisualProperty(vpProperty4, vpProperty3)

## keep old values
updateCyVisualProperty(vpProperty4, vpProperty3, 
                       replace = FALSE)

## keep old values
try(updateCyVisualProperty(vpProperty4, vpProperty3, 
                           stopOnDuplicates = TRUE))
## =>ERROR: 
## Elements of name (in properties<appliesTo=NA,view=NA>) must not contain duplicates!
