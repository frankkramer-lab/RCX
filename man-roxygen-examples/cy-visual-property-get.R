## Visual property: Properties
vpPropertyP1 = createCyVisualPropertyProperties(c(NODE_BORDER_STROKE="SOLID"))

## Visual property: Dependencies
vpPropertyD1 = createCyVisualPropertyDependencies(c(nodeSizeLocked="false"))

## Visual property: Mappings
vpPropertyM1 = createCyVisualPropertyMappings(c(NODE_FILL_COLOR="CONTINUOUS"), 
                                              "COL=directed,T=boolean,K=0=true,V=0=ARROW")

## Create visual property object 
vpProperty = createCyVisualProperty(properties=list(vpPropertyP1,
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
                                             1))

## Get VP for no subnetwork an no view
getCyVisualProperty(vpProperty)

getCyVisualProperty(vpProperty, 
                    appliesTo = 1,
                    view = 1)
