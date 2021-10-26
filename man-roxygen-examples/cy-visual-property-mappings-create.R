## Using a named vector
vpMappingNamedType = c(NODE_FILL_COLOR="CONTINUOUS",
                       EDGE_TARGET_ARROW_SHAPE="DISCRETE")
vpMappingDefinition = c("COL=gal1RGexp,T=double,...",
                        "COL=directed,T=boolean,K=0=true,V=0=ARROW")
createCyVisualPropertyMappings(vpMappingNamedType, 
                               vpMappingDefinition)

## Using three separate vectors
vpMappingName = c("NODE_FILL_COLOR", 
                  "EDGE_TARGET_ARROW_SHAPE")
vpMappingType = c("CONTINUOUS", 
                  "DISCRETE")
createCyVisualPropertyMappings(vpMappingType, 
                               vpMappingDefinition, 
                               vpMappingName)

# Result for either:
#                      name       type                                definition
# 1         NODE_FILL_COLOR CONTINUOUS                COL=gal1RGexp,T=double,...
# 2 EDGE_TARGET_ARROW_SHAPE   DISCRETE COL=directed,T=boolean,K=0=true,V=0=ARROW
