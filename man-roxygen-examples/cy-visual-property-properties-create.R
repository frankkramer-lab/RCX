## Using a named vector
vpPropertyNamedValue = c(NODE_BORDER_STROKE="SOLID", 
                         NODE_BORDER_WIDTH="1.5")
createCyVisualPropertyProperties(vpPropertyNamedValue)

## Using two separate vectors
vpPropertyName = c("NODE_BORDER_STROKE", 
                   "NODE_BORDER_WIDTH")
vpPropertyValue = c("SOLID", 
                    "1.5")
createCyVisualPropertyProperties(vpPropertyValue, 
                                 vpPropertyName)

# Result for either:
#                 name value
# 1 NODE_BORDER_STROKE SOLID
# 2  NODE_BORDER_WIDTH   1.5