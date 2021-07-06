## Using a named vector
vpDependencyNamedValue = c(nodeSizeLocked="false", 
                           arrowColorMatchesEdge="true")
createCyVisualPropertyDependencies(vpDependencyNamedValue)

## Using two separate vectors
vpDependencyName = c("nodeSizeLocked", 
                     "arrowColorMatchesEdge")
vpDependencyValue = c("false", 
                      "true")
createCyVisualPropertyDependencies(vpDependencyValue, 
                                   vpDependencyName)

# Result for either:
#                    name value
# 1        nodeSizeLocked false
# 2 arrowColorMatchesEdge  true
