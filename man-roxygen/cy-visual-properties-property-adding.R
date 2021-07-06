#' @details  
#' \code{\link{CyVisualProperty}} objects differ in the sub-networks and views (\code{\link{CySubNetworks}}) they apply to, 
#' subsequently properties, dependencies and mappings are merged based on the uniqueness in those two.
#' 
#' Properties, dependencies and mappings (i.e. \code{\link{CyVisualPropertyProperties}}, \code{\link{CyVisualPropertyDependencies}} and 
#' \code{\link{CyVisualPropertyMappings}} objects) are unique in `name`. 
#' By default, the duplicate attributes are updated with the values of the latest one. 
#' This can prevented by setting the *replace* parameter to `FALSE`, in that case only new attributes are added and 
#' the existing attributes remain untouched.
#' Furthermore, if duplicated attributes are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated attributes are present.
