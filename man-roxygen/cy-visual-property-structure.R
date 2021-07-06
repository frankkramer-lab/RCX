#' @details 
#' ## Structure of Cytoscape Visual Property
#'  ```
#' CyVisualProperty
#' ├──properties = CyVisualPropertyProperties
#' │   ├──name
#' │   └──value 
#' ├──dependencies = CyVisualPropertyDependencies
#' │   ├──name
#' │   └──value 
#' ├──mappings = CyVisualPropertyMappings
#' │   ├──name
#' │   ├──type
#' │   └──definition 
#' ├──appliesTo = <reference to subnetwork id>
#' └──view = <reference to subnetwork id>
#' ```
