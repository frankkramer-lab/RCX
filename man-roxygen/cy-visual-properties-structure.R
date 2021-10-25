#' @details 
#' ## Structure of Cytoscape Visual Properties
#' ```
#' CyVisualProperties
#' |---network = CyVisualProperty
#' |---nodes = CyVisualProperty
#' |---edges = CyVisualProperty
#' |---defaultNodes = CyVisualProperty
#' |---defaultEdges = CyVisualProperty
#' 
#' CyVisualProperty
#' |---properties = CyVisualPropertyProperties
#' |   |--name
#' |   |--value 
#' |---dependencies = CyVisualPropertyDependencies
#' |   |--name
#' |   |--value 
#' |---mappings = CyVisualPropertyMappings
#' |   |--name
#' |   |--type
#' |   |--definition 
#' |---appliesTo = <reference to subnetwork id>
#' |---view = <reference to subnetwork id>
#' ```
