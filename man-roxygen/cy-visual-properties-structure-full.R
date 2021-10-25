#' @details 
#' # Structure of Cytoscape Visual Properties
#' ```
#' CyVisualProperties
#' |---network = CyVisualProperty
#' |   |---properties = CyVisualPropertyProperties
#' |   |   |--name
#' |   |   |--value 
#' |   |---dependencies = CyVisualPropertyDependencies
#' |   |   |--name
#' |   |   |--value 
#' |   |---mappings = CyVisualPropertyMappings
#' |   |   |--name
#' |   |   |--type
#' |   |   |--definition 
#' |   |---appliesTo = <reference to subnetwork id>
#' |   |---view = <reference to subnetwork id>
#' |---nodes = CyVisualProperty
#' |   |---properties = CyVisualPropertyProperties
#' |   |   |--name
#' |   |   |--value 
#' |   |---dependencies = CyVisualPropertyDependencies
#' |   |   |--name
#' |   |   |--value 
#' |   |---mappings = CyVisualPropertyMappings
#' |   |   |--name
#' |   |   |--type
#' |   |   |--definition 
#' |   |---appliesTo = <reference to subnetwork id>
#' |   |---view = <reference to subnetwork id>
#' |---edges = CyVisualProperty
#' |   |---properties = CyVisualPropertyProperties
#' |   |   |--name
#' |   |   |--value 
#' |   |---dependencies = CyVisualPropertyDependencies
#' |   |   |--name
#' |   |   |--value 
#' |   |---mappings = CyVisualPropertyMappings
#' |   |   |--name
#' |   |   |--type
#' |   |   |--definition 
#' |   |---appliesTo = <reference to subnetwork id>
#' |   |---view = <reference to subnetwork id>
#' |---defaultNodes = CyVisualProperty
#' |   |---properties = CyVisualPropertyProperties
#' |   |   |--name
#' |   |   |--value 
#' |   |---dependencies = CyVisualPropertyDependencies
#' |   |   |--name
#' |   |   |--value 
#' |   |---mappings = CyVisualPropertyMappings
#' |   |   |--name
#' |   |   |--type
#' |   |   |--definition 
#' |   |---appliesTo = <reference to subnetwork id>
#' |   |---view = <reference to subnetwork id>
#' |---defaultEdges = CyVisualProperty
#'     |---properties = CyVisualPropertyProperties
#'     |   |--name
#'     |   |--value 
#'     |---dependencies = CyVisualPropertyDependencies
#'     |   |--name
#'     |   |--value 
#'     |---mappings = CyVisualPropertyMappings
#'     |   |--name
#'     |   |--type
#'     |   |--definition 
#'     |---appliesTo = <reference to subnetwork id>
#'     |---view = <reference to subnetwork id>
#' ```
