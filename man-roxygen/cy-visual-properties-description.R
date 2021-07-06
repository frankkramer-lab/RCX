#' @details 
#' Cytoscape contributes aspects that organize subnetworks, attribute tables, and visual attributes for use by its own layout and
#' analysis tools. Furthermore are the aspects used in web-based visualizations like within the NDEx platform.
#'  
#' The visual properties aspect is the only aspect (\code{\link{CyVisualProperties}}) with a complex structure. It is composed of several sub-property classes and
#' consists of \code{\link{CyVisualProperty}} objects, that belong to, or more precisely describe one of the following network elements: 
#' *network*, *nodes*, *edges*, *defaultNodes* or *defaultEdges*.
#' 
#' A single visual property (i.e. \code{\link{CyVisualProperty}} object) organizes the information as *properties*, *dependencies* and *mappings*,
#' as well as the single values *appliesTo* and *view*, that define the subnetwork or view to which the IDs apply.
#' 
#' Properties are \code{\link{CyVisualPropertyProperties}} objects, that hold information like 
#' `"NODE_FILL_COLOR" : "#26CCC9"` or `"NODE_LABEL_TRANSPARENCY" : "255"` in a key-value like manner.
#' 
#' Dependencies are \code{\link{CyVisualPropertyDependencies}} objects, that hold information like 
#' `"nodeSizeLocked" : "false"` or `""arrowColorMatchesEdge" : "true"` in a key-value like manner.
#' 
#' Mappings are \code{\link{CyVisualPropertyMappings}} objects, that hold information as a triplet consisting of name, type and definition, like
#' `"NODE_FILL_COLOR" : "DISCRETE" : "COL=molecule_type,T=string,K=0=miRNA,V=0=#FCEC00"`,
#' `"NODE_FILL_COLOR" : "CONTINUOUS" : "COL=gal1RGexp,T=double...` or 
#' `"NODE_LABEL" : "PASSTHROUGH" : "COL=COMMON,T=string"`.
#' 
#' For further information about Cytoscape visual properties see the `Styles` topic of the official Cytoscape documentation:
#' \href{http://manual.cytoscape.org/en/stable/Styles.html}{http://manual.cytoscape.org/en/stable/Styles.html}
