################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Conversion from and to Bioconductor graph (graphNEL)
################################################################################


#' Convert an RCX object from and to an graphNEL object
#' 
#' Convert an [RCX][RCX-object] object to an [graphNEL][graph::graphNEL-class] object
#' 
#' @details 
#' In the [graphNEL][graph::graphNEL-class] object the attributes are not separated from the graph like in [RCX][RCX-object].
#' Therefore, for converting an [RCX][RCX-object] object to an [graphNEL][graph::graphNEL-class] object, and back,
#' some adjustments in the naming of the attributes have to be made.
#' 
#' For nodes the `name` can be present in the [nodes][Nodes] aspect, as name in the [nodeAttributes][NodeAttributes] aspect.
#' Also `name` is used in [graphNEL][graph::graphNEL-class] for naming the vertices.
#' To avoid collisions in the conversion, the [nodes][Nodes] name is saved in [graphNEL][graph::graphNEL-class] as `nodeName`, 
#' while the [nodeAttributes][NodeAttributes] property `name` is saved as `"attribute...name"`.
#' These names are also used for the conversion back to [RCX][RCX-object], but here the `name` used in the 
#' [nodes][Nodes] aspect can be changed by the `nodeName` parameter.
#' 
#' Similar to the node name, if `"represents"` is present as property in [nodeAttributes][NodeAttributes] its name is changed to
#' `"attribute...represents"`.
#' 
#' The conversion of [edges][Edges] works analogously:
#' If `"interaction"` is present as property in [edgeAttributes][EdgeAttributes] its name is changed to `"attribute...interaction"`.
#' 
#' [Nodes] and [edges][Edges] must have IDs in the [RCX][RCX-object], but not in the [graphNEL][graph::graphNEL-class] object.
#' To define an [vertex][graph::nodeData] or [edge][graph::edgeData] attribute to be used as ID, the parameters
#' `nodeId` and `edgeId` can be used to define ether an attribute name (default:"id") or set it to `NULL` to generate ID automatically.
#' 
#' The attributes also may have a special data type assigned.
#' The data type then is saved by adding `"...dataType"` to the attribute name.
#' 
#' The [cartesian layout][CartesianLayout] is also stored in the [graphNEL][graph::graphNEL-class] object.
#' To make those [graph vertex attributes][graph::nodeData()] distinguishable from [nodeAttributes][NodeAttributes]
#' they are named `"cartesianLayout...x"`, `"cartesianLayout...y"` and `"cartesianLayout...z"`.
#' 
#' In the [RCX][RCX-object] attributes it is also possible to define a [subnetwork][CySubNetworks], to which an attribute applies.
#' Those attributes are added with `"...123"` added to its name, where `"123"` is the [subnetwork id][CySubNetworks].
#' The [subnetwork id][CySubNetworks] itself are added as graph graph attributes, and are named
#' `subnetwork...123...nodes"` and `"subnetwork...123...edges"`, where `"123"` is the [subnetwork id][CySubNetworks].
#' 
#' Altogether, the conventions look as follows:
#' `"[attribute...]<name>[...<subnetwork>][...dataType]"`
#' 
#'
#' @param rcx [RCX][RCX-object] object
#' @param directed logical; whether the graph is directed
#'
#' @return [graphNEL][graph::graphNEL-class] or [RCX][RCX-object] object
#' @export
#' @seealso [Igraph], [igraph::as_graphnel()]
#' 
#' @name graphNEL
#'
#' @example man-roxygen-examples/CX_load.R
#' @example man-roxygen-examples/graphNEL.R
toGraphNEL = function(rcx, directed=FALSE){
    fname = "toGraphNEL"
    if(missing(rcx)) .stop("paramMissingRCX")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    
    aspects = names(rcx)
    if(! "edges" %in% aspects) .stop("graphNELEdgesRequired", fname)
    
    graph = toIgraph(rcx, directed)
    
    igraph::vertex_attr(graph, "name") = as.character(seq_len(length(igraph::vertex_attr(graph, "id"))))
    res = igraph::as_graphnel(graph)
    
    return(res)
}


#' @rdname graphNEL
#' @export
#'
#' @param graphNEL [graphNEL][graph::graphNEL-class] object
#' @param nodeId character; igraph attribute name used for [node][Nodes] ids
#' @param nodeName character; igraph attribute name used for [node][Nodes] names
#' @param nodeIgnore character; igraph attribute names that should be ignored
#' @param edgeId character; igraph attribute name used for [edge][Edges] ids
#' @param edgeInteraction character; igraph attribute name used for [edge][Edges] interaction
#' @param edgeIgnore character; igraph attribute names that should be ignored
#' @param suppressWarning logical; whether to suppress a warning message, if the validation of the [RCX][RCX-object] object fails
fromGraphNEL = function(graphNEL, 
                        nodeId="id", nodeName="nodeName", nodeIgnore=c("name"), 
                        edgeId="id", edgeInteraction="edgeInteraction", edgeIgnore=c(),
                        suppressWarning=FALSE){
    fname = "fromGraphNEL"
    if(! "graphNEL" %in% class(graphNEL)) .stop("wrongClass",c(.formatLog("graphNEL", fname), "graphNEL"))
    
    ig = igraph::graph_from_graphnel(graphNEL)
    
    rcx = fromIgraph(ig,
                     nodeId="id", nodeName="nodeName", nodeIgnore=c("name"), 
                     edgeId="id", edgeInteraction="edgeInteraction", edgeIgnore=c(),
                     suppressWarning=FALSE)
    
    if((! validate(rcx, FALSE)) && (! suppressWarning)) warning("RCX object didn't validate!")
    return(rcx)
}

