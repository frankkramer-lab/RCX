#' R package implementing the Cytoscape Exchange (CX) format
#'
#' Create, handle, validate, visualize and convert networks in the Cytoscape exchange (CX) format to standard data types and objects. 
#' 
#' The CX format is also used by the NDEx platform, a online commons for biological networks, and the network visualization software Cytocape.
#'
#' \code{browseVignettes("RCy3")}
#'
#' @author Florian Auer \email{florian.auer@informatik.uni-augsburg.de}
#' @name RCX
#' @docType package
NULL


#' Create an RCX object from aspects
#' 
#' An RCX object consists of several aspects, but at least one node in the [nodes][Nodes] aspect.
#' The network can either created by creating every single aspect first and the create the network
#' with all aspects present, or by creating the aspect only with the nodes and adding the remaining 
#' aspects one by one.
#' 
#' \code{vignette("01. RCX - an R package implementing the Cytoscape Exchange (CX) format", package = "RCX")}
#' \code{vignette("02. Creating RCX from scratch", package = "RCX")}
#' \code{vignette("Appendix: The RCX and CX Data Model", package = "RCX")}
#'
#' @param nodes \code{\link{Nodes}} aspect;
#' @param edges \code{\link{Edges}} aspect (optional);
#' @param nodeAttributes \code{\link{NodeAttributes}} aspect (optional);
#' @param edgeAttributes \code{\link{EdgeAttributes}} aspect (optional);
#' @param networkAttributes \code{\link{NetworkAttributes}} aspect (optional);
#' @param cartesianLayout \code{\link{CartesianLayout}} aspect (optional);
#' @param cyGroups \code{\link{CyGroups}} aspect (optional);
#' @param cyVisualProperties \code{\link{CyVisualProperties}} aspect (optional);
#' @param cyHiddenAttributes \code{\link{CyHiddenAttributes}} aspect (optional);
#' @param cyNetworkRelations \code{\link{CyNetworkRelations}} aspect (optional);
#' @param cySubNetworks \code{\link{CySubNetworks}} aspect (optional);
#' @param cyTableColumn \code{\link{CyTableColumn}} aspect (optional);
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#'
#' @return `r .CLS$rcx` object
#' @name RCX-object
#' @export
#'
#' @example man-roxygen-examples/rcx-create.R
createRCX = function(nodes, edges, nodeAttributes, edgeAttributes, networkAttributes, cartesianLayout, 
                     cyGroups, cyVisualProperties, cyHiddenAttributes, cyNetworkRelations, cySubNetworks, cyTableColumn,
                     checkReferences=TRUE){
  fname = "createRCX"
  if(missing(nodes)) .stop("paramMissing", "nodes is required (i.e. at least one node)!")
  .checkClass(nodes, .CLS$nodes, "nodes", fname)
  
  rcx <- list()
  .addClass(rcx) = .CLS$rcx
  
  rcx = updateNodes(rcx, nodes)
  
  if((! missing(edges)) && (! is.null(edges))){
    .checkClass(edges, .CLS$edges, "edges", fname)
    .checkRefs(edges$source, nodes$id, c("edges$source", "nodes$id"), fname)
    .checkRefs(edges$target, nodes$id, c("edges$target", "nodes$id"), fname)
    rcx = updateEdges(rcx, edges, checkReferences=checkReferences)
  }
  
  if((! missing(cySubNetworks)) && (! is.null(cySubNetworks))){
    .checkClass(cySubNetworks, .CLS$cySubNetworks, "cySubNetworks", fname)
    rcx = updateCySubNetworks(rcx, cySubNetworks, checkReferences=checkReferences)
  }
  
  if((! missing(nodeAttributes)) && (! is.null(nodeAttributes))){
    .checkClass(nodeAttributes, .CLS$nodeAttributes, "nodeAttributes", fname)
    pos = unique(nodeAttributes$propertyOf)
    .checkRefs(pos, nodes$id, c("nodeAttributes$propertyOf", "nodes$id"), fname)
    rcx = updateNodeAttributes(rcx, nodeAttributes, checkReferences=checkReferences)
  }
  
  if((! missing(edgeAttributes)) && (! is.null(edgeAttributes))){
    .checkClass(edgeAttributes, .CLS$edgeAttributes, "edgeAttributes",fname)
    pos = unique(edgeAttributes$propertyOf)
    .checkRefs(pos, rcx$edges$id, c("edgeAttributes$propertyOf", "edges$id"), fname)
    rcx = updateEdgeAttributes(rcx, edgeAttributes, checkReferences=checkReferences)
  }
  
  if((! missing(networkAttributes)) && (! is.null(networkAttributes))){
    .checkClass(networkAttributes, .CLS$networkAttributes, "networkAttributes", fname)
    rcx = updateNetworkAttributes(rcx, networkAttributes, checkReferences=checkReferences)
  }
  
  if((! missing(cartesianLayout)) && (! is.null(cartesianLayout))){
    .checkClass(cartesianLayout, .CLS$cartesianLayout, "cartesianLayout", fname)
    clIds = unique(cartesianLayout$node)
    .checkRefs(clIds, nodes$id, c("cartesianLayout$node", "nodes$id"), fname)
    rcx = updateCartesianLayout(rcx, cartesianLayout, checkReferences=checkReferences)
  }
  
  if((! missing(cyGroups)) && (! is.null(cyGroups))){
    .checkClass(cyGroups, .CLS$cyGroups, "cyGroups", fname)
    rcx = updateCyGroups(rcx, cyGroups, checkReferences=checkReferences)
  }
  
  if((! missing(cyVisualProperties)) && (! is.null(cyVisualProperties))){
    .checkClass(cyVisualProperties, .CLS$cyVisualProperties, "cyVisualProperties", fname)
    rcx = updateCyVisualProperties(rcx, cyVisualProperties, checkReferences=checkReferences)
  }
  
  if((! missing(cyHiddenAttributes)) && (! is.null(cyHiddenAttributes))){
    .checkClass(cyHiddenAttributes, .CLS$cyHiddenAttributes, "cyHiddenAttributes", fname)
    rcx = updateCyHiddenAttributes(rcx, cyHiddenAttributes, checkReferences=checkReferences)
  }
  
  if((! missing(cyNetworkRelations)) && (! is.null(cyNetworkRelations))){
    .checkClass(cyNetworkRelations, .CLS$cyNetworkRelations, "cyNetworkRelations", fname)
    rcx = updateCyNetworkRelations(rcx, cyNetworkRelations, checkReferences=checkReferences)
  }
  
  if((! missing(cyTableColumn)) && (! is.null(cyTableColumn))){
    .checkClass(cyTableColumn, .CLS$cyTableColumn, "cyTableColumn", fname)
    rcx = updateCyTableColumn(rcx, cyTableColumn, checkReferences=checkReferences)
  }
  
  rcx = updateMetaData(rcx)
  return(rcx)
}
