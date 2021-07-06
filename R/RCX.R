#' R package implementing the Cytoscape Exchange (CX) format
#'
#' \tabular{ll}{
#' Package: \tab RCX\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.7\cr
#' Date: \tab 2016-12-02\cr
#' License: \tab TBD\cr
#' }
#'
#' @author Florian Auer \email{florian.auer@informatik.uni-augsburg.de}
#' @name RCX-package
#' @aliases RCX
#' @docType package
#' @title RCX and RCXgraph data structure package
#' @keywords package
#' @examples
#' \dontrun{
#' require(RCX)
#' ###TODO: add some sample code
#' }
NULL

#TODO: validate: metaData aspect
#TODO: validate: check node/edge/networkAttributes subnetworkIds
#TODO: validate: view, but which id??
#TODO: rcx to rcxgraph: include the node/edge attributes differently, but leave untouched in rcx; the lists in attributes cause an error in rcy3
#TODO: diff on RCX: diff(rcx, by=c(<aspectNames>)
#TODO: list aspects: listAspects(rcx, verbose)



#TODO: add addRCX to combine two networks. That needs a diff of two aspects!
#' Create an RCX objects from aspects
#' 
#' ddd
#' 
#' \code{vignette("CX-Data", package = "RCX")}
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
                     checkReferences=T){
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
