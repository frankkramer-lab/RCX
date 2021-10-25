##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   CartesianLayout
##########################################################################################
# property |options          |values                    |description
# ---------|-----------------|--------------------------|-----------------------------------------------------------
# "node"   |Required         |integer                   |property of, reference to CX internal [node id "id"](#nodeid)
# "x"      |Required         |numeric                   |x coordinate
# "y"      |Required         |numeric                   |y coordinate
# "z"      |Optional         |numeric                   |z coordinate
# "view"   |Optional         |integer                   |view id (see [Cytoscape subnetworks](#cytoscape))
##########################################################################################


#' Cartesian layout
#' 
#' This function creates a cartesian layout aspect, that stores coordinates of nodes.
#' 
#' @details 
#' The layout of networks can be influenced by setting the [node][Nodes] position manually. 
#' While x an y coordinates are mandatory, the z coordinates are optional and can, for example, be used to define the vertical stacking order
#' of overlapping nodes.
#' 
#' Similar to Cytoscape \url{https://cytoscape.org/}, it is possible to define different views of the same network.
#' The views itself are definded in \code{\link{CySubNetworks}} and \code{\link{CyNetworkRelations}}, and only referenced by a unique
#' subnetwork id.
#'
#' @param node integer; reference to [node ids][Nodes]
#' @param x numeric; x coordinate
#' @param y numeric; y coordinate
#' @param z numeric (optional); z coordinate
#' @param view integer (optional); reference to [subnetwork id][CySubNetworks] of type view (\code{\link{CyNetworkRelations}})
#'
#' @return *`r .CLS$cartesianLayout`* object
#' @export
#' @seealso \code{\link{updateCartesianLayout}};
#' @name CartesianLayout
#'
#' @example man-roxygen-examples/cartesian-layout-create.R
createCartesianLayout = function(node, x, y, z=NULL, view=NULL){
    fname="createCartesianLayout"
    if(missing(node) || missing(x) || missing(y)) .stop("paramMissing", "node, x and y are required!")
    .checkIsId(node, "node", fname)
    .checkNumeric(x, "x", fname)
    .checkNoNa(x, "x", fname)
    .checkNumeric(y, "y", fname)
    .checkNoNa(y, "y", fname)

    .checkSameLength(fname,
                     node, x, y, z, view)

    clAspect = data.frame(node=node,
                          x=x,
                          y=y,
                          z=NA,
                          view=NA,
                          stringsAsFactors=FALSE, check.names=FALSE)

    if(is.null(z)){
        clAspect$z = NULL
    }else{
        .checkNumeric(z, "z", fname)
        clAspect$z = z
    }

    if(is.null(view)){
        clAspect$view = NULL
    }else{
        .checkNumeric(view, "view", fname)
        clAspect$view = view
    }
    .addClass(clAspect) = .CLS$cartesianLayout
    return(clAspect)
}


#' Update Cartesian Layouts
#' 
#' This functions add a cartesian layout in the form of a \code{\link{CartesianLayout}} object to an other \code{\link{CartesianLayout}} 
#' or an [RCX][RCX-object] object.
#' 
#' @details
#' Networks, or more precisely its nodes may have a cartesian layout, that is represented as \code{\link{CartesianLayout}} object.
#' \code{\link{CartesianLayout}} objects can be added to an [RCX][RCX-object] or an other \code{\link{CartesianLayout}} object.
#' 
#' In the case, that a \code{\link{CartesianLayout}} object is added to an other, or the [RCX][RCX-object] object already contains a 
#' \code{\link{CartesianLayout}} object, some attributes might be present in both. By default, the properties are updated with the values
#' of the latest one. This can prevented by setting the *replace* parameter to `FALSE`, in that case only new properties are added and 
#' the existing properties remain untouched.
#' 
#' Furthermore, if duplicated properties are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated properties are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{CartesianLayout}} object; (to which the new layout will be added)
#' @param cartesianLayout \code{\link{CartesianLayout}} object; (the layout, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in `nodes` (and `view` if present) column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return *`r .CLS$cartesianLayout`* or [RCX][RCX-object] object with added layout
#' @export
#' @example man-roxygen-examples/cartesian-layout-update.R
updateCartesianLayout = function(x, cartesianLayout, replace=T, stopOnDuplicates=F, ...){
    UseMethod("updateCartesianLayout", x)
}


#' @rdname updateCartesianLayout
#' @export
updateCartesianLayout.CartesianLayoutAspect = function(x, cartesianLayout, replace=T, stopOnDuplicates=F, ...){
    cartesianLayoutOld = x
    fname="addCartesianLayout"
    if(missing(cartesianLayoutOld)) .stop("paramMissing", "x")
    if(missing(cartesianLayout)) .stop("paramMissing", "cartesianLayout")
    .checkClass(cartesianLayoutOld, .CLS$cartesianLayout, .formatO("x",fname))
    .checkClass(cartesianLayout, .CLS$cartesianLayout, .formatO("cartesianLayout",fname))
    
    cartesianLayoutOld$markObjectSource = "old"
    cartesianLayout$markObjectSource = "new"
    keepElements = ifelse(replace,"new","old")
    
    ## fills missing columns with NAs
    cartesianLayout = plyr::rbind.fill(cartesianLayoutOld, cartesianLayout)
    
    if("view" %in% colnames(cartesianLayout)){
        cols = c("node","view")
    }else{
        cols = c("node")
    }
    
    if(! .elementsUniqueDF(cartesianLayout, cols)) {
        if(stopOnDuplicates)  .checkUniqueDF(cartesianLayout, cols, fname)
        
        mapFunction = function(x){
            if(nrow(x)>1) x = x[x$markObjectSource==keepElements,]
            return(x)
        }
        cartesianLayout = plyr::ddply(cartesianLayout,
                                            cols,
                                            mapFunction)
    }
    cartesianLayout$markObjectSource = NULL
    .addClass(cartesianLayout) = .CLS$cartesianLayout
    
    return(cartesianLayout)
}


#' @rdname updateCartesianLayout
#' @export
updateCartesianLayout.RCX = function(x, cartesianLayout, replace=T, stopOnDuplicates=F, checkReferences=T, ...){
    rcx = x
    fname="addCartesianLayout"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(cartesianLayout)) .stop("paramMissing", "cartesianLayout")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(cartesianLayout, .CLS$cartesianLayout, "cartesianLayout", fname)
    
    if(checkReferences) {
        ## Check if the provided node ids are present in the node ids
        .checkRefPresent(rcx, "nodes", .CLS$nodes, "rcx$nodes", fname)
        pos = unique(cartesianLayout$node)
        .checkRefs(pos, rcx$nodes$id, c("cartesianLayout$node", "rcx$nodes$id"), fname)
        
        ## Check if the provided view ids are present in the subnetwork aspect
        if("view" %in% colnames(cartesianLayout)){
            .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
            pos = unique(cartesianLayout$view)
            pos = pos[!is.na(pos)]
            .checkRefs(pos, rcx$cySubNetworks$id, c("cartesianLayout$view", "rcx$cySubNetworks$id"), fname)
        }
    }
    
    cartesianLayoutRCX = rcx$cartesianLayout
    if(! is.null(cartesianLayoutRCX)){
        cartesianLayout = updateCartesianLayout(cartesianLayoutRCX, cartesianLayout, replace, stopOnDuplicates)
        .addClass(cartesianLayout) = .CLS$cartesianLayout
    }
    
    rcx$cartesianLayout = cartesianLayout
    rcx = updateMetaData(rcx)
    
    return(rcx)
}

