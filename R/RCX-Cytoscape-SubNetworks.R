##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   cySubNetworks
##########################################################################################
# property |options          |values  |description
# ---------|-----------------|--------|------------------------------------------------
# <a id="subnetworkid">"id"</a>|Required, Unique |integer |CX/Cytoscape internal id
# "nodes"  |Optional         |list of integer or "all" |edges making up this subnetwork, reference to CX internal [node id "id"](#nodeid)
# "edges"  |Optional         |list of integer or "all" |nodes making up this subnetwork, reference to CX internal [node id "id"](#edgeid)
##########################################################################################


#' Cytoscape subnetworks
#' 
#' This function is used to create Cytoscape subnetwork aspects.
#' 
#' @details 
#' Cytoscape contributes aspects that organize subnetworks, attribute tables, and visual attributes for use by its own layout and 
#' analysis tools. Furthermore are the aspects used in web-based visualizations like within the NDEx platform.
#' 
#' A group is defined by its unique *id*, which must be an (positive) integer, which serves as reference to other aspects.
#' If no IDs are provided, they are created automatically. 
#' 
#' Nodes and edges are referred by the IDs of the corresponding aspect. Unlike other aspects referring those IDs, the Cytoscape
#' subnetwork aspect allows to refer to all nodes and edges using the keyword `all`.
#' 
#' The relationship between (sub-)networks and views, and also the type (subnetwork or view) is defined in \code{\link{CyNetworkRelations}}.
#'
#' @param id integer; subnetwork IDs
#' @param nodes integer; reference to [node id][Nodes] OR character "all" to refer to all nodes
#' @param edges integer; reference to [edge id][Edges] OR character "all" to refer to all edges
#'
#' @return *`r .CLS$cySubNetworks`* object
#' @export
#' @seealso \code{\link{updateCySubNetworks}};
#' @name CySubNetworks
#'
#' @example man-roxygen-examples/cy-subnetworks-create.R
createCySubNetworks = function(id, nodes=NULL, edges=NULL){
    fname="createCySubNetworks"
    if(!is.list(nodes)) nodes = list(nodes)
    if(!is.list(edges)) edges = list(edges)
    
    if(missing(id) || is.null(id)){
        if( is.null(nodes) &&  is.null(edges)){
            id = 0
        }else{
            if(is.null(nodes)){
                id = seq(0, (length(edges) -1))
            }else{
                id = seq(0, (length(nodes) -1))
            }
        }
    }
    
    .checkIsUniqueId(id, "id", fname)
    
    .checkAnyNotNull(c("nodes", "edges"), fname, nodes, edges)
    
    if(! is.null(nodes)){
        ## nodes must be a list of integers or a list of "all"
        .checkAllNumericOrInDict(nodes, "SN", "nodes", fname)
    }
    if(! is.null(edges)){
        ## edges must be a list of integers or a list of "all"
        .checkAllNumericOrInDict(edges, "SN", "nodes", fname)
    }
    
    .checkSameLength(fname,
                     id, nodes, edges)
    
    cySubNetworks = data.frame('id'=id,
                               nodes=NA,
                               edges=NA,
                               stringsAsFactors=FALSE, check.names=FALSE)
    
    if(is.null(nodes)){
        cySubNetworks$nodes = NULL
    }else{
        cySubNetworks$nodes = nodes
    }
    
    if(is.null(edges)){
        cySubNetworks$edges = NULL
    }else{
        cySubNetworks$edges = edges
    }
    
    .addClass(cySubNetworks) = .CLS$cySubNetworks
    return(cySubNetworks)
}


#' Update Cytoscape subnetworks
#' 
#' This functions add subnetworks in the form of a \code{\link{CySubNetworks}} object to an other \code{\link{CySubNetworks}} or an [RCX][RCX-object] object.
#' 
#' @details 
#' Cytoscape subnetworks allow to group a set of nodes and corresponding edges together.
#' \code{\link{CySubNetworks}} objects can be added to an [RCX][RCX-object] or an other \code{\link{CySubNetworks}} object.
#' The *nodes* and *edges* parameters reference the node or edge IDs that belong to a subnetwork. 
#' When adding an \code{\link{CySubNetworks}} object to an [RCX][RCX-object] object, those IDs must be present in the 
#' \code{\link{Nodes}} or \code{\link{Edges}} aspect respectively, otherwise an error is raised. 
#' Unlike other aspects referring those IDs, the Cytoscape subnetwork aspect allows to refer to all nodes and edges using the keyword `all`.
#' 
#' When subnetworks should be added to a \code{\link{CySubNetworks}} or a [RCX][RCX-object] object some conflicts may rise, since the 
#' aspects might use the same IDs. If the aspects do not share any IDs, the two aspects are simply combined.
#' Otherwise, the IDs of the new subnetworks are re-assinged continuing with the next available ID 
#' (i.e. \code{\link{maxId}}(cySubNetworks) + 1 and \code{\link{maxId}}(rcx$cySubNetworks) + 1, respectively). 
#' 
#' To keep track of the changes, it is possible to keep the old IDs of the newly added nodes in the automatically added column *oldId*.
#' This can be omitted by setting *keepOldIds* to `FALSE`.
#' Otherwise, if a re-assignment of the IDs is not desired, this can be prevented by setting *stopOnDuplicates* to `TRUE`.
#' This forces the function to stop and raise an error, if duplicated IDs are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{CySubNetworks}} object; (to which the new subnetworks will be added)
#' @param cySubNetworks \code{\link{CySubNetworks}} object; (the subnetwork, that will be added)
#' @param ... additional parameters
#' @param stopOnDuplicates logical; whether to stop, if duplicates in *id* column are found, or re-assign ids instead.
#' @param keepOldIds logical; if ids are re-assigned, the original ids are kept in the column *oldId*
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{CySubNetworks}} or [RCX][RCX-object] object with added subnetworks
#' @seealso \code{\link{CyNetworkRelations}};
#' @export
#' @example man-roxygen-examples/cy-subnetworks-update.R
updateCySubNetworks = function(x, cySubNetworks, stopOnDuplicates=FALSE, keepOldIds=TRUE, ...){
    UseMethod("updateCySubNetworks", x)
}


#' @rdname updateCySubNetworks
#' @export
updateCySubNetworks.CySubNetworksAspect = function(x, cySubNetworks, stopOnDuplicates=FALSE, keepOldIds=TRUE, ...){
    cySubNetworksOld = x
    fname="updateCySubNetworks"
    if(missing(cySubNetworks)) .stop("paramMissing", "x")
    if(missing(cySubNetworks)) .stop("paramMissing", "cySubNetworks")
    .checkClass(cySubNetworks, .CLS$cySubNetworks, "x", fname)
    .checkClass(cySubNetworks, .CLS$cySubNetworks, "cySubNetworks", fname)
    
    aspect = .mergeIdAspect(cySubNetworksOld, cySubNetworks, 
                            idProperty(cySubNetworksOld), idProperty(cySubNetworks), 
                            stopOnDuplicates, keepOldIds,
                            fname)
    .addClass(aspect) = .CLS$cySubNetworks
    return(aspect)
}


#' @rdname updateCySubNetworks
#' @export
updateCySubNetworks.RCX = function(x, cySubNetworks, stopOnDuplicates=FALSE, keepOldIds=TRUE, checkReferences=TRUE, ...){
    rcx = x
    fname="addCySubNetworks"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(cySubNetworks)) .stop("paramMissing", .CLS$cySubNetworks)
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(cySubNetworks, .CLS$cySubNetworks, "cySubNetworks", fname)
    
    if((! is.null(cySubNetworks$nodes)) && (checkReferences)){
        .checkRefPresent(rcx, "nodes", .CLS$nodes, "rcx$nodes", fname)
        ids = unique(unlist(cySubNetworks$nodes))
        ids = ids[!is.na(ids)]
        ids = ids[!ids %in% .DICT$SN]
        .checkRefs(ids, rcx$nodes$id, c("cySubNetworks$nodes", "rcx$nodes$id"), fname)
    }
    
    if((! is.null(cySubNetworks$edges)) && (checkReferences)){
        .checkRefPresent(rcx, "edges", .CLS$edges, "rcx$edges", fname)
        ids = unique(unlist(cySubNetworks$edges))
        ids = ids[!is.na(ids)]
        ids = ids[!ids %in% .DICT$SN]
        .checkRefs(ids, rcx$edges$id, c("cySubNetworks$edges", "rcx$edges$id"), fname)
    }
    
    if(! is.null(rcx$cySubNetworks)){
        cySubNetworks = updateCySubNetworks(rcx$cySubNetworks, cySubNetworks, stopOnDuplicates, keepOldIds)
    }
    
    rcx$cySubNetworks = cySubNetworks
    rcx = updateMetaData(rcx)
    
    return(rcx)
}

