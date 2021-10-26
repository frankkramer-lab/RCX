################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


##########################################################################################
## Nodes
##########################################################################################
## property |options          |values  |description
## ---------|-----------------|--------|------------------------------------------------
## <a id="nodeid">"id"</a>|Required, Unique |integer |CX internal node id, starts at 0
## "n"      |Optional         |string  |node name, eg. "EGFR", "node 1"
## "r"      |Optional         |string  |represents, eg. "HGNC:AKT1"
## **Note:** At least one node has to be present!
##########################################################################################


#' Nodes
#' 
#' This function creates nodes for networks.
#' 
#' @details 
#' Nodes are represented by *`r .CLS$nodes`* objects.  
#' A single node is defined by its unique *id*, which must be an (positive) integer, which serves as reference to other aspects.
#' Optionally, nodes can have a name and a represents attribute. 
#' If no IDs are provided, but either names or representations (or both) IDs are assigned automatically. 
#' To be valid, a nodes aspect must contain at least one node. 
#' However, if no parameters are set (i.e. *id*, *name* and *represents* = `NULL`) there is still one node created with neither name nor representation,
#' just an ID.
#' The *`r .CLS$nodes`* is the only mandatory aspect for an \code{\link{RCX-object}}. 
#'
#' @param id integer (optional); node IDs
#' @param name character (optional); names of the nodes
#' @param represents character (optional); representation, e.g. a link to another database
#'
#' @return *`r .CLS$nodes`* object
#' @export
#' @seealso \code{\link{updateNodes}}, \code{\link{RCX-object}}
#' @name Nodes
#'
#' @example man-roxygen-examples/nodes-create.R
createNodes = function(id=NULL, name=NULL, represents=NULL){
    fname="createNodes"
    if(missing(id) || is.null(id)){
        if( is.null(name) &&  is.null(represents)){
            id = 0
        }else{
            if(is.null(name)){
                id = seq(0, (length(represents) -1))
            }else{
                id = seq(0, (length(name) -1))
            }
        }
    }
    
    .checkIsUniqueId(id, "id", fname)
    
    .checkSameLength(fname,
                     id, name, represents)
    
    nodes = data.frame(id=id,
                       name="",
                       represents="",
                       stringsAsFactors=FALSE, check.names=FALSE)
    
    if(is.null(name)){
        nodes$name = NULL
    }else{
        nodes$name = name
    }
    
    if(is.null(represents)){
        nodes$represents = NULL
    }else{
        nodes$represents = represents
    }
    
    .addClass(nodes) = .CLS$nodes
    return(nodes)
}


#' Update nodes
#' 
#' This functions add nodes in the form of a \code{\link{Nodes}} object to an other \code{\link{Nodes}} or an \code{\link{RCX-object}}.
#' 
#' @details 
#' When nodes should be added to a \code{\link{Nodes}} or a \code{\link{RCX-object}} object some conflicts may rise, since the aspects might use 
#' the same IDs. If the aspects do not share any IDs, the two aspects are simply combined. Otherwise, the IDs of the new nodes are re-assinged 
#' continuing with the next available ID (i.e. \code{\link{maxId}}(nodesAspect) + 1 and \code{\link{maxId}}(rcx$nodes) + 1, respectively). 
#' 
#' To keep track of the changes, it is possible to keep the old IDs of the newly added nodes in the automatically added column *oldId*.
#' This can be omitted by setting *keepOldIds* to `FALSE`.
#' Otherwise, if a re-assignment of the IDs is not desired, this can be prevented by setting *stopOnDuplicates* to `TRUE`.
#' This forces the function to stop and raise an error, if duplicated IDs are present.
#'
#' @param x \code{\link{RCX-object}} or \code{\link{Nodes}} object; (to which the new \code{\link{Nodes}} will be added)
#' @param nodes \code{\link{Nodes}} object; (the \code{\link{Nodes}}, that will be added)
#' @param stopOnDuplicates logical (optional); whether to stop, if duplicates in `id` column are found, or re-assign ids instead.
#' @param keepOldIds logical (optional); if ids are re-assigned, the original ids are kept in the column `oldId`
#' 
#' @return \code{\link{Nodes}} or [RCX][RCX-object] object with added nodes
#' @export
#' 
#' @example man-roxygen-examples/nodes-update.R
updateNodes = function(x, nodes, stopOnDuplicates=FALSE, keepOldIds=TRUE){
    UseMethod("updateNodes", x)
}


#' @rdname updateNodes
#' @export
updateNodes.NodesAspect = function(x, nodes, stopOnDuplicates=FALSE, keepOldIds=TRUE){
    nodesAspect = x
    fname="updateNodes"
    if(missing(nodesAspect)) .stop("paramMissing", "x")
    if(missing(nodes)) .stop("paramMissing", "nodes")
    .checkClass(nodesAspect, .CLS$nodes, "nodesAspect", fname)
    .checkClass(nodes, .CLS$nodes, "nodes", fname)
    
    aspect = .mergeIdAspect(nodesAspect, nodes, 
                            idProperty(nodesAspect), idProperty(nodes), 
                            stopOnDuplicates, keepOldIds,
                            fname)
    .addClass(aspect) = .CLS$nodes
    return(aspect)
}


#' @rdname updateNodes
#' @export
updateNodes.RCX = function(x, nodes, stopOnDuplicates=FALSE, keepOldIds=TRUE){
    rcx = x
    fname="updateNodes"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(nodes)) .stop("paramMissing", "nodes")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(nodes, .CLS$nodes, "nodes", fname)
    
    oldAspect = rcx$nodes
    if(! is.null(oldAspect)){
        nodes = updateNodes(oldAspect, nodes, stopOnDuplicates, keepOldIds)
        .addClass(nodes) = .CLS$nodes
    }
    
    rcx$nodes = nodes
    rcx = updateMetaData(rcx)
    
    return(rcx)
}


##########################################################################################
## Edges
##########################################################################################
## #property |options          |values  |description
## ---------|-----------------|--------|------------------------------------------------
## <a id="edgeid">"id"</a>|Required, Unique |integer |CX internal edge id, starts at 0
## "s"      |Required         |integer |source, reference to CX internal [node id "id"](#nodeid)
## "t"      |Required         |integer |target, reference to CX internal [node id "id"](#nodeid)
## "i"      |Optional         |string  |interaction, eg. "binds"
##########################################################################################


#' Edges
#' 
#' This function creates edges between nodes in networks.
#' 
#' @details 
#' Edges are represented by *`r .CLS$edges`* objects. 
#' Edges connect two nodes, which means that *source* and *target* must reference the IDs of nodes in a \code{\link{Nodes}} object.
#' On creation, the IDs don't matter yet, but at least while adding the *`r .CLS$edges`* object to an \code{\link{RCX-object}}, 
#' the *IDs* must be present in the nodes aspect of the \code{\link{RCX-object}}.
#' 
#' Similar to nodes, an edge also has a unique *id*, which must be an (positive) integer, which serves as reference to other aspects.
#' If no IDs are provided, those are assigned automatically. 
#' Optionally, edges can have an interaction attribute to define the type of interaction between the nodes. 
#'
#' @param id integer (optional); edge IDs
#' @param source integer; reference to [node id][Nodes]
#' @param target integer; reference to [node id][Nodes]
#' @param interaction character (optional); type of interaction, eg. "binds" or "activates"
#'
#' @return *`r .CLS$edges`* object
#' @export
#' @seealso \code{\link{updateEdges}} for adding a *`r .CLS$edges`* object to an *`r .CLS$edges`* or *`r .CLS$rcx`* object
#' @name Edges
#'
#' @example man-roxygen-examples/edges-create.R
createEdges = function(id=NULL, source, target, interaction=NULL){
    fname="createEdges"
    if(missing(source) || missing(target)) .stop("paramMissing", "source and target are required!")
    
    if(missing(id) || is.null(id)){
        id = seq(0, (length(source)-1))
    }
    
    .checkIsUniqueId(id, "id", fname)
    .checkIsId(source, "source", fname)
    .checkIsId(target, "target", fname)
    
    .checkSameLength("createEdges",
                     id, source, target, interaction)
    
    edges = data.frame(id=as.integer(id),
                       source=as.integer(source),
                       target=as.integer(target),
                       interaction="",
                       stringsAsFactors=FALSE, check.names=FALSE)
    
    if(missing(interaction) || is.null(interaction)){
        edges$interaction = NULL
    }else{
        edges$interaction = interaction
    }
    
    .addClass(edges) = .CLS$edges
    return(edges)
}

#' Update edges
#' 
#' This functions add edges in the form of a \code{\link{Edges}} object to an other \code{\link{Edges}} or an *`r .CLS$rcx`* object.
#' 
#' @details 
#' When edges should be added to a \code{\link{Edges}} or a \code{\link{RCX-object}} object some conflicts may rise, since the aspects might use 
#' the same IDs. If the aspects do not share any IDs, the two aspects are simply combined. Otherwise, the IDs of the new edges are re-assinged 
#' continuing with the next available ID (i.e. \code{\link{maxId}}(edgesAspect) + 1 and \code{\link{maxId}}(rcx$edges) + 1, respectively). 
#' 
#' To keep track of the changes, it is possible to keep the old IDs of the newly added edges in the automatically added column *oldId*.
#' This can be omitted by setting *keepOldIds* to `FALSE`.
#' Otherwise, if a re-assignment of the IDs is not desired, this can be prevented by setting *stopOnDuplicates* to `TRUE`.
#' This forces the function to stop and raise an error, if duplicated IDs are present.
#'
#' @param x \code{\link{RCX-object}} or \code{\link{Edges}} object; (to which the new \code{\link{Edges}} will be added)
#' @param edges \code{\link{Edges}} object; (the \code{\link{Edges}}, that will be added)
#' @param ... additional parameters
#' @param stopOnDuplicates logical (optional); whether to stop, if duplicates in *id* column are found, or re-assign ids instead.
#' @param keepOldIds logical (optional); if ids are re-assigned, the original ids are kept in the column *oldId*
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{Edges}} or [RCX][RCX-object] with added edges
#' @export
#' @example man-roxygen-examples/edges-update.R
updateEdges = function(x, edges, stopOnDuplicates=FALSE, keepOldIds=TRUE, ...){
    UseMethod("updateEdges", x)
}


#' @rdname updateEdges
#' @export
updateEdges.EdgesAspect = function(x, edges, stopOnDuplicates=FALSE, keepOldIds=TRUE, ...){
    edgeAspect = x
    fname="updateEdges"
    if(missing(edgeAspect)) .stop("paramMissing", "x")
    if(missing(edges)) .stop("paramMissing", "edges")
    .checkClass(edgeAspect, .CLS$edges, "edgeAspect", fname)
    .checkClass(edges, .CLS$edges, "edges", fname)
    
    aspect = .mergeIdAspect(edgeAspect, edges, 
                            idProperty(edgeAspect), idProperty(edges), 
                            stopOnDuplicates, keepOldIds,
                            fname)
    .addClass(aspect) = .CLS$edges
    return(aspect)
}



#' @rdname updateEdges
#' @export
updateEdges.RCX = function(x, edges, stopOnDuplicates=FALSE, keepOldIds=TRUE, checkReferences=TRUE, ...){
    rcx = x
    fname="updateEdges"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(edges)) .stop("paramMissing", "edgesAspect")
    .checkClass(rcx, .CLS$rcx, .formatO("rcx",fname))
    .checkClass(edges, .CLS$edges, .formatO("edges",fname))
    
    oldAspect = rcx$edges
    if(! is.null(oldAspect)){
        if(checkReferences){
            ## Check if the provided ids are present in the nodes ids
            .checkRefPresent(rcx, "nodes", .CLS$nodes, "rcx$nodes", fname)
            .checkRefs(edges$source, rcx$nodes$id, c("edges$source", "nodes$id"), fname)
            .checkRefs(edges$target, rcx$nodes$id, c("edges$target", "nodes$id"), fname)
        }
        edges = updateEdges(oldAspect, edges, stopOnDuplicates, keepOldIds)
    }
    
    rcx$edges = edges
    rcx = updateMetaData(rcx)
    
    return(rcx)
}





