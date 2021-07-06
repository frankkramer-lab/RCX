##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   cyGroups
##########################################################################################
# property          |options          |values          |description
# ------------------|-----------------|----------------|------------------------------------------------
#<a id="cygroupid">"id"</a>|Required, Unique |integer         |Cytoscape group id, starts at 0, exported to metaData!
#"n"               |Required?        |string          |name of this group
#"nodes"           |Required?        |list of integer |the nodes making up the group, reference to CX internal [node id "id"](#nodeid)
#"external_edges"  |Required?        |list of integer |the external edges making up the group, reference to CX internal [edge id "id"](#edgeid)
#"internal_edges"  |Required?        |list of integer |the internal edges making up the group, reference to CX internal [edge id "id"](#edgeid)
#"collapsed"       |Optional?        |boolean         |indicate whether the group is displayed as a single node
##########################################################################################


#' Cytoscape Groups
#' 
#' This function is used to create Cytoscape "groups" aspects.
#' 
#' @details 
#' Cytoscape contributes aspects that organize subnetworks, attribute tables, and visual attributes for use by its own layout and 
#' analysis tools. Furthermore are the aspects used in web-based visualizations like within the NDEx platform.
#' 
#' Cytoscape groups allow to group a set of \code{\link{Nodes}} and corresponding internal and external \code{\link{Edges}} together, 
#' and represent a group as a single node in the visualization.
#' A group is defined by its unique id, which must be an (positive) integer, which serves as reference to other aspects.
#' If no ids are provided, they are created automatically.
#'
#' @param id integer (optional); Cytoscape group ids
#' @param name character; names of the groups
#' @param nodes list of integers (optional); reference to [node ids][Nodes]
#' @param externalEdges list of integers (optional); the external edges making up the group; reference to [edge ids][Edges]
#' @param internalEdges list of integers (optional); the internal edges making up the group; reference to [edge ids][Edges]
#' @param collapsed logical (optional); whether the group is displayed as a single node
#'
#' @return *`r .CLS$cyGroups`* object
#' @export
#' @seealso \code{\link{updateCyGroups}};
#' @name CyGroups
#'
#' @example man-roxygen-examples/cy-groups-create.R
createCyGroups = function(id=NULL, name, nodes=NULL, externalEdges=NULL, internalEdges=NULL, collapsed=NULL){
    fname="createCyGroups"
    if(missing(name)) .stop("paramMissing", "name")
    .checkNoNa(name, "name", fname)
    
    .checkAnyNotNull(c("nodes", "externalEdges", "internalEdges"), fname,
                     nodes, externalEdges, internalEdges)
    
    if(missing(id) || is.null(id)) id = 0:(length(name) -1)
    
    .checkIsUniqueId(id, "id", fname)
    .checkCharacter(name, "name", fname)
    
    if(! is.null(nodes)){
        ## nodes must be a list of integers
        if(!is.list(nodes)) nodes = list(nodes)
        .checkAllNumeric(nodes, "nodes", fname)
    }
    if(! is.null(externalEdges)){
        ## externalEdges must be a list of integers
        if(!is.list(externalEdges)) externalEdges = list(externalEdges)
        .checkAllNumeric(externalEdges, "externalEdges", fname)
    }
    if(! is.null(internalEdges)){
        ## internalEdges must be a list of integers
        if(!is.list(internalEdges)) internalEdges = list(internalEdges)
        .checkAllNumeric(internalEdges, "internalEdges", fname)
    }
    
    .checkSameLength(fname,
                     id, name, nodes, externalEdges, internalEdges, collapsed)
    
    cyGroups = data.frame(id=id,
                          name=name,
                          nodes=1,
                          externalEdges=1,
                          internalEdges=1,
                          collapsed=F,
                          stringsAsFactors=FALSE, check.names=FALSE)
    
    if(is.null(nodes)){
        cyGroups$nodes = NULL
    }else{
        .checkList(nodes, "nodes", fname)
        cyGroups$nodes = nodes
    }
    
    if(is.null(externalEdges)){
        cyGroups$externalEdges = NULL
    }else{
        .checkList(externalEdges, "externalEdges", fname)
        cyGroups$externalEdges = externalEdges
    }
    
    if(is.null(internalEdges)){
        cyGroups$internalEdges = NULL
    }else{
        .checkList(internalEdges, "internalEdges", fname)
        cyGroups$internalEdges = internalEdges
    }
    
    if(is.null(collapsed)){
        cyGroups$collapsed = NULL
    }else{
        cyGroups$collapsed = collapsed
    }
    
    .addClass(cyGroups) = .CLS$cyGroups
    return(cyGroups)
}


#' Update Cytoscape Groups
#' 
#' This functions add Cytoscape groups in the form of a \code{\link{CyGroups}} object to an [RCX][RCX-object] or an other \code{\link{CyGroups}} object.
#' 
#' @details
#' Cytoscape groups allow to group a set of nodes and corresponding internal and external edges together, and represent a group as
#' a single node in the visualization.
#' \code{\link{CyGroups}} objects can be added to an [RCX][RCX-object] or an other \code{\link{CyGroups}} object.
#' The *nodes*, *internalEdges* and *externalEdges* parameters reference the node or edge IDs that belong to a group. 
#' When adding an \code{\link{CyGroups}} object to an [RCX][RCX-object] object, those IDs must be present in the \code{\link{Nodes}} 
#' or \code{\link{Edges}} aspect respectively, otherwise an error is raised.
#' 
#' When two groups should be added to each other some conflicts may rise, since the aspects might use the same IDs. 
#' If the aspects do not share any IDs, the two aspects are simply combined. Otherwise, the IDs of the new groups are re-assinged 
#' continuing with the next available ID (i.e. \code{\link{maxId}}(cyGroupsAspect) + 1 and \code{\link{maxId}}(rcx$cyGroups) + 1, respectively). 
#' 
#' To keep track of the changes, it is possible to keep the old IDs of the newly added nodes in the automatically added column *oldId*.
#' This can be omitted by setting *keepOldIds* to `FALSE`.
#' Otherwise, if a re-assignment of the IDs is not desired, this can be prevented by setting *stopOnDuplicates* to `TRUE`.
#' This forces the function to stop and raise an error, if duplicated IDs are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{CyGroups}} object; (to which the new Cytoscape groups will be added)
#' @param cyGroups \code{\link{CyGroups}} object; (the new aspect, that will be added)
#' @param ... additional parameters
#' @param stopOnDuplicates logical; whether to stop, if duplicates in `id` column are found, or re-assign ids instead.
#' @param keepOldIds logical; if ids are re-assigned, the original ids are kept in the column *oldId*
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{CyGroups}} or [RCX][RCX-object] object with added Cytoscape groups
#' @seealso \code{\link{CyGroups}};
#' @export
#' @example man-roxygen-examples/cy-groups-update.R
updateCyGroups = function(x, cyGroups, stopOnDuplicates=F, keepOldIds=T, ...){
    UseMethod("updateCyGroups", x)
}


#' @rdname updateCyGroups
#' @export
updateCyGroups.CyGroupsAspect = function(x, cyGroups, stopOnDuplicates=F, keepOldIds=T, ...){
    cyGroupsOld = x
    fname="updateCyGroups"
    if(missing(cyGroupsOld)) .stop("paramMissing", "x")
    if(missing(cyGroups)) .stop("paramMissing", "cyGroups")
    .checkClass(cyGroupsOld, .CLS$cyGroups, "x", fname)
    .checkClass(cyGroups, .CLS$cyGroups, "cyGroups",fname)
    
    cyGroups = .mergeIdAspect(cyGroups, cyGroups, "id", "id", stopOnDuplicates, keepOldIds, fname)
    
    ## correct lists to be not NULL but NA instead
    if("nodes" %in% colnames(cyGroups)) cyGroups$nodes[sapply(cyGroups$nodes, is.null)] = NA
    if("externalEdges" %in% colnames(cyGroups)) cyGroups$externalEdges[sapply(cyGroups$externalEdges, is.null)] = NA
    if("internalEdges" %in% colnames(cyGroups)) cyGroups$internalEdges[sapply(cyGroups$internalEdges, is.null)] = NA
        
    .addClass(cyGroups) = .CLS$cyGroups
    
    return(cyGroups)
}


#' @rdname updateCyGroups
#' @export
updateCyGroups.RCX = function(x, cyGroups, stopOnDuplicates=F, keepOldIds=T, checkReferences=T, ...){
    rcx = x
    fname="updateCyGroups"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(cyGroups)) .stop("paramMissing", "cyGroups")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(cyGroups, .CLS$cyGroups, "cyGroups", fname)
    
    if((! is.null(cyGroups$nodes)) && (checkReferences)){
        .checkRefPresent(rcx, "nodes", .CLS$nodes, "rcx$nodes", fname)
        ids = unique(unlist(cyGroups$nodes))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$nodes$id, c("cyGroups$nodes", "rcx$nodes$id"), fname)
    }
    
    if((! is.null(cyGroups$externalEdges)) && (checkReferences)){
        .checkRefPresent(rcx, "edges", .CLS$edges, "rcx$edges", fname)
        ids = unique(unlist(cyGroups$externalEdges))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$edges$id, c("cyGroups$externalEdges", "rcx$edges$id"), fname)
    }
    if((! is.null(cyGroups$internalEdges)) && (checkReferences)){
        .checkRefPresent(rcx, "edges", .CLS$edges, "rcx$edges", fname)
        ids = unique(unlist(cyGroups$internalEdges))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$edges$id, c("cyGroups$internalEdges", "rcx$edges$id"), fname)
    }
    
    cyGroups = rcx$cyGroups
    if(! is.null(cyGroups)){
        cyGroups = updateCyGroups(cyGroups, cyGroups, stopOnDuplicates, keepOldIds)
        .addClass(cyGroups) = .CLS$cyGroups
    }
    
    rcx$cyGroups = cyGroups
    rcx = updateMetaData(rcx)
    
    return(rcx)
}

