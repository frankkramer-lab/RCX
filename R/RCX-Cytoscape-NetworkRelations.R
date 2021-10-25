##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   cyNetworkRelations
##########################################################################################
# property          |options          |values          |description
# ------------------|-----------------|----------------|------------------------------------------------
# "p"               |Optional         |integer                   |parent network id (see [Cytoscape subnetworks](#subnetworkid))
# "c"               |Required         |integer                   |child network
# "r"               |Optional         |string: "view", "subnetwork"|relationship type, default "subnetwork"
# "name"            |Optional         |string                    |the name of the child network; if missing, default is reader-dependent
##########################################################################################


#' Cytoscape network relations
#' 
#' This function is used to create Cytoscape network relations aspects.
#' 
#' @details
#' Cytoscape contributes aspects that organize subnetworks, attribute tables, and visual attributes for use by its own layout and
#' analysis tools. Furthermore are the aspects used in web-based visualizations like within the NDEx platform.
#' 
#' Cytoscape network relations define the relationship between the main network, subnetworks and views and also a name can be 
#' assigned to the relationship.
#' Both, subnetworks and views are defined as [subnetworks][CySubNetworks] aspect, but their type is defined here by the *isView* property.
#' The parent of a subnetwork or view can be an other subnetwork or the root network.
#' 
#' @param child integer; reference to [subnetwork id][CySubNetworks]
#' @param parent integer (optional); reference to [subnetwork id][CySubNetworks], but left blank (or `NA`) for root-network
#' @param name character (optional); name of the subnetwork or view
#' @param isView logical (optional); `TRUE` for views, else the network defines a subnetwork
#' 
#' @return *`r .CLS$cyNetworkRelations`* object
#' @export
#' @seealso \code{\link{updateCyNetworkRelations}};
#' @name CyNetworkRelations
#' 
#' @example man-roxygen-examples/cy-network-relations-create.R
createCyNetworkRelations = function(child, parent=NULL, name=NULL, isView=F){
    fname="createCyNetworkRelations"
    if(missing(child)) .stop("paramMissing", "child is required!")
    
    .checkIsId(child, "child", fname)
    
    .checkSameLength(fname,
                     child, parent, name)
    
    aspect = data.frame(child=child,
                        isView=isView,
                        stringsAsFactors = F,
                        check.names = F)
        
    if(! is.null(parent)) {
        .checkNonNeg(parent, "parent", fname)
        aspect$parent = parent
    }
    
    if(! is.null(name)) {
        .checkCharacter(name, "name", fname)
        aspect$name = name
    }
    
    .addClass(aspect) = .CLS$cyNetworkRelations
    return(aspect)
}


#' Update Cytoscape network relations
#' 
#' This functions add network relations in the form of a \code{\link{CyNetworkRelations}} object to an other \code{\link{CyNetworkRelations}} or an
#' [RCX][RCX-object] object.
#' 
#' @details 
#' Cytoscape subnetworks allow to group a set of nodes and corresponding edges together, and network relations define the relations between those
#' networks.
#' \code{\link{CyNetworkRelations}} objects can be added to an [RCX][RCX-object] or an other \code{\link{CyNetworkRelations}} object.
#' 
#' When network relations are added to a \code{\link{CyNetworkRelations}} or a [RCX][RCX-object] object some conflicts may rise, since the aspects
#' might use the same child IDs. 
#' If the aspects do not share any child IDs, the two aspects are simply combined, otherwise, the properties of the child are updated. 
#' If that is not wanted, the updating can be prevented by setting *replace* to `FALSE`.
#' Furthermore, if duplicated properties are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated child IDs are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{CySubNetworks}} object; (to which the new network relations will be added)
#' @param cyNetworkRelations \code{\link{CySubNetworks}} object; (the network relations, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in the *child* column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{CyNetworkRelations}} or [RCX][RCX-object] object with added network relations
#' @export
#' @example man-roxygen-examples/cy-network-relations-update.R
updateCyNetworkRelations = function(x, cyNetworkRelations, replace=T, stopOnDuplicates=F, ...){
    UseMethod("updateCyNetworkRelations", x)
}


#' @rdname updateCyNetworkRelations
#' @export
updateCyNetworkRelations.CyNetworkRelationsAspect = function(x, cyNetworkRelations, replace=T, stopOnDuplicates=F, ...){
    cyNetworkRelationsOld = x
    fname="updateCyNetworkRelations"
    if(missing(cyNetworkRelationsOld)) .stop("paramMissing", "x")
    if(missing(cyNetworkRelations)) .stop("paramMissing", "cyNetworkRelations")
    .checkClass(cyNetworkRelationsOld, .CLS$cyNetworkRelations, "x", fname)
    .checkClass(cyNetworkRelations, .CLS$cyNetworkRelations, "cyNetworkRelations", fname)
    
    cyNetworkRelations = .mergeAttributesAspect(cyNetworkRelationsOld,
                                                cyNetworkRelations,
                                                replace, stopOnDuplicates,
                                                required="child",
                                                optional=c(),
                                                fname)
    .addClass(cyNetworkRelations) = .CLS$cyNetworkRelations
    
    return(cyNetworkRelations)
}


#' @rdname updateCyNetworkRelations
#' @export
updateCyNetworkRelations.RCX = function(x, cyNetworkRelations, replace=T, stopOnDuplicates=F, checkReferences=T, ...){
    rcx = x
    fname="addCyNetworkRelations"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(cyNetworkRelations)) .stop("paramMissing", "cyNetworkRelations")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(cyNetworkRelations, .CLS$cyNetworkRelations, "cyNetworkRelations", fname)
    
    if((! is.null(cyNetworkRelations$child)) && (checkReferences)){
        .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
        ids = unique(unlist(cyNetworkRelations$child))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$cySubNetworks$id, c("cyNetworkRelations$child", "rcx$cySubNetworks$id"), fname)
    }
    
    if((! is.null(cyNetworkRelations$parent)) && (checkReferences)){
        .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
        ids = unique(unlist(cyNetworkRelations$parent))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$cySubNetworks$id, c("cyNetworkRelations$parent", "rcx$cySubNetworks$id"), fname)
    }
    
    ## Merge aspects if already present in RCX object
    if(! is.null(rcx$cyNetworkRelations)){
        cyNetworkRelations = updateCyNetworkRelations(rcx$cyNetworkRelations,
                                                      cyNetworkRelations,
                                                      replace, stopOnDuplicates)
    }
    rcx$cyNetworkRelations = cyNetworkRelations
    rcx = updateMetaData(rcx)
    
    return(rcx)
}
