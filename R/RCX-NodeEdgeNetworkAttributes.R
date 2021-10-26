################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


##########################################################################################
## NodeAttributes
##########################################################################################
## property |options          |values                    |description
## ---------|-----------------|--------------------------|-----------------------------------------------------------
## "po"     |Required         |integer                   |property of, reference to CX internal [node id "id"](#nodeid)
## "n"      |Required         |string                    |attribute name, eg. "weight", "name", "selected"
## "v"      |Required         |string or list of strings |attribute value(s), eg. ["2", "0.34", "2.3"], "Node 6", "true"
## "d"      |Optional         |string                    |[data type](#datatypes), default "string"
## "s"      |Optional         |integer                   |subnetwork id (see [Cytoscape subnetworks](#subnetworkid))
##
## NDEx conventions:
## property            |description
## --------------------|---------------------------------------------------------------------------------------------
## "alias"             |alternative identifiers for the node. Same meaning as BioPAX "aliases"
## "relatedTo"         |identifiers denoting concepts related to the node. Same meaning as BioPAX "relatedTerms"
##########################################################################################


#' Node attributes
#' 
#' This function creates an aspect for additional attributes of nodes.
#' 
#' @details 
#' Nodes may have additional attributes besides a name and a representation. Those additional attributes reference a node by its id and
#' are defined in a key-value like manner, with the name of the attribute as key. The same attribute can also be defined for different
#' [subnetworks][CySubNetworks] with different values. 
#' The values itself may also differ in their data types, therefore it is necessary to provide the values as a list of the single values 
#' instead of a vector.
#' 
#' With *isList* it can be set, if a value should be considered as a list. This is of minor significance while working solely with 
#' [RCX][RCX-object] objects, unless it will be transformed to JSON. For some attributes it might be necessary that the values are encoded as
#' lists, even if they contain only one element (or even zero elements). To force an element to be encoded correctly, this parameter can 
#' be used, for example: `name="A", value=a, isList=T` will be encoded in JSON as `A=["a"]`.
#' 
#' @note The *propertyOf* parameter references the node ids to which the attributes belong to. When adding an `r .CLS$nodeAttributes` object
#' to an [RCX][RCX-object] object, those ids must be present in the \code{\link{Nodes}} aspect, otherwise an error is raised.
#'
#' @param propertyOf integer; reference to [node ids][Nodes]
#' @param name character; key of the attribute
#' @param value character; value of the attribute
#' @param dataType character (optional); data type of the attribute
#' @param isList logical (optional); a value should be considered as list
#' @param subnetworkId integer (optional); reference to [subnetwork id][CySubNetworks]
#'
#' @return *`r .CLS$nodeAttributes`* object
#' @seealso \code{\link{updateNodeAttributes}}, \code{\link{EdgeAttributes}}, \code{\link{NetworkAttributes}}
#' @export
#' @name NodeAttributes
#'
#' @example man-roxygen-examples/nodeAttributes-create.R
createNodeAttributes = function(propertyOf, name, value, dataType=NULL, isList=NULL, subnetworkId=NULL){
    fname="createNodeAttributes"
    if(missing(propertyOf) || missing(name) || missing(value)) .stop("paramMissing", "propertyOf, name and value are required!")
    
    .checkSameLength(fname,
                     propertyOf, name, value, dataType, isList, subnetworkId)

    nodeAttributesAspect = .createAttributeAspect(propertyOf, name, value, dataType, isList, subnetworkId, fname)

    .addClass(nodeAttributesAspect) = .CLS$nodeAttributes
    return(nodeAttributesAspect)
}


#' Update node attributes
#' 
#' This functions add node attributes in the form of a \code{\link{NodeAttributes}} object to an [RCX][RCX-object] or an other 
#' \code{\link{NodeAttributes}} object.
#' 
#' @details
#' Nodes may have additional attributes besides a name and a representation, and are represented as \code{\link{NodeAttributes}} objects.
#' \code{\link{NodeAttributes}} objects can be added to an [RCX][RCX-object] object or an other \code{\link{NodeAttributes}} object.
#' The *propertyOf* parameter references the node IDs to which the attributes belong to. When adding an \code{\link{NodeAttributes}} object
#' to an [RCX][RCX-object] object, those IDs must be present in the \code{\link{Nodes}} aspect, otherwise an error is raised.
#' 
#' In the case, that a \code{\link{NodeAttributes}} object is added to an other, or the [RCX][RCX-object] object already contains a 
#' \code{\link{NodeAttributes}} object, some attributes might be present in both. By default, the attributes are updated with the values
#' of the latest one. This can prevented setting the *replace* parameter to `FALSE`, in that case only new attributes are added and 
#' the existing attributes remain untouched.
#' 
#' Furthermore, if duplicated attributes are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated attributes are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{NodeAttributes}} object; (to which the new node attributes will be added)
#' @param nodeAttributes \code{\link{NodeAttributes}} object; (the new aspect, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in *propertyOf* and *name* (and *subnetworkId* if present) columns are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{NodeAttributes}} or [RCX][RCX-object] object with added node attributes
#' @seealso \code{\link{EdgeAttributes}}, \code{\link{NetworkAttributes}}
#' @export
#' @example man-roxygen-examples/nodeAttributes-update.R
updateNodeAttributes = function(x, nodeAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    UseMethod("updateNodeAttributes", x)
}

#' @rdname updateNodeAttributes
#' @export
updateNodeAttributes.NodeAttributesAspect = function(x, nodeAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    nodeAttr = x
    fname="addNodeAttributes"
    if(missing(nodeAttr)) .stop("paramMissing", "x")
    if(missing(nodeAttributes)) .stop("paramMissing", "nodeAttributes")
    .checkClass(nodeAttr, .CLS$nodeAttributes, "x", fname)
    .checkClass(nodeAttributes, .CLS$nodeAttributes, "nodeAttributes", fname)
    
    nodeAttributes = .mergeAttributesAspect(nodeAttr,
                                            nodeAttributes,
                                            replace, stopOnDuplicates,
                                            .log=fname)
    .addClass(nodeAttributes) = .CLS$nodeAttributes
    
    return(nodeAttributes)
}


#' @rdname updateNodeAttributes
#' @export
updateNodeAttributes.RCX = function(x, nodeAttributes, replace=TRUE, stopOnDuplicates=FALSE, checkReferences=TRUE, ...){
    rcx = x
    fname="addNodeAttributes"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(nodeAttributes)) .stop("paramMissing", "nodeAttributes")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(nodeAttributes, .CLS$nodeAttributes, "nodeAttributes", fname)

    if(checkReferences){
        ## Check if the provided propertyOf ids are present in the node ids
        .checkRefPresent(rcx, "nodes", .CLS$nodes, "rcx$nodes", fname)
        pos = unique(nodeAttributes$propertyOf)
        .checkRefs(pos, rcx$nodes$id, c("nodeAttributes$propertyOf", "rcx$nodes$id"), fname)
        
        ## Check if the provided subnetwork ids are present in the subnetwork aspect
        if("subnetworkId" %in% colnames(nodeAttributes)){
            subIds = nodeAttributes$subnetworkId
            subIds = subIds[!is.na(subIds)]
            if(length(subIds)!=0){
                .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
                pos = unique(nodeAttributes$subnetworkId)
                pos = pos[!is.na(pos)]
                .checkRefs(pos, rcx$cySubNetworks$id, c("nodeAttributes$subnetworkId", "rcx$cySubNetworks$id"), fname)
            }
        }
    }

    ## Merge aspects if already present in RCX object
    if(! is.null(rcx$nodeAttributes)){
        nodeAttributes = updateNodeAttributes(rcx$nodeAttributes,
                                                    nodeAttributes,
                                                    replace, stopOnDuplicates)
        .addClass(nodeAttributes) = .CLS$nodeAttributes
    }
    rcx$nodeAttributes = nodeAttributes
    rcx = updateMetaData(rcx)

    return(rcx)
}


##########################################################################################
## EdgeAttributes
##########################################################################################
## edgeAttributes
## property |options          |values                    |description
## ---------|-----------------|--------------------------|-----------------------------------------------------------
## "po"     |Required         |integer                   |property of, reference to CX internal [edge id "id"](#edgeid)
## "n"      |Required         |string                    |attribute name, eg. "weight", "name", "selected"
## "v"      |Required         |string or list of strings |attribute value(s), eg. ["2", "0.34", "2.3"], "Node 6", "true"
## "d"      |Optional         |string                    |[data type](#datatypes), default "string"
## "s"      |Optional         |integer                   |subnetwork id (see [Cytoscape subnetworks](#subnetworkid))
##########################################################################################


#' Edge attributes
#' 
#' This function creates an aspect for additional attributes of edges.
#' 
#' @details 
#' Edges may have additional attributes besides a name and a representation. Those additional attributes reference a edge by its id and
#' are defined in a key-value like manner, with the name of the attribute as key. The same attribute can also be defined for different
#' [subnetworks][CySubNetworks] with different values. 
#' The values itself may also differ in their data types, therefore it is necessary to provide the values
#' as a list of the single values instead of a vector.
#' 
#' With *isList* it can be set, if a value should be considered as a list. This is of minor significance while working solely with 
#' [RCX][RCX-object] objects, unless it will be transformed to JSON. For some attributes it might be necessary that the values are encoded as
#' lists, even if they contain only one element (or even zero elements). To force an element to be encoded correctly, this parameter can 
#' be used, for example: `name="A", value=a, isList=T` will be encoded in JSON as `A=["a"]`.
#' 
#' @note The *propertyOf* parameter references the edge ids to which the attributes belong to. When adding an `r .CLS$edgeAttributes` object
#' to an [RCX][RCX-object] object, those ids must be present in the \code{\link{Edges}} aspect, otherwise an error is raised.
#'
#' @param propertyOf integer; reference to [edge ids][Edges]
#' @param name character; key of the attribute
#' @param value character; value of the attribute
#' @param dataType character (optional); data type of the attribute
#' @param isList logical (optional); a value should be considered as list
#' @param subnetworkId integer (optional); reference to [subnetwork id][CySubNetworks]
#'
#' @return *`r .CLS$edgeAttributes`* object
#' @seealso \code{\link{updateEdgeAttributes}} 
#' @export
#' @name EdgeAttributes
#'
#' @example man-roxygen-examples/edgeAttributes-create.R
createEdgeAttributes = function(propertyOf, name, value, dataType=NULL, isList=NULL, subnetworkId=NULL){
    fname="createEdgeAttributes"
    if(missing(propertyOf) || missing(name) || missing(value)) .stop("paramMissing", "propertyOf, name and value are required!")
    
    .checkSameLength(fname,
                     propertyOf, name, value, dataType, isList, subnetworkId)
    
    edgeAttributesAspect = .createAttributeAspect(propertyOf, name, value, dataType, isList, subnetworkId, fname)
    
    .addClass(edgeAttributesAspect) = .CLS$edgeAttributes
    return(edgeAttributesAspect)
}


#' Update edge attributes
#' 
#' This functions add edge attributes in the form of a \code{\link{EdgeAttributes}} object to an [RCX][RCX-object] or an other \code{\link{EdgeAttributes}} object.
#' 
#' @details
#' Edges may have additional attributes besides a name and a representation, and are represented as \code{\link{EdgeAttributes}} objects.
#' \code{\link{EdgeAttributes}} objects can be added to an [RCX][RCX-object] or an other \code{\link{EdgeAttributes}} object.
#' The *propertyOf* parameter references the \code{\link{Edges}} ids to which the attributes belong to. 
#' When adding an \code{\link{EdgeAttributes}} object to an [RCX][RCX-object] object, those ids must be present in the \code{\link{Edges}} 
#' aspect, otherwise an error is raised.
#' 
#' In the case, that a \code{\link{EdgeAttributes}} object is added to an other, or the [RCX][RCX-object] object already contains a 
#' \code{\link{EdgeAttributes}} object, some attributes might be present in both. By default, the attributes are updated with the values
#' of the latest one. This can prevented by setting the *replace* parameter to *FALSE*, in that case only new attributes are added and 
#' the existing attributes remain untouched.
#' 
#' Furthermore, if duplicated attributes are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated attributes are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{EdgeAttributes}} object; (to which the new edge attributes will be added)
#' @param edgeAttributes \code{\link{EdgeAttributes}} object; (the new aspect, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in *propertyOf* and *name* (and *subnetworkId* if present) column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{EdgeAttributes}} or [RCX][RCX-object] object with added node attributes
#' @seealso \code{\link{NodeAttributes}}, \code{\link{NetworkAttributes}}
#' @export
#' @example man-roxygen-examples/edgeAttributes-update.R
updateEdgeAttributes = function(x, edgeAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    UseMethod("updateEdgeAttributes", x)
}


#' @rdname updateEdgeAttributes
#' @export
updateEdgeAttributes.EdgeAttributesAspect = function(x, edgeAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    fname="updateEdgeAttributes"
    if(missing(x)) .stop("paramMissing", "x")
    if(missing(edgeAttributes)) .stop("paramMissing", "edgeAttributes")
    .checkClass(x, .CLS$edgeAttributes, "x", fname)
    .checkClass(edgeAttributes, .CLS$edgeAttributes, "edgeAttributes", fname)
    
    edgeAttributes = .mergeAttributesAspect(x,
                                            edgeAttributes,
                                            replace, stopOnDuplicates,
                                            .log=fname)
    .addClass(edgeAttributes) = .CLS$edgeAttributes
    
    return(edgeAttributes)
}


#' @rdname updateEdgeAttributes
#' @export
updateEdgeAttributes.RCX = function(x, edgeAttributes, replace=TRUE, stopOnDuplicates=FALSE, checkReferences=TRUE, ...){
    rcx = x
    fname="updateEdgeAttributes"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(edgeAttributes)) .stop("paramMissing", "edgeAttributes")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(edgeAttributes, .CLS$edgeAttributes, "edgeAttributes", fname)
    
    if(checkReferences){
        ## Check if the provided propertyOf ids are present in the edge ids
        .checkRefPresent(rcx, "edges", .CLS$edges, "rcx$edges", fname)
        pos = unique(edgeAttributes$propertyOf)
        .checkRefs(pos, rcx$edges$id, c("edgeAttributes$propertyOf", "rcx$edges$id"), fname)
        
        ## Check if the provided subnetwork ids are present in the subnetwork aspect
        if("subnetworkId" %in% colnames(edgeAttributes)){
            .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
            pos = unique(edgeAttributes$subnetworkId)
            pos = pos[!is.na(pos)]
            .checkRefs(pos, rcx$cySubNetworks$id, c("edgeAttributes$subnetworkId", "rcx$cySubNetworks$id"), fname)
        }
    }
    
    ## Merge aspects if already present in RCX object
    if(! is.null(rcx$edgeAttributes)){
        edgeAttributes = updateEdgeAttributes(rcx$edgeAttributes,
                                                    edgeAttributes,
                                                    replace, stopOnDuplicates)
    }
    rcx$edgeAttributes = edgeAttributes
    rcx = updateMetaData(rcx)
    
    return(rcx)
}


##########################################################################################
## NetworkAttributes
##########################################################################################
# ## networkAttributes
# property |options          |values                    |description
# ---------|-----------------|--------------------------|-----------------------------------------------------------
# "n"      |Required         |string                    |attribute name, eg. "dc:title"
# "v"      |Required         |string or list of strings |attribute value(s), eg. "Result of heat diffusion analysis"
# "d"      |Optional         |string                    |[data type](#datatypes), default "string"
# "s"      |Optional         |integer                   |subnetwork id (see [Cytoscape subnetworks](#subnetworkid))
##########################################################################################


#' Network attributes
#' 
#' This function creates an aspect for attributes of a network.
#' 
#' @details 
#' Networks may have describing attributes, that are defined in a key-value like manner, with the name of the attribute as key. 
#' The same attribute can also be defined for different [subnetworks][CySubNetworks] with different values. 
#' The values itself may differ in their data types, therefore it is necessary to provide the values as a list of the single values 
#' instead of a vector.
#' 
#' With *isList* it can be set, if a value should be considered as a list. This is of minor significance while working solely with 
#' [RCX][RCX-object] objects, unless it will be transformed to JSON. For some attributes it might be necessary that the values are encoded as
#' lists, even if they contain only one element (or even zero elements). To force an element to be encoded correctly, this parameter can 
#' be used, for example: `name="A", value=a, isList=T` will be encoded in JSON as `A=["a"]`.
#' 
#' @param name character; key of the attribute
#' @param value character; value of the attribute
#' @param dataType character (optional); data type of the attribute
#' @param isList logical (optional); a value should be considered as list
#' @param subnetworkId integer (optional); reference to [subnetwork id][CySubNetworks]
#'
#' @return `r .CLS$networkAttributes` object
#' @seealso \code{\link{updateNetworkAttributes}}; \code{\link{NodeAttributes}}, \code{\link{EdgeAttributes}}
#' @export
#' @name NetworkAttributes
#'
#' @example man-roxygen-examples/networkAttributes-create.R
createNetworkAttributes = function(name, value, dataType=NULL, isList=NULL, subnetworkId=NULL){
    fname="createNetworkAttributes"
    if(missing(name) || missing(value)) .stop("paramMissing", "name and value are required!")

    .checkNoNa(name, "name", fname)
    ## value can be string or a list of strings
    if(!is.list(value)) value = as.list(value)

    .checkSameLength(fname,
                     name, value, dataType, isList, subnetworkId)

    networkAttributesAspect = data.frame(name=name,
                                         stringsAsFactors = FALSE,
                                         check.names = FALSE)

    networkAttributesAspect$value = value
    
    if(is.null(dataType)) dataType = .inferDataType(value)
    networkAttributesAspect$dataType = dataType
    
    if(is.null(isList)) isList = .inferIsList(value)
    networkAttributesAspect$isList = isList

    if(is.null(subnetworkId)) {
        .checkUniqueDF(networkAttributesAspect, "name", fname)
    }else{
        networkAttributesAspect$subnetworkId = subnetworkId
        .checkUniqueDF(networkAttributesAspect, c("name","subnetworkId"), fname)
    }

    .addClass(networkAttributesAspect) = .CLS$networkAttributes
    return(networkAttributesAspect)
}


#' Update network attributes
#' 
#' This functions add network attributes in the form of a \code{\link{NetworkAttributes}} object to an [RCX][RCX-object] or an other 
#' \code{\link{NetworkAttributes}} object.
#' 
#' @details
#' Networks may have attributes, that are represented as \code{\link{NetworkAttributes}} objects.
#' \code{\link{NetworkAttributes}} objects can be added to an [RCX][RCX-object] or an other \code{\link{NetworkAttributes}} object.
#' 
#' In the case, that a \code{\link{NetworkAttributes}} object is added to an other, or the [RCX][RCX-object] object already contains a 
#' \code{\link{NetworkAttributes}} object, some attributes might be present in both. By default, the attributes are updated with the values
#' of the latest one. This can prevented by setting the *replace* parameter to `FALSE`, in that case only new attributes are added and 
#' the existing attributes remain untouched.
#' 
#' Furthermore, if duplicated attributes are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated attributes are present.
#'
#' @param x [RCX][RCX-object] object; (to which the new network attributes will be added)
#' @param networkAttributes \code{\link{NetworkAttributes}} object; (the new aspect, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in *name* (and *subnetworkId* if present) column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{NetworkAttributes}} or [RCX][RCX-object] object with added network attributes
#' @seealso \code{\link{NetworkAttributes}}; \code{\link{NodeAttributes}}, \code{\link{EdgeAttributes}}
#' @export
#' @example man-roxygen-examples/networkAttributes-update.R
updateNetworkAttributes = function(x, networkAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    UseMethod("updateNetworkAttributes", x)
}


#' @rdname updateNetworkAttributes
#' @export
updateNetworkAttributes.NetworkAttributesAspect = function(x, networkAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    networkAttributesOld = x
    fname="updateNetworkAttributes"
    if(missing(networkAttributesOld)) .stop("paramMissing", "x")
    if(missing(networkAttributes)) .stop("paramMissing", "networkAttributes")
    .checkClass(networkAttributesOld, .CLS$networkAttributes, "x", fname)
    .checkClass(networkAttributes, .CLS$networkAttributes, "networkAttributes", fname)
    
    networkAttributesOld$markObjectSource = "old"
    networkAttributes$markObjectSource = "new"
    keepElements = ifelse(replace,"new","old")
    
    ## fills missing columns with NAs
    networkAttributes = plyr::rbind.fill(networkAttributesOld, networkAttributes)
    
    if("subnetworkId" %in% colnames(networkAttributes)){
        cols = c("name","subnetworkId")
    }else{
        cols = c("name")
    }
    
    if(! .elementsUniqueDF(networkAttributes, cols)) {
        if(stopOnDuplicates) .checkUniqueDF(networkAttributes, cols, fname)
        
        mapFunction = function(x){
            if(nrow(x)>1) x = x[x$markObjectSource==keepElements,]
            return(x)
        }
        networkAttributes = plyr::ddply(networkAttributes,
                                        cols,
                                        mapFunction)
    }
    networkAttributes$markObjectSource = NULL
    
    .addClass(networkAttributes) = .CLS$networkAttributes
    
    return(networkAttributes)
}


#' @rdname updateNetworkAttributes
#' @export
updateNetworkAttributes.RCX = function(x, networkAttributes, replace=TRUE, stopOnDuplicates=FALSE, checkReferences=TRUE, ...){
    rcx = x
    fname="updateNetworkAttributes"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(networkAttributes)) .stop("paramMissing", "networkAttributes")
    .checkClass(rcx, .CLS$rcx, "x", fname)
    .checkClass(networkAttributes, .CLS$networkAttributes, "networkAttributes", fname)
    
    if(checkReferences){
        ## Check if the provided subnetwork ids are present in the subnetwork aspect
        if("subnetworkId" %in% colnames(networkAttributes)){
            .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
            pos = unique(networkAttributes$subnetworkId)
            pos = pos[!is.na(pos)]
            .checkRefs(pos, rcx$cySubNetworks$id, c("networkAttributes$subnetworkId", "rcx$cySubNetworks$id"), fname)
        }
    }
    
    ## Merge aspects if already present in RCX object
    if(! is.null(rcx$networkAttributes)) networkAttributes = updateNetworkAttributes(rcx$networkAttributes, 
                                                                                     networkAttributes, 
                                                                                     replace, 
                                                                                     stopOnDuplicates)

    rcx$networkAttributes = networkAttributes
    rcx = updateMetaData(rcx)
    
    return(rcx)
}

