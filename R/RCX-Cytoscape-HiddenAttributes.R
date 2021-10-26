##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   cyHiddenAttributes
##########################################################################################
# property          |options          |values          |description
# ------------------|-----------------|----------------|------------------------------------------------
# "s"               |Optional         |integer                   |subnetwork id (see [Cytoscape subnetworks](#subnetworkid))
# "n"               |Required         |string                    |name of this hidden attribute
# "v"               |Required         |string or list of other   |attribute value(s), eg. "layoutAlgorithm"
# "d"               |Optional         |string                    |[data type](#datatypes), default "string"
##########################################################################################


#' Cytoscape hidden attributes
#' 
#' This function is used to create Cytoscape hidden attributes aspects.
#' 
#' @details
#' Cytoscape contributes aspects that organize subnetworks, attribute tables, and visual attributes for use by its own layout and
#' analysis tools. Furthermore are the aspects used in web-based visualizations like within the NDEx platform.
#'  
#' Besides network attributes, networks may have additional describing attributes originated from and used by Cytoscape.
#' They are also defined in a key-value like manner, with the name of the attribute as key. 
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
#' @param value character or list of character; value of the attribute
#' @param dataType character (optional); data type of the attribute
#' @param isList logical (optional); a value should be considered as list
#' @param subnetworkId integer (optional); refers to the IDs of a subnetwork aspect, but left blank (or `NA`) if root-network
#'
#' @return *`r .CLS$cyHiddenAttributes`* object
#' @seealso \code{\link{updateCyHiddenAttributes}};
#' @export
#' @name CyHiddenAttributes
#'
#' @example man-roxygen-examples/cy-hidden-attributes-create.R
createCyHiddenAttributes = function(name, value, dataType=NULL, isList=NULL, subnetworkId=NULL){
    fname="createCyHiddenAttributes"
    if(missing(name) || missing(value)) .stop("paramMissing", "name and value are required!")
    
    .checkNoNa(name,"name", fname)
    
    ## value can be string or a list of strings
    if(!is.list(value)) value = as.list(value)
    
    .checkSameLength(fname,
                     name, value, dataType, isList, subnetworkId)
    
    aspect = data.frame(name=name,
                        stringsAsFactors = FALSE,
                        check.names = FALSE)
    
    aspect$value = value
    
    if(is.null(dataType)) dataType = .inferDataType(value)
    aspect$dataType = dataType
    
    if(is.null(isList)) isList = .inferIsList(value)
    aspect$isList = isList
    
    if(is.null(subnetworkId)) {
        .checkUniqueDF(aspect, "name", fname)
    }else{
        .checkNumeric(subnetworkId, "subnetworkId", fname)
        aspect$subnetworkId = subnetworkId
        .checkUniqueDF(aspect, c("name","subnetworkId"), fname)
    }
    
    .addClass(aspect) = .CLS$cyHiddenAttributes
    return(aspect)
}


#' Update Cytoscape hidden attributes
#' 
#' This functions add hidden attributes in the form of a \code{\link{CyHiddenAttributes}} object to an other \code{\link{CyHiddenAttributes}} or an
#' [RCX][RCX-object] object.
#' 
#' @details 
#' Cytoscape subnetworks allow to group a set of nodes and corresponding edges together, and network relations define the relations between those
#' networks.
#' \code{\link{CyHiddenAttributes}} objects can be added to an [RCX][RCX-object] or an other \code{\link{CyHiddenAttributes}} object.
#' 
#' In the case, that a \code{\link{CyHiddenAttributes}} object is added to an other, or the [RCX][RCX-object] object already contains a 
#' \code{\link{CyHiddenAttributes}} object, some attributes might be present in both. By default, the attributes are updated with the values
#' of the latest one. This can prevented by setting the *replace* parameter to `FALSE`, in that case only new attributes are added and 
#' the existing attributes remain untouched.
#' 
#' Furthermore, if duplicated attributes are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated attributes are present.
#'
#' @param x [RCX][RCX-object] or \code{\link{CyHiddenAttributes}} object; (to which the new hidden attributes will be added)
#' @param hiddenAttributes \code{\link{CyHiddenAttributes}} object; (the new aspect, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in `name` (and `subnetworkId` if present) column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{CyHiddenAttributes}} or [RCX][RCX-object] object with added hidden attributes
#' @export
#' @example man-roxygen-examples/cy-hidden-attributes-update.R
updateCyHiddenAttributes = function(x, hiddenAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    UseMethod("updateCyHiddenAttributes", x)
}


#' @rdname updateCyHiddenAttributes
#' @export
updateCyHiddenAttributes.CyHiddenAttributesAspect = function(x, hiddenAttributes, replace=TRUE, stopOnDuplicates=FALSE, ...){
    hiddenAttributesOld = x
    fname="updateCyHiddenAttributes"
    if(missing(hiddenAttributesOld)) .stop("paramMissing", "x")
    if(missing(hiddenAttributes)) .stop("paramMissing", "hiddenAttributes")
    .checkClass(hiddenAttributesOld, .CLS$cyHiddenAttributes, "x", fname)
    .checkClass(hiddenAttributes, .CLS$cyHiddenAttributes, "hiddenAttributes", fname)
    
    hiddenAttributesOld$markObjectSource = "old"
    hiddenAttributes$markObjectSource = "new"
    keepElements = ifelse(replace,"new","old")
    
    ## fills missing columns with NAs
    hiddenAttributes = plyr::rbind.fill(hiddenAttributesOld, hiddenAttributes)
    
    if("subnetworkId" %in% colnames(hiddenAttributes)){
        cols = c("name","subnetworkId")
    }else{
        cols = c("name")
    }
    
    if(! .elementsUniqueDF(hiddenAttributes, cols)) {
        if(stopOnDuplicates)  .checkUniqueDF(hiddenAttributes, cols, fname)
        
        mapFunction = function(x){
            if(nrow(x)>1) x = x[x$markObjectSource==keepElements,]
            return(x)
        }
        hiddenAttributes = plyr::ddply(hiddenAttributes,
                                       cols,
                                       mapFunction)
    }
    hiddenAttributes$markObjectSource = NULL
    
    .addClass(hiddenAttributes) = .CLS$cyHiddenAttributes
    
    return(hiddenAttributes)
}


#' @rdname updateCyHiddenAttributes
#' @export
updateCyHiddenAttributes.RCX = function(x, hiddenAttributes, replace=TRUE, stopOnDuplicates=FALSE, checkReferences=TRUE, ...){
    rcx = x
    fname="updateCyHiddenAttributes"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(hiddenAttributes)) .stop("paramMissing", "hiddenAttributes")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(hiddenAttributes, .CLS$cyHiddenAttributes, "hiddenAttributes",fname)
    
    if((! is.null(hiddenAttributes$subnetworkId)) && (checkReferences)){
        .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
        ids = unique(unlist(hiddenAttributes$subnetworkId))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$cySubNetworks$id, c("hiddenAttributes$subnetworkId", "rcx$cySubNetworks$id"), fname)
    }
    
    ## Merge aspects if already present in RCX object
    if(! is.null(rcx$cyHiddenAttributes)) hiddenAttributes = updateCyHiddenAttributes(rcx$cyHiddenAttributes, 
                                                                                          hiddenAttributes, 
                                                                                          replace, 
                                                                                          stopOnDuplicates)
    
    rcx$cyHiddenAttributes = hiddenAttributes
    rcx = updateMetaData(rcx)
    
    return(rcx)
}

