##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   cyTableColumn
##########################################################################################
# property          |options          |values          |description
# ------------------|-----------------|----------------|------------------------------------------------
# "s"               |Optional         |integer                   |subnetwork id (see [Cytoscape subnetworks](#subnetworkid))
# "n"               |Required         |integer                   |name of the table column
# "d"               |Optional         |integer                   |[data type](#datatypes), default "string"
# "applies_to"      |Required         |string: "node_table", "edge_table", or "network_table"|indicates the Cytoscape table this applies to
##########################################################################################


#TODO: Add/update check of subnetwork ids to all other functions (.paramIsOptionalId)
#TODO: Add check for allowed data types to all functions (character, numeric, etc)
#' Cytoscape table column properties
#' 
#' This function is used to create Cytoscape table column aspects.
#' 
#' @details
#' Cytoscape contributes aspects that organize subnetworks, attribute tables, and visual attributes for use by its own layout and
#' analysis tools. Furthermore are the aspects used in web-based visualizations like within the NDEx platform.
#'  
#' These elements are used to represent Cytoscape table column labels and types. Its main use is to disambiguate empty table columns. 
#' The same attribute can also be defined for different [subnetworks][CySubNetworks] with different values.
#' Cytoscape does not currently support table columns for the root network, but this is option is included here for consistency.
#' 
#' With *isList* it can be set, if a value should be considered as a list. This is of minor significance while working solely with 
#' [RCX][RCX-object] objects, unless it will be transformed to JSON. 
#' 
#' @param appliesTo character; indicates whether this applies to "nodes", "edges" or "networks" table columns
#' @param name character; key of the attribute
#' @param dataType character (optional); data type of the attribute
#' @param isList logical (optional); a value should be considered as list
#' @param subnetworkId integer (optional); reference to [subnetwork id][CySubNetworks], but left blank (or `NA`) if root-network
#'
#' @return *`r .CLS$cyTableColumn`* object
#' @seealso \code{\link{updateCyTableColumn}}; \code{\link{CyNetworkRelations}}
#' @export
#' @name CyTableColumn
#'
#' @example man-roxygen-examples/cy-table-column-create.R
createCyTableColumn = function(appliesTo, name, dataType=NULL, isList=NULL, subnetworkId=NULL){
    fname="createCyTableColumn"
    if(missing(name)) .stop("paramMissing", "name is required!")
    
    ## are the values of appliesTo in the dictionary (.DICT$TCappliesTo)
    .checkBContainsAllA(appliesTo, .DICT$TCappliesTo, "appliesTo", fname)
    
    .checkNoNa(name, "name", fname)
    
    .checkSameLength(fname,
                     appliesTo, name, dataType, isList, subnetworkId)
    
    aspect = data.frame(appliesTo=appliesTo,
                        name=name,
                        stringsAsFactors = F,
                        check.names = F)
    
    if(is.null(dataType)) dataType = rep("character", length(name))
    aspect$dataType = dataType
    
    if(is.null(isList)) isList = rep(F, length(name))
    aspect$isList = isList
    
    if(is.null(subnetworkId)) {
        .checkUniqueDF(aspect, c("appliesTo","name"), fname)
    }else{
        .checkNumeric(subnetworkId, "subnetworkId", fname)
        aspect$subnetworkId = subnetworkId
        .checkUniqueDF(aspect, c("appliesTo","name","subnetworkId"), fname)
    }
    
    .addClass(aspect) = .CLS$cyTableColumn
    return(aspect)
}


#' Update Cytoscape table column properties
#' 
#' This functions add hidden attributes in the form of a \code{\link{CyTableColumn}} object to an other \code{\link{CyTableColumn}} or an
#' [RCX][RCX-object] object.
#' 
#' @details 
#' In the case, that a \code{\link{CyTableColumn}} object is added to an other, or the [RCX][RCX-object] object already contains a 
#' \code{\link{CyTableColumn}} object, some properties might be present in both. By default, the properties are updated with the values
#' of the latest one. This can prevented by setting the *replace* parameter to `FALSE`, in that case only new attributes are added and 
#' the existing attributes remain untouched.
#' 
#' Furthermore, if duplicated properties are considered as a preventable mistake, an error can be raised by setting *stopOnDuplicates* 
#' to `TRUE`. This forces the function to stop and raise an error, if duplicated properties are present.
#' 
#' Cytoscape does not currently support table columns for the root network, but this is option is included here for consistency.
#'
#' @param x [RCX][RCX-object] or \code{\link{CyTableColumn}} object; (to which the new table column properties will be added)
#' @param cyTableColumns \code{\link{CyTableColumn}} object; (the new aspect, that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in *appliesTo* and *name*` (and *subnetworkId* if present) column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{CyTableColumn}} or [RCX][RCX-object] object with added hidden attributes
#' @seealso \code{\link{CySubNetworks}}
#' @export
#' @example man-roxygen-examples/cy-table-column-update.R
updateCyTableColumn = function(x, cyTableColumns, replace=T, stopOnDuplicates=F, ...){
    UseMethod("updateCyTableColumn", x)
}


#' @rdname updateCyTableColumn
#' @export
updateCyTableColumn.CyTableColumnAspect = function(x, cyTableColumns, replace=T, stopOnDuplicates=F, ...){
    tableColumnsOld = x
    fname="updateCyTableColumn"
    if(missing(tableColumnsOld)) .stop("paramMissing", "x")
    if(missing(cyTableColumns)) .stop("paramMissing", "cyTableColumns")
    .checkClass(tableColumnsOld, .CLS$cyTableColumn, "x", fname)
    .checkClass(cyTableColumns, .CLS$cyTableColumn, "cyTableColumns", fname)
    
    tableColumnsOld$markObjectSource = "old"
    cyTableColumns$markObjectSource = "new"
    keepElements = ifelse(replace,"new","old")
    
    ## fills missing columns with NAs
    tableColumns = plyr::rbind.fill(tableColumnsOld, cyTableColumns)
    
    if("subnetworkId" %in% colnames(tableColumns)){
        cols = c("appliesTo","name","subnetworkId")
    }else{
        cols = c("appliesTo","name")
    }
    
    if(! .elementsUniqueDF(tableColumns, cols)) {
        if(stopOnDuplicates) .checkUniqueDF(tableColumns, cols, fname)
        
        mapFunction = function(x){
            if(nrow(x)>1) x = x[x$markObjectSource==keepElements,]
            return(x)
        }
        tableColumns = plyr::ddply(tableColumns,
                                   cols,
                                   mapFunction)
    }
    tableColumns$markObjectSource = NULL
    
    .addClass(tableColumns) = .CLS$cyTableColumn
    
    return(tableColumns)
}


#' @rdname updateCyTableColumn
#' @export
updateCyTableColumn.RCX = function(x, cyTableColumns, replace=T, stopOnDuplicates=F, checkReferences=T, ...){
    rcx = x
    fname="addCyTableColumn"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(cyTableColumns)) .stop("paramMissing", "cyTableColumns")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(cyTableColumns, .CLS$cyTableColumn, "cyTableColumns",fname)
    
    if((! is.null(cyTableColumns$subnetworkId)) && (checkReferences)){
        .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
        ids = unique(unlist(cyTableColumns$subnetworkId))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$cySubNetworks$id, c("cyTableColumns$subnetworkId", "rcx$cySubNetworks$id"), fname)
    }
    
    ## Merge aspects if already present in RCX object
    if(! is.null(rcx$cyTableColumn)) cyTableColumns = updateCyTableColumn(rcx$cyTableColumn, 
                                                                          cyTableColumns, 
                                                                          replace, 
                                                                          stopOnDuplicates)
    
    rcx$cyTableColumn = cyTableColumns
    rcx = updateMetaData(rcx)
    
    return(rcx)
}
