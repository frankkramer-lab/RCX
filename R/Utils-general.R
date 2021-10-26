################################################################################
## Authors:
##   Florian Auer
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################

#' Concatenate as comma separated character vector
#'
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @param x character vector.
#'
#' @return character vector of length 1.
#' 
#' @examples
#' \dontrun{
#' a <- c("one", "two", "three")
#' .pasteC(a)
#' #[1] "one, two, three"
#' }
.pasteC = function(x){
    return(paste0(x,collapse = ", "))
}


#' Format objects for error logging
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param v character vector; just some strings
#' @param fname character; function name
#' @param .log character; previous called functions
#'
#' @return character
#' 
#' @name .format
#' @examples
#' \dontrun{
#' v <- c("one", "two", "three")
#' fname <- "foo"
#' .log <- c("foo1", "foo2", "foo3")
#' 
#' .formatQuote(v)
#' #[1] "\"one\""   "\"two\""   "\"three\""
#' 
#' .formatComma(v)
#' #[1] "one, two, three"
#' 
#' .formatParams(v)
#' #[1] "\"one\", \"two\" and \"three\""
#' 
#' .formatData(v)
#' #[1] "one$two$three"
#' 
#' .formatLog(v)
#' #[1] "\"one\", \"two\" and \"three\""
#' 
#' .formatLog(v, fname)
#' #[1] "\"one\", \"two\" and \"three\" (in foo)"
#' 
#' .formatO(.formatLog(v), fname)
#' #[1] "\"\"one\", \"two\" and \"three\"\" (foo)"
#' }
NULL

#' @describeIn dot-format add quotes to the vector elements: `"<v[i]>"`
.formatQuote = function(v){
    return(paste0('"',v,'"'))
}

#' @describeIn dot-format add commas between the vector elements: `<v[1]>, <v[2]>, <v[3]>`
.formatComma = function(v){
    return(paste0(v,collapse = ", "))
}

#' @describeIn dot-format format the vector: `"<v[1]>", "<v[2]>" and "<v[3]>"`
.formatParams = function(v, con="and"){
    # add quotes
    v = paste0('"',v,'"')
    # add commas and "and" inbetween
    if(length(v)>1){
        first = v[seq_len(length(v)-1)]
        last = v[length(v)]
        first = .formatComma(first)
        v = paste0(first," ",con," ", last)
    }
    return(v)
}

#' @describeIn dot-format format the vector: `<v[1]>$<v[2]>$<v[3]>`
.formatData = function(v){
    v = paste(v, collapse = "$")
    return(v)
}

#' @describeIn dot-format format the vector: `"<v[1]>", "<v[2]>" and "<v[3]>" (in <fname>)`
.formatLog = function(v,fname=c()){
    v = .formatParams(v)
    if(length(fname)!=0) fname = paste0(" (in ",fname,")")
    v = paste0(v, fname)
    return(v)
}

#' @describeIn dot-format format a object name with its calling function
.formatO = function(v,fname){
    return(paste0('"',v,'" (',fname,')'))
}


#' Add or remove a class name from an object
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @param x an R object.
#' @param value character vector of lenght 1.
#'
#' @return an R object.
#' 
#' @name .modClass
#' @examples 
#' x = list(a="foo", b="bar")
#' class(x)
#' # [1] "list"
#' \dontrun{
#' .addClass(x) <- "blubb"
#' class(x)
#' # [1] "blubb" "list" 
#' 
#' .addClass(x) <- "blubb"
#' class(x)
#' # [1] "blubb" "list" 
#' 
#' .removeClass(x) <- "blubb"
#' class(x)
#' # [1] "list"
#' }
NULL

#' @rdname dot-modClass 
#' @usage .addClass(x) <- value
#' @aliases .addClass
'.addClass<-' = function(x, value){
    if(! value %in% class(x)) class(x) = c(value, class(x))
    return(x)
}

#' @rdname dot-modClass
#' @usage .removeClass(x) <- value
#' @aliases .removeClass
'.removeClass<-' = function(x, value){
    class(x)=class(x)[! class(x) %in% value]
    return(x)
}


#' Get the class of aspects
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' Only get the class of aspects that are defined in `aspectClasses`
#' 
#' @param x a potential aspect
#'
#' @return The aspect class name or `NA` if it's not an aspect
#'
#' @examples
#' x = list(a="foo", b="bar")
#' class(x)
#' # [1] "list"
#' \dontrun{
#' .addClass(x) <- .CLS$nodes
#' class(x)
#' # [1] "NodesAspect" "list" 
#' 
#' .aspectClass(x)
#' # [1] "NodesAspect"
#' 
#' .removeClass(x) <- "NodesAspect"
#' 
#' .aspectClass(x)
#' # [1] "NA"
#' 
#' }
.aspectClass = function(x){
    cls = NA
    if(any(class(x) %in% aspectClasses)){
        cls = class(x)[class(x) %in% aspectClasses]
    }
    return(cls)
}


#' Logging (printing) the results of test cases
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param info character; description of the test case.
#' @param pass boolean; was the test passed?
#' @param sep character (default="..."); separates description from result.
#' @param spaceLine boolean (default=FALSE); should a blank line be added after.
#' 
#' @return NULL, only prints the log
#'
#' @examples
#' testPassed <- TRUE
#' testFailed <- FALSE
#' \dontrun{
#' .log('testing something', testPassed)
#' #testing something...OK
#' .log('testing other stuff', testFailed, spaceLine=TRUE)
#' #testing other stuff...FAIL
#' #
#' .log('testing more', testPassed, " ", TRUE)
#' #testing more OK
#' }
.log = function(info, pass, sep="...", spaceLine=FALSE){
    cat(paste0(info,sep))
    if(pass){
        cat("OK\n")
    }else{
        cat("FAIL\n")
    }
    if(spaceLine) cat("\n")
    return(NULL)
}


#' Infer the data type from values and check if the value elements are a list
#' 
#' Each element has an R data type (i.e. class). If more than one element are present in one list element, it is marked as list
#' 
#' `.inferDataType` infers the data type of the elements in the vector or list.
#' `.inferIsList` infers for each element if it is a list. For a vector, the return therefore is `FALSE` for each element!
#'
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param values vector or list of R data values 
#'
#' @return character vector of data types or logical vector of list status
#' 
#' @name .infer
#'
#' @examples
#' NULL
.inferDataType = function(values){
    if(!is.list(values)) values = as.list(values)
    mapFunction = function(x){
        type = "string"
        if(is.logical(x)) type = "boolean"
        if(is.numeric(x)) type = "double"
        return(type)
    }
    return(sapply(values, mapFunction))
}


#' @describeIn dot-infer Infer, if the values are lists
.inferIsList = function(values){
    if(!is.list(values)) values = as.list(values)
    mapFunction = function(x){
        isList = (length(x)>1)
        return(isList)
    }
    return(sapply(values, mapFunction))
}


#TODO: add an example from nodeAspect
#' Merge two aspects (data.frames)
#' 
#' Merges two aspects, that are both data.frames and of the same aspect class.
#' If the `idCol` contains duplicates new ids (for `secondAspect`) are created (ids of `firstAspect` are kept), 
#' unless it is spezified otherwise by `saveOldIds`.
#' 
#' @note The two aspects must be the same type of aspect (same aspect class)!
#' 
#' @note Internal function only for convenience
#' @keywords internal
#' @seealso \code{\link{.mergeAttributesAspect}}, \code{\link{.createAttributeAspect}}
#'
#' @param firstAspect data.frame; first aspect.
#' @param secondAspect data.frame; second aspect.
#' @param idCol character; name of the column to merge on.
#' @param uniqCols character; name of the column to be checked for uniqueness.
#' @param stopOnDuplicates boolean (default=FALSE); whether to stop, if duplicates in `uniqCols` column are found.
#' @param saveOldIds boolean (default=TRUE); whether to keep the IDs from `secondAspect`, if duplicates in `uniqCols` column are found.
#' @param .log character (optional); name of the calling function used in logging
#'
#' @return data.frame of the merged aspects.
#'
#' @examples
#' NULL
.mergeIdAspect = function(firstAspect, secondAspect, idCol, uniqCols, stopOnDuplicates=FALSE, saveOldIds=TRUE, .log=c()){
    newAspCls = .aspectClass(secondAspect)
    .checkClass(firstAspect, newAspCls, "aspects", .log)
    
    # ## if firstAspect has already oldId column, then keep it
    # removeOldIdCol = (! "oldId" %in% colnames(firstAspect))
    firstAspect$oldId = NA

    ## remember the ids in secondAspect
    if(saveOldIds) secondAspect$oldId = secondAspect[,idCol]

    ## fills missing columns with NAs
    mergedAsp = plyr::rbind.fill(firstAspect, secondAspect)

    ## create new ids (for secondAspect) if id already in use (in firstAspect)
    if(! .elementsUniqueDF(mergedAsp, uniqCols)){
        if(stopOnDuplicates) .stop("paramNotUnique",.formatLog(uniqCols, .log))
        firstId = maxId(firstAspect) + 1
        lastId = firstId + length(secondAspect[,idCol]) - 1
        secondAspect[,idCol] = firstId:lastId
        ## oldId column necessary for duplicated ids, so don't remove it
        removeOldIdCol = FALSE
        ## redo with new ids
        mergedAsp = plyr::rbind.fill(firstAspect, secondAspect)
    }

    if(!saveOldIds) mergedAsp$oldId = NULL

    return(mergedAsp)
}


#' Create a default *AttributeAspect
#' 
#' Some aspects like *`r .CLS$nodeAttributes`* or *`r .CLS$edgeAttributes`* use a key-value scheme. 
#' This function helps in constructing while avoiding repetition.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#' @seealso \code{\link{.mergeIdAspect}}, \code{\link{.mergeAttributesAspect}}, \code{\link{CySubNetworks}}
#'
#' @param propertyOf integer; refers to the IDs of an other aspect
#' @param name character; key of the attribute
#' @param value character; value of the attribute
#' @param dataType character (optional); 
#' @param isList logical (optional); 
#' @param subnetworkId integer (optional); CySubNetworks
#'
#' @return *AttributeAspect prototype object
#'
#' @examples
#' NULL
.createAttributeAspect = function(propertyOf, name, value, dataType, isList, subnetworkId=NULL, .log=""){
    if(missing(propertyOf) || missing(name) || missing(value)) .stop("paramMissing", "propertyOf, name and value are required!")
    
    ## value can be string or a list of strings
    if(!is.list(value)) value = as.list(value)
    
    .checkSameLength(.log,
                     propertyOf, name, value, dataType, isList, subnetworkId)
    
    .checkIsId(propertyOf, "propertyOf", .log)
    .checkNoNa(name, "name", .log)
    
    attributesAspect = data.frame(propertyOf=propertyOf,
                                  name=name,
                                  stringsAsFactors = FALSE,
                                  check.names = FALSE)

    ## with characters and numbers mixed
    attributesAspect$value = value
    
    if(is.null(dataType)) dataType = .inferDataType(value)
    attributesAspect$dataType = dataType
    
    if(is.null(isList)) isList = .inferIsList(value)
    attributesAspect$isList = isList
    
    if(is.null(subnetworkId)) {
        .checkUniqueDF(attributesAspect,c("propertyOf","name"), .log)
    }else{
        .checkNumeric(subnetworkId, "subnetworkId", .log)
        attributesAspect$subnetworkId = subnetworkId
        
        .checkUniqueDF(attributesAspect,c("propertyOf","name","subnetworkId"), .log)
    }
    
    return(attributesAspect)
}


#' Merge two *AttributeAspects
#'
#' Some aspects like *`r .CLS$nodeAttributes`* or *`r .CLS$edgeAttributes`* use a key-value scheme. 
#' This function helps in merging while avoiding repetition.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#' @seealso \code{\link{.mergeIdAspect}}, \code{\link{.createAttributeAspect}}
#' 
#' @param firstAspect *AttributeAspect object; first aspect.
#' @param secondAspect *AttributeAspect object; second aspect.
#' @param replace logical (default: TRUE); should duplicate keys be replaced with values of the secondAspect
#' @param stopOnDuplicates logical (default: FALSE); whether to stop, if duplicate keys are found
#' @param required character (optional); names of required column names
#' @param optional character (optional); names of optional column names
#' @param .log character (optional); origin of the data used for error logging
#'
#' @return *AttributeAspect object
#'
#' @examples
#' NULL
.mergeAttributesAspect = function(firstAspect, secondAspect, 
                                  replace=TRUE, stopOnDuplicates=FALSE, 
                                  required=c("propertyOf","name"), 
                                  optional="subnetworkId",
                                  .log=c()){
    firstAspect$markObjectSource = "old"
    secondAspect$markObjectSource = "new"
    keepElements = ifelse(replace,"new","old")
    
    ## fills missing columns with NAs
    secondAspect = plyr::rbind.fill(firstAspect, secondAspect)
    
    ## if subnetworkId is in
    containOpt = optional %in% colnames(secondAspect)
    cols = c(required, optional[containOpt])
    
    if(! .elementsUniqueDF(secondAspect, cols)) {
        if(stopOnDuplicates) .checkUniqueDF(secondAspect, cols, .log)
        
        mapFunction = function(x){
            if(nrow(x)>1) x = x[x$markObjectSource==keepElements,]
            return(x)
        }
        secondAspect = plyr::ddply(secondAspect,
                                   cols,
                                   mapFunction)
    }
    secondAspect$markObjectSource = NULL
    
    return(secondAspect)
}


#' Helper to create structure for classes `r .CLSvp$properties` and `r .CLSvp$dependencies`
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param name character, optional; name of the properties
#' @param value character or named character; value of the properties
#' @param .log character (optional); name of the calling function used in logging
#'
#' @return data.frame
#' @seealso Used in \code{\link{CyVisualPropertyProperties}}, \code{\link{CyVisualPropertyDependencies}} and \code{\link{CyVisualPropertyMappings}}
#'
#' @examples
#' \dontrun{
#' data1 = c(NODE_BORDER_STROKE="SOLID", NODE_BORDER_WIDTH="1.5")
#' .createCyVpPorD(value=data1)
#' 
#' key1 = c("NODE_BORDER_STROKE", "NODE_BORDER_WIDTH")
#' value1 = c("SOLID", "1.5")
#' .createCyVpPorD(key1, value1)
#' 
#' # Result for either:
#' #                 name value
#' # 1 NODE_BORDER_STROKE SOLID
#' # 2  NODE_BORDER_WIDTH   1.5
#' }
.createCyVpPorD = function(name=NULL, value, .log=""){
    if(missing(value) || is.null(value)) .stop("paramMissing", "value is required!")
    if(is.null(name)) {
        if(is.null(names(value))) .stop("paramMissing", "value with names is required!")
        name = names(value)
    }
    value = unname(value)
    
    .checkCharacter(name, "name", .log)
    .checkCharacter(value, "value", .log)
    
    pod  = data.frame(name=name,
                      value=value,
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
    
    idCols = "name"
    .checkUniqueDF(pod, idCols, .log)
    
    return(pod)
}

