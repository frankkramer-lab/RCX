##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   MetaData
##########################################################################################
# property			|options							|description
# --------------------|-----------------------------------|-------------------------------------------------------
# "name"				|Required in pre- and post-metadata	|The name of the aspect
# "version"			|Required in pre-metadata			|version of this aspect schema ("1.1.0", "2.0.0", etc.)
# "idCounter"			|Required if the aspect exports IDs	|Integer (All Element IDs are integers; see [node id](#nodeid), [edge id](#edgeid), [cytoscape group id](#cygroupid))
# "elementCount"      |Optional							|number (integer) of elements in this aspect
# "consistencyGroup"	|Optional                           |An integer identifier shared by aspects to indicate that they are mutually consistent
# "properties"		|Optional					        |An aspect-defined property list
# "checksum"          |Optional (Deprecated) 	            |(NDEx CX implementation doesn???t support this attribute. This attribute is ignored in NDEx)
##########################################################################################


#' Update RCX meta-data
#' 
#' The meta-data aspect contains meta-data about the aspects in the [RCX][RCX-object] object.
#' It can be generated automatically based on the aspects present in a [RCX][RCX-object] object:
#' - for *version* and *consistencyGroup* default values are used
#' - *idCounter* is inferred with \code{\link{hasIds}} and \code{\link{maxId}} of an aspect
#' - *elementCount* is inferred from \code{\link{countElements}}
#' - *properties* is left out by default
#' 
#' If *version*, *consistencyGroup* or *properties* should have a different value, they can be set using a named vector 
#' (or named list for *properties*), where the name must be an accession name of that aspect in the \code{\link{RCX-object}} 
#' (e.g. `nodes` or `cyVisualProperties`).
#' 
#' Besides being a named list by aspect accession name, *properties* must also contain the single key-value pairs as a further named list.
#' To remove all key-value pairs for one aspect, an empty list can be provided instead of a list with key-value pairs.
#' To simplify adding of properties to a single aspect, there is the \code{\link{updateMetaDataProperties}} function available.
#' 
#' @note The meta-data will always be updated automatically, when an aspect is added to or changed in the [RCX][RCX-object] object.
#'
#' @param x [RCX][RCX-object] object or an aspect of a RCX; its class must be one of the standard RCX aspect classes
#' @param version named character (optional); version of the aspect (default:"1.0")
#' @param consistencyGroup named numerical (optional); consistency group of the aspect (default:1)
#' @param properties named list (optional); properties that need to be fetched or updated independently of aspect data
#'
#' @return `r .CLS$metaData` object or [RCX][RCX-object] object
#' @name Meta-data
#' @seealso \code{\link{updateMetaDataProperties}}
#' @export
#'
#' @example man-roxygen-examples/meta-data-update.R
updateMetaData = function(x, version = NULL, consistencyGroup = NULL, properties=NULL){
    UseMethod("updateMetaData", x)
}

#' @rdname Meta-data
#' @export
updateMetaData.RCX = function(x, version = NULL, consistencyGroup = NULL, properties=NULL){
    rcx = x
    fname = "updateMetaData"
    if(missing(rcx)) .stop("paramMissingRCX")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    
    existingMetaData = rcx$metaData
    if(!is.null(existingMetaData)) .checkClass(existingMetaData, .CLS$metaData, .formatO("metaData",fname))
    
    if(!is.null(version)) .checkNamedCharacter(version, "version", fname)
    if(!is.null(consistencyGroup)) .checkNamedNumeric(consistencyGroup, "consistencyGroup", fname)
    if(!is.null(properties)) .checkNamedList(properties, "properties", fname)
    
    metaData = data.frame(name=c(),
                          version=c(),
                          properties=list(),
                          stringsAsFactors=FALSE, check.names=FALSE)
    
    for (aspect in rcx) {
        if(!.paramClass(aspect, .CLS$metaData)){
            versionTmp = NULL
            consistencyGroupTmp = NULL
            propertiesTmp = NULL
            
            name = aspectClass2Name(.aspectClass(aspect))
            
            if(!is.null(existingMetaData)){
                if(name %in% existingMetaData$name){
                    versionTmp = existingMetaData[existingMetaData$name==name, "version"]
                    consistencyGroupTmp = existingMetaData[existingMetaData$name==name, "consistencyGroup"]
                    propertiesTmp = existingMetaData[existingMetaData$name==name, "properties"][[1]]
                }
            }
            
            if((!is.null(version)) && (name %in% names(version))) versionTmp = version[name]
            if((!is.null(consistencyGroup)) && (name %in% names(consistencyGroup))) consistencyGroupTmp = consistencyGroup[name]
            if((!is.null(properties)) && (name %in% names(properties))) propertiesTmp = properties[[name]]
            
            metaData = plyr::rbind.fill(metaData,
                                        updateMetaData(aspect,
                                                       versionTmp,
                                                       consistencyGroupTmp,
                                                       propertiesTmp))
        }
    }

    
    .addClass(metaData) = .CLS$metaData
    rcx$metaData = metaData
    return(rcx)
}

#' @rdname Meta-data
#' @export
updateMetaData.default = function(x, version = NULL, consistencyGroup = NULL, properties=NULL){
    aspect = x
    fname = "updateMetaData"
    .checkClassOneOf(aspect, .CLS, "aspect", fname)
    if(!is.null(version)) .checkCharacter(version, "version", fname)
    if(!is.null(consistencyGroup)) .checkNumeric(consistencyGroup, "consistencyGroup", fname)
    if(!is.null(properties)) {
        .checkList(properties, "properties", fname)
        if(length(properties)!=0) .checkNamedList(properties, "properties", fname)
    }
    
    name = aspectClass2Name(.aspectClass(aspect))
    version = ifelse(is.null(version),"1.0",version)
    idCounter = ifelse(hasIds(aspect), maxId(aspect), NA)
    elementCount = countElements(aspect)
    consistencyGroup = ifelse(is.null(consistencyGroup),1,consistencyGroup)
    
    metaData = data.frame(name=name,
                          version=version,
                          idCounter=idCounter,
                          elementCount=elementCount,
                          consistencyGroup=consistencyGroup,
                          stringsAsFactors=FALSE, check.names=FALSE)
    
    if(!is.null(properties)) metaData$properties[[1]] = properties
    #if(!is.null(properties)) metaData$properties[[1]] = unlist(properties)
    
    return(metaData)
}


#' Update meta-data properties
#' 
#' The \code{\link{Meta-data}} aspect contains meta-data about the aspects in the \code{\link{RCX-object}}.
#' Properties that need to be fetched or updated independently of aspect data are added with this function.
#'
#' @param rcx [RCX][RCX-object] object;
#' @param aspectName character; name of the aspect as displayed in \code{\link{Meta-data}} (e.g. "nodes")
#' @param property named list; property as key-value pairs (empty list to remove all)
#'
#' @return [RCX][RCX-object] object with updated \code{\link{Meta-data}} aspect
#' @export
#'
#' @example man-roxygen-examples/meta-data-properties-update.R
updateMetaDataProperties = function(rcx, aspectName, property){
    fname="addMetaDataProperties"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(aspectName)) .stop("paramMissing", "aspectName")
    if(missing(property)) .stop("paramMissing", "property")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkCharacter(aspectName, "aspectName", fname)
    .checkList(property, "property", fname)
    if(length(property)!=0) .checkNamedList(property, "property", fname)
    
    if(! "metaData" %in% names(rcx)) rcx = updateMetaData(rcx)
    metaData = rcx$metaData
    if("properties" %in% colnames(metaData)){
        mp = metaData$properties
    }else{
        mp = vector(mode = "list", length = nrow(metaData))
    }
    
    i = which(metaData$name==aspectName)
    
    mp[[i]] = property
    metaData$properties = mp
    rcx$metaData = metaData
    return(rcx)
}

