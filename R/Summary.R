################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


##########################################################################################
## Helper for summary
##########################################################################################


#' Helper function to mark columns that are ids or reference ids
#'
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @param aspect an aspect (data.frame)
#'
#' @return the aspect (data.frame)
#' @examples 
#' edges = createEdges(source=c(1,1), target=c(2,3))
#' edges = RCX:::.summaryAspect(edges)
#' class(edges$id)
.summaryAspect = function(aspect){
  ## mark id columns
  if(hasIds(aspect)){
    idProp = aspect[[idProperty(aspect)]]
    .addClass(idProp) = "AspectIdColumn"
    aspect[[idProperty(aspect)]] = idProp
  }
  
  ## mark reference columns
  refs = refersTo(aspect)
  if(!is.null(refs)){
    refs = names(refs)
    for(r in refs){
      .markRefColumn(aspect) = r
    }
  }
  
  return(aspect)
}


#' Mark required and optional references within a data.frame
#' 
#' Assigns a class to a data.frame column to force a custom format in summary generation.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param aspect an aspect (data.frame)
#' @param value character; property 
#'
#' @return the aspect (data.frame)
#' @name markRefColumn
#' @export
#'
#' @examples
#' df = data.frame(bla=c("a","b","c"),
#'                 blubb=c("a","b","c"))
#' RCX:::.markRefColumn(df) = "bla"
#' 
#' summary(df)
`.markRefColumn<-` = function(aspect, value){
  if(value %in% colnames(aspect)){
    rObject = aspect[[value]]
    .addClass(rObject) = "AspectRefColumn"
    aspect[[value]] = rObject
  }
  return(aspect)
}

#' @rdname markRefColumn
`.markReqRefColumn<-` = function(aspect, value){
  if(value %in% colnames(aspect)){
    rObject = aspect[[value]]
    .addClass(rObject) = "AspectReqRefColumn"
    aspect[[value]] = rObject
  }
  return(aspect)
}

#' Mark attribute name columns within a data.frame
#' 
#' Assigns a class to a data.frame column to force a custom format in summary generation.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param aspect an aspect (data.frame)
#' @param value character; property 
#'
#' @return the aspect (data.frame)
#' @name markAttributeColumn
#' @export
#'
#' @examples
#' df = data.frame(name=c("a","b","c"),
#'                 value=c("a","b","c"))
#' RCX:::.markRefColumn(df) = "name"
#' 
#' summary(df)
`.markAttributeColumn<-` = function(aspect, value){
  if(value %in% colnames(aspect)){
    rObject = aspect[[value]]
    .addClass(rObject) = "AspectAttributeColumn"
    aspect[[value]] = rObject
  }
  return(aspect)
}



#' Transform an aspect with data type
#' 
#' Transforms an aspect with `value`, `dataType` and `isList` columns to force a custom format
#' in summary generation.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param aspect an aspect (data.frame)
#'
#' @return the aspect (data.frame)
#'
#' @examples
#' df = data.frame(bla=c("a","b","c"),
#'                 value=list("a",2,TRUE),
#'                 dataType=c("string","integer","boolean"),
#'                 isList=c(FALSE,FALSE,FALSE))
#' df = RCX:::.transformVLD(df)
#' 
#' summary(df)
.transformVLD = function(aspect){
  aspect$dataType[aspect$isList] = paste0("List of ",aspect$dataType[aspect$isList])
  value = paste0(toupper(substring(aspect$dataType, 1, 1)), substring(aspect$dataType, 2))
  .addClass(value) = "AspectValueColumn"
  
  aspect$value = value
  aspect$isList = NULL
  aspect$dataType = NULL
  
  return(aspect)
}


#' Transform an aspect with a list as column
#' 
#' Transforms an aspect with a column, that is a list to force a custom format
#' in summary generation. Only show the number of elements in the list elements.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param aspect an aspect (data.frame)
#' @param value character; property 
#'
#' @return the aspect (data.frame)
#'
#' @examples
#' df = data.frame(bla=c("a","b","c"))
#' df$blubb=list(c("a","b","c"),
#'               c(1,2),
#'               c(TRUE,FALSE,FALSE,TRUE,TRUE))
#' 
#' RCX:::.transformListLength(df) = "blubb"
#' 
#' summary(df)
`.transformListLength<-` = function(aspect, value){
  if(value %in% colnames(aspect)){
    col = vapply(aspect[,value], length, integer(1))
    .addClass(col) = "AspectListLengthColumn"
    aspect[,value] = col
  }
  return(aspect)
}


##########################################################################################
## Summary for aspects
##########################################################################################


#' RCX and aspect summary
#' 
#' `summary` is a generic function used to produce result summaries of the [RCX][RCX-object] object.
#' The function invokes particular methods which depend on the class of the first argument.
#' 
#' @details 
#' The form of the returned summary depends on the class of its argument, therefore it is possible to summarize 
#' [RCX][RCX-object] objects and their single aspects.
#' 
#' To enhance readability of the summary, some additional classes have `summary` functions, that are used to show
#' for example ids of an aspect, required and optional references to ids of aspects, or the number of elements in lists.
#'
#' @param object an object; [RCX][RCX-object] object or aspect (or column of data.frame)
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return object summary as list
#'
#' @name summary
#' @export
#'
#' @examples
#' rcx = createRCX(
#'   nodes = createNodes(name = c("a","b","c")),
#'   edges = createEdges(source=1, target=2)
#' )
#' 
#' summary(rcx)
summary.RCX = function(object, ...){
  result = lapply(object, function(aspect){
    summary(aspect)
  })
  
  return(result)
}


#' @rdname summary
#' @export
summary.MetaDataAspect = function(object, ...){
  .transformListLength(object) = "properties"
  
  result = NextMethod(object)
  return(result)
}


#' @rdname summary
#' @export
summary.NodesAspect = function(object, ...){
  object = .summaryAspect(object)
  
  result = NextMethod(object)
  return(result)
}


#' @rdname summary
#' @export
summary.EdgesAspect = function(object, ...){
  object = .summaryAspect(object)
  .markReqRefColumn(object) = "source"
  .markReqRefColumn(object) = "target"
  
  result = NextMethod(object)
  return(result)
}


#' @rdname summary
#' @export
summary.NodeAttributesAspect = function(object, ...){
  object = .summaryAspect(object)
  object = .transformVLD(object)
  
  .markReqRefColumn(object) = "propertyOf"
  .markAttributeColumn(object) = "name"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.EdgeAttributesAspect = summary.NodeAttributesAspect


#' @rdname summary
#' @export
summary.NetworkAttributesAspect = function(object, ...){
  object = .summaryAspect(object)
  object = .transformVLD(object)
  
  .markRefColumn(object) = "subnetworkId"
  .markAttributeColumn(object) = "name"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.CartesianLayoutAspect = function(object, ...){
  object = .summaryAspect(object)
  .markReqRefColumn(object) = "node"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.CyGroupsAspect = function(object, ...){
  object = .summaryAspect(object)
  .markReqRefColumn(object) = "id"
  
  .transformListLength(object) = "nodes"
  .transformListLength(object) = "externalEdges"
  .transformListLength(object) = "internalEdges"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.CyHiddenAttributesAspect = summary.NetworkAttributesAspect


#' @rdname summary
#' @export
summary.CyNetworkRelationsAspect = function(object, ...){
  object = .summaryAspect(object)
  .markReqRefColumn(object) = "child"
  .markRefColumn(object) = "parent"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.CySubNetworksAspect = function(object, ...){
  object = .summaryAspect(object)
  .markReqRefColumn(object) = "id"
  
  .transformListLength(object) = "nodes"
  .transformListLength(object) = "edges"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.CyTableColumnAspect = function(object, ...){
  object = .summaryAspect(object)
  object = .transformVLD(object)
  
  at = object$appliesTo
  .addClass(at) = "AspectValueColumn"
  object$appliesTo = at
  
  .markRefColumn(object) = "subnetworkId"
  
  NextMethod(object)
}


#' @rdname summary
#' @export
summary.CyVisualPropertiesAspect = function(object, ...){
  result = lapply(object, function(aspect){
    summary(aspect)
  })
  return(result)
}

#' @rdname summary
#' @export
summary.CyVisualProperty = function(object, ...){
  av = data.frame(appliesTo=object$appliesTo, view=object$view)
  .markRefColumn(av) = "appliesTo"
  .markRefColumn(av) = "view"
  
  result = list(summary(av))
  properties = object$properties[!vapply(object$properties, is.na, logical(1))]
  if(! all(vapply(object$properties, is.na, logical(1))))
    result$properties=summary(plyr::ldply(properties, data.frame))
  
  dependencies = object$dependencies[!vapply(object$dependencies, is.na, logical(1))]
  if(! all(vapply(object$dependencies, is.na, logical(1))))
    result$dependencies=summary(plyr::ldply(dependencies, data.frame))
  
  mappings = object$mappings[!vapply(object$mappings, is.na, logical(1))]
  if(! all(vapply(object$mappings, is.na, logical(1))))
    result$mappings=summary(plyr::ldply(mappings, data.frame))
  
  return(result)
}


##########################################################################################
## Summary for single columns
##########################################################################################


#' @describeIn summary Summarize an id property
#' @export
summary.AspectIdColumn = function(object, ...){
  result = c(length(object), min(object), max(object))
  resultNames = c("Total", "Min.", "Max.")
  
  names(result) = resultNames
  return(result)
}


#' @describeIn summary Summarize an optional property, that references the ids of an other aspect
#' @export
summary.AspectRefColumn = function(object, ...){
  la = length(object)
  lna = sum(is.na(object))
  
  result = c(la, (la-lna), lna)
  resultNames <- c("Total", "Refered ids", "NAs")
  
  if((la-lna)!=0){
    result = c(result, 
               length(unique(object[!is.na(object)])), 
               min(object[!is.na(object)]), 
               max(object[!is.na(object)]))
    resultNames = c(resultNames, 
                    c("Unique ids", 
                      "Min.", 
                      "Max."))
  }
  
  names(result) = resultNames
  return(result)
}


#' @describeIn summary Summarize a required property, that references the ids of an other aspect
#' @export
summary.AspectReqRefColumn = function(object, ...){
  result = c(length(object), 
             length(unique(object[!is.na(object)])), 
             min(object[!is.na(object)]), 
             max(object[!is.na(object)]))
  resultNames = c("Total",
                  "Unique ids", 
                  "Min.", 
                  "Max.")
  
  names(result) = resultNames
  return(result)
}


#' @describeIn summary Summarize the occurrences of the different elements in the property
#' @export
summary.AspectValueColumn = function(object, ...){
  result = table(object)
  return(result)
}


#' @describeIn summary Summarize the different attributes in the property
#' @export
summary.AspectAttributeColumn = function(object, ...){
  .removeClass(object) = "AspectAttributeColumn"
  result = c(length(object), 
             length(unique(object[!is.na(object)])), 
             class(object))
  resultNames = c("Length",
                  "Unique", 
                  "Class")
  
  names(result) = resultNames
  return(result)
}


#summary.AspectListLengthColumn = function(object, ..., digits, quantile.type = 7){
#' @describeIn summary The property is a list of vectors, so summarize the length of the vectors
#' @export
summary.AspectListLengthColumn = function(object, ...){
  nas = is.na(object)
  object = object[!nas]
  
  result = c(min(object),
             mean(object),
             max(object))
  resultNames = c("Min. #elements ", 
                  "Mean #elements",
                  "Max. #elements")
  
  names(result) = resultNames
  return(result)
}
