################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   helping function to determine the dependencies between the aspects.
################################################################################

##########################################################################################
## hasIds
##########################################################################################
## hasIds.default     ==> FALSE
## hasIds.NodesAspect ==> TRUE
##########################################################################################

#' IDs of an aspect
#'
#' This function checks, if an aspect has IDs that may be referenced by other aspects.
#'  
#' By default aspects don't have IDs, so only the implemented classes have IDs.
#' Aspects with IDs will be considered in the meta-data aspect to determine properties like:
#' *idCounter* and *elementCount*.
#'
#' Uses method dispatch, so the default return is *FALSE* and only aspect classes with IDs are
#' implemented. This way it is easier to extend the data model.
#'
#' @param aspect an object of one of the aspect classes (e.g. `r .CLS$nodes`, `r .CLS$edges`, etc.)
#' @return logical
#' 
#' @seealso [idProperty()], [refersTo()], [referedBy()], [maxId()]
#' 
#' @export
#' @examples
#' edges = createEdges(source = c(0,0), target = c(1,2))
#' hasIds(edges)
hasIds = function (aspect) {
  UseMethod("hasIds", aspect)
}


#' @rdname hasIds
#' @export
hasIds.default = function (aspect) {
  return(FALSE)
}


#' @rdname hasIds
#' @export
hasIds.NodesAspect = function (aspect) {
  return(TRUE)
}


#' @rdname hasIds
#' @export
hasIds.EdgesAspect = function (aspect) {
  return(TRUE)
}


#' @rdname hasIds
#' @export
hasIds.CyGroupsAspect = function (aspect) {
  return(TRUE)
}


#' @rdname hasIds
#' @export
hasIds.CySubNetworksAspect = function (aspect) {
  return(TRUE)
}

##########################################################################################
## idProperty
##########################################################################################
## idProperty.default     ==> NULL
## idProperty.NodesAspect ==> "id"
##########################################################################################

#' Name of the property of an aspect that is an ID
#'
#' This function returns the name of the property, if an aspect uses IDs for its elements.
#' As example, the aspect *NodesAspect* has the property *id* that represents the IDs of
#' the aspect. 
#' 
#' By default aspects don't have IDs, so only the implemented classes have IDs.
#' Aspects with IDs will be considered in the meta-data aspect to determine properties like:
#' *idCounter* and *elementCount*.
#'
#' Uses method dispatch, so the default return is *NULL* and only aspect classes with IDs are
#' implemented. This way it is easier to extend the data model.
#'
#' @param aspect an object of one of the aspect classes (e.g. `r .CLS$nodes`, `r .CLS$edges`, etc.)
#' @return character; Name of the ID property or *NULL*
#' 
#' @seealso [hasIds()], [refersTo()], [referedBy()], [maxId()]
#' 
#' @export
#' @examples
#' edges = createEdges(source = c(0,0), target = c(1,2))
#' idProperty(edges)
idProperty = function (aspect) {
  UseMethod("idProperty", aspect)
}


#' @rdname idProperty
#' @export
idProperty.default = function (aspect) {
  return(NULL)
}


#' @rdname idProperty
#' @export
idProperty.NodesAspect = function (aspect) {
  return(.IDProp$nodes)
}


#' @rdname idProperty
#' @export
idProperty.EdgesAspect = function (aspect) {
  return(.IDProp$edges)
}


#' @rdname idProperty
#' @export
idProperty.CyGroupsAspect = function (aspect) {
  return(.IDProp$cyGroups)
}


#' @rdname idProperty
#' @export
idProperty.CySubNetworksAspect = function (aspect) {
  return(.IDProp$cySubNetworks)
}


##########################################################################################
## refersTo
##########################################################################################
## refersTo.default ==> NULL
## refersTo.EdgesAspect ==> c(source="NodesAspect", target="NodesAspect")
##########################################################################################

#' Name of the property of an aspect that is an ID
#'
#' This function returns the name of the property and the aspect class it refers to.
#' As example, the aspect *EdgesAspect* has the property *source* that refers to the *ids* of
#' the *NodesAspect* aspect.
#'
#' Uses method dispatch, so the default return is *NULL* and only aspect classes that refer to other
#' aspects are implemented. This way it is easier to extend the data model.
#'
#' @param aspect an object of one of the aspect classes (e.g. `r .CLS$nodes`, `r .CLS$edges`, etc.)
#' @return named list; Name of the refering property and aspect class name.
#' 
#' @seealso [hasIds()], [idProperty()], [referedBy()], [maxId()]
#' 
#' @export
#' @examples
#' edges = createEdges(source = c(0,0), target = c(1,2))
#' refersTo(edges)
refersTo = function (aspect) {
  UseMethod("refersTo", aspect)
}


#' @describeIn refersTo of default returns *NULL*
#' @export
refersTo.default = function (aspect) {
  return(NULL)
}


#' @describeIn refersTo of `r .CLS$edges` refers to `r .IDProp$nodes` by *source* and *target*
#' @export
refersTo.EdgesAspect = function (aspect) {
  return(c(source=.CLS$nodes, 
           target=.CLS$nodes))
}


#' @describeIn refersTo of `r .CLS$nodeAttributes` refers to `r .IDProp$nodes` by *propertyOf* and to `r .IDProp$cySubNetworks` by *subnetworkId*
#' @export
refersTo.NodeAttributesAspect = function (aspect) {
  result = c(propertyOf=.CLS$nodes)
  if(!is.null(aspect$subnetworkId)) result["subnetworkId"]=.CLS$cySubNetworks
  return(result)
}


#' @describeIn refersTo of `r .CLS$edgeAttributes` refers to `r .IDProp$edges` by *propertyOf* and to `r .IDProp$cySubNetworks` by *subnetworkId*
#' @export
refersTo.EdgeAttributesAspect = function (aspect) {
  result = c(propertyOf=.CLS$edges)
  if(!is.null(aspect$subnetworkId)) result["subnetworkId"]=.CLS$cySubNetworks
  return(result)
}


#' @describeIn refersTo of `r .CLS$cartesianLayout` refers to `r .IDProp$nodes` by *node* and to `r .IDProp$cySubNetworks` by *view*
#' @export
refersTo.CartesianLayoutAspect = function (aspect) {
  result = c(node=.CLS$nodes)
  if(!is.null(aspect$view)) result["view"]=.CLS$cySubNetworks
  return(result)
}


#' @describeIn refersTo of `r .CLS$cyGroups` refers to `r .IDProp$nodes` by *nodes* and to `r .IDProp$edges` by *externalEdges* and *internalEdges*
#' @export
refersTo.CyGroupsAspect = function (aspect) {
  result = c()
  if(!is.null(aspect$nodes)) result["nodes"]=.CLS$nodes
  if(!is.null(aspect$externalEdges)) result["externalEdges"]=.CLS$edges
  if(!is.null(aspect$internalEdges)) result["internalEdges"]=.CLS$edges
  
  return(result)
}


#' @describeIn refersTo of `r .CLS$cyVisualProperties` refers to id by *appliesTo* of the sub-aspects
#' @export
refersTo.CyVisualPropertiesAspect = function (aspect) {
  result = c()
  for(po in names(aspect)) {
    if(any(!is.na(aspect[[po]]$appliesTo))) result[po] = .CLS[[.VPref[[po]]]]
  }
  return(result)
}


#' @describeIn refersTo of `r .CLS$cySubnetworks` refers to `r .IDProp$nodes` by *nodes* and to `r .IDProp$edges` by *edges*
#' @export
refersTo.CySubNetworksAspect = function (aspect) {
  result = c()
  if(!is.null(aspect$nodes)) result["nodes"]=.CLS$nodes
  if(!is.null(aspect$edges)) result["edges"]=.CLS$edges
  
  return(result)
}


##########################################################################################
## referedBy
##########################################################################################
## referedBy.default ==> NULL
## referedBy.RCX ==> list(NodesAspect=c("EdgesAspect", "NodeAttributesAspect"),
##                        EdgesAspect=c("EdgeAttributesAspect"))     
##                   # only show present aspects
##########################################################################################

#' List the aspects that are refered by an other aspect
#'
#' This function returns a list of all aspects with all present aspects, that refer to it.
#' As example, the aspect *NodesAspect* is refered by the property *source* and *target* of 
#' the *EdgesAspect* aspect.
#' 
#' @note Uses [hasIds()] and [refersTo()] to determine the refering aspects.
#'
#' @param rcx an object of one of the aspect classes (e.g. `r .CLS$nodes`, `r .CLS$edges`, etc.)
#' @return named list; Aspect class names with names of aspect classes, that refer to them.
#' 
#' @seealso [hasIds()], [idProperty()], [refersTo()], [maxId()]
#' 
#' @export
#' @examples
#' nodes = createNodes(name = c("ĆDK1","CDK2","CDK3"))
#' edges = createEdges(source = c(0,0), target = c(1,2))
#' rcx = createRCX(nodes = nodes, edges = edges)
#' 
#' referedBy(rcx)
referedBy = function (rcx) {
  fname="referedBy"
  if(missing(rcx)) .stop("paramMissingRCX")
  .checkClass(rcx, .CLS$rcx, .formatO("rcx",fname))
  
  result = list()
  
  for(aspect in rcx){
    ref = refersTo(aspect)
    
    ## e.g. refersTo(edges):
    ##  source="NodesAspect" 
    ##  target="NodesAspect" 
    for (r in names(ref)) {
      if(is.null(result[[ref[r]]])) result[[ref[r]]]=c()
      result[[ref[r]]]=unique(c(result[[ref[r]]], .aspectClass(aspect)))
    }
  }
  return(result)
}


##########################################################################################
## maxId
##########################################################################################
## maxId.default ==> integer or NULL
## maxId.RCX ==> list(NodesAspect=<integer>,
##                    EdgesAspect=<integer>)     
##               # only show present aspects
##########################################################################################

#' Highest ID of an aspect
#'
#' This function returns the highest id used in an aspect, that has ids.
#' As example, the aspect *NodesAspect* has the property *id* that must be a unique positive integer.
#'
#' Uses method dispatch, so the default return is *NULL* and only aspect classes that have ids
#' are implemented. This way it is easier to extend the data model. 
#'
#' @param x an object of one of the aspect classes (e.g. `r .CLS$nodes`, `r .CLS$edges`, etc.) or [RCX][RCX-object] class.
#' @return integer; Highest id. For [RCX][RCX-object] objects all highest ids are returned in the vector named by the aspect class.
#' 
#' @seealso [hasIds()], [idProperty()], [refersTo()], [referedBy()], [maxId()]
#' 
#' @export
#' @examples
#' nodes = createNodes(name = c("ĆDK1","CDK2","CDK3"))
#' maxId(nodes)
maxId = function(x){
  UseMethod("maxId", x)
}

#' @rdname maxId
#' @export
maxId.default = function(x){
  aspect = x
  if(hasIds(aspect)){
    return(max(aspect[idProperty(aspect)]))
  }
  return(NULL)
}

#' @rdname maxId
#' @export
maxId.RCX = function(x){
  rcx = x
  result = c()
  
  for(aspect in names(rcx)){
    mId = maxId(rcx[[aspect]])
    if(!is.null(mId)) result[.aspectClass(rcx[[aspect]])]=mId
  }
  return(result)
}


##########################################################################################
## countElements
##########################################################################################
## countElements.default ==> integer
## countElements.MetaDataAspect ==> NA
## countElements.RCX ==> c(NodesAspect=<integer>,
##                         EdgesAspect=<integer>)     
##                       # only show present aspects
##########################################################################################


#' Number of elements in aspect
#'
#' This function returns the number of elements in an aspect.
#'
#' Uses method dispatch, so the default methods already returns the correct number for the most aspect classes.
#' This way it is easier to extend the data model. 
#' 
#' There are only two exceptions in the core and Cytoscape aspects: \code{\link{Meta-data}} and \code{\link{CyVisualProperties}}.
#' 
#' \code{\link{Meta-data}} is a meta-aspect and therefore not included in \code{\link{Meta-data}}, and so its return is `NA`.
#' 
#' \code{\link{CyVisualProperties}} is the only aspect with a complex data structure beneath. 
#' Therefore its number of elements is just the number of how many of the following properties are set:
#' `network`, `nodes`, `edges`, `defaultNodes` or `defaultEdges`. 
#'
#' @param x an object of one of the aspect classes (e.g. \code{\link{Nodes}}) or [RCX][RCX-object] class.
#' @return integer; number of elements. For RCX objects all counts are returned in the vector named by the aspect class.
#' 
#' @seealso [hasIds()], [idProperty()], [refersTo()], [referedBy()], [maxId()]
#' 
#' @export
#' @examples
#' nodes = createNodes(name = c("ĆDK1","CDK2","CDK3"))
#' edges = createEdges(source = c(0,0), target = c(1,2))
#' rcx = createRCX(nodes = nodes, edges = edges)
#' 
#' countElements(nodes)
#' 
#' countElements(rcx)
countElements = function(x){
  UseMethod("countElements", x)
}

#' @rdname countElements
#' @export
countElements.default = function(x){
  aspect = x
  
  if(hasIds(aspect)){
    return(length(aspect[[idProperty(aspect)]]))
  }else{
    return(nrow(aspect))
  }
  return(0)
}

#' @rdname countElements
#' @export
countElements.RCX = function(x){
  rcx = x
  result = c()
  
  for(aspect in names(rcx)){
    count = countElements(rcx[[aspect]])
    if(!is.null(count)) result[.aspectClass(rcx[[aspect]])]=count
  }
  return(result)
}

#' @rdname countElements
#' @export
countElements.CyVisualPropertiesAspect = function(x){
  return(length(x))
}

#' @rdname countElements
#' @export
countElements.MetaDataAspect = function(x){
  return(NA)
}


##########################################################################################
## Conversion between RCX accession name and class name
##########################################################################################

#TODO: usfull???
#' Convert aspect class name to RCX accession
#' 
#' The aspects in an RCX object are accessed by a `name` and return the aspect as an object of `cls`.
#' To simplify the conversion between those, these functions return the corresponding name.
#' 
#' @details 
#' The following accessions/classes are available within the standard RCX implementation:
#' 
#' **accession name <=> class name**
#' `r paste0("```",paste(names(.CLS)," <=> ", .CLS, collapse="\n"),"```")`
#'
#' @param name character; name of the RCX accession of the Aspect
#' @param cls character; name of the aspect class 
#'
#' @return accession or class name
#' @name Convert-Names-and-Classes
#' @export
#'
#' @examples
#' aspectName2Class("nodes")
#' ##[1] "NodesAspect"
#' aspectClass2Name("NodesAspect")
#' ##[1] "nodes"
aspectName2Class = function(name){
  result = NA
  if(name %in% .CLSnames){
    result = .CLS[[name]]
  }
  return(result)
}

#' @rdname Convert-Names-and-Classes
#' @export
aspectClass2Name = function(cls){
  result = NA
  if(cls %in% .CLS){
    result = .CLSnames[[cls]]
  }
  return(result)
}


