################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Convert an [RCX][RCX-object] object to CX (JSON)
#' 
#' This function converts an [RCX][RCX-object] object to JSON in a valid CX data structure 
#' (see NDEx documentation: \url{https://home.ndexbio.org/data-model/}).
#' 
#' The single aspects of the [RCX][RCX-object] object are processed by generic functions of [rcxToJson] for each aspect class.
#' Therefore, not only the single aspects are converted to JSON, but also necessary additional aspects are added,
#' so the resulting CX is accepted by the NDEx platform (\url{https://ndexbio.org/}):
#'  - *numberVerication* shows the supported maximal number
#'  - *status* is needed at the end to show, that no errors have occurred while creation 
#'  
#' If the [RCX][RCX-object] object contains additional aspects besides the officially defined ones,
#' the corresponding [rcxToJson] functions for those aspect classes have to be implemented in order to include
#' them in the resulting CX.
#'
#' @param rcx [RCX][RCX-object] object
#' @param verbose logical; whether to print what is happening 
#' @param pretty logical; adds indentation whitespace to JSON output. 
#' Can be TRUE/FALSE or a number specifying the number of spaces to indent. See [jsonlite::prettify()]
#'
#' @return CX (JSON) text
#' @export
#' @seealso [toCX], [rcxToJson], [readCX], [writeCX]
#'
#' @examples
#' rcx = createRCX(
#'   nodes = createNodes(
#'     name = LETTERS[seq_len(10)]
#'   ),
#'   edges = createEdges(
#'     source=c(1,2),
#'     target = c(2,3)
#'   )
#' )
#' 
#' json = toCX(rcx, pretty=TRUE)
toCX = function(rcx, verbose=FALSE, pretty=FALSE){
  fname = "toCX"
  if(missing(rcx)) .stop("paramMissingRCX")
  .checkClass(rcx, .CLS$rcx, "rcx", fname)
  
  aspects = names(rcx)
  #firstly print all official aspects in the order given by .CLS
  ordered = names(.CLS)
  ordered = ordered[ordered %in% aspects]
  remaining = aspects[!aspects %in% ordered]
  
  failed = c()
  result = paste0('{"numberVerification":[{"longNumber":',.Machine$integer.max,'}]}')

  for(n in ordered){
    # cat(paste0("[[",n,"]] = "))
    if(verbose) cat(paste0("Processing standard aspects: RCX$",n,":\n"))
    json = rcxToJson(rcx[[n]], verbose)
    if(is.null(json)){
      failed = c(failed, n)
    }else{
      result = c(result, json)
    }
  }
  
  ## then process all remaining (self defined)
  for(n in remaining){
    if(verbose) cat(paste0("Processing non-standard aspects: RCX$",n,":\n"))
    json = rcxToJson(rcx[[n]], verbose)
    if(is.null(json)){
      failed = c(failed, n)
    }else{
      result = c(result, json)
    }
  }
  if(verbose) {
    cat("Done with RCX!\n")
    if(length(failed)!=0) cat(paste0("Failed:\n",paste("\t-",failed,collapse = "\n")))
  }
  
  result = c(result, '{"status":[{"error":"","success":true}]}')
  
  result = paste0(result, collapse = ",")
  result = paste0("[",result,"]")
  
  if(pretty){
    result = jsonlite::prettify(result)
  }else{
    class(result) = "json"
  }
  .addClass(result) = "CX"
  
  return(result)
}


#' Write RCX to file
#' 
#' These function write an [RCX][RCX-object] object or a [CX][readCX] object to a file.
#'
#' @param x [RCX][RCX-object] or [CX][readCX] object
#' @param file character; the name of the file to which the data are written
#' @param verbose logical; whether to print what is happening
#' @param pretty logical; adds indentation whitespace to JSON output. 
#' Can be TRUE/FALSE or a number specifying the number of spaces to indent. See [jsonlite::prettify()]
#' 
#' @return file character; the name of the file to which the data were written
#'
#' @export
#' @seealso [toCX], [rcxToJson], [readCX]
#' 
#' @examples
#' NULL
writeCX = function(x, file, verbose=FALSE, pretty=FALSE){
  UseMethod("writeCX", x)
}


#' @rdname writeCX
#' @export
writeCX.RCX = function(x, file, verbose=FALSE, pretty=FALSE){
  cx = toCX(x, verbose, pretty)
  res = writeCX(cx, file, verbose, pretty)
  return(res)
}

#' @rdname writeCX
#' @export
writeCX.CX = function(x, file, verbose=FALSE, pretty=FALSE){
  if(pretty) x = jsonlite::prettify(x)
  fileCon = file(file)
  writeLines(x, fileCon)
  close(fileCon)
  return(file)
}



#' Convert RCX aspects to JSON
#' 
#' Functions for converting the different aspects to JSON following the CX data structure definition
#' (see NDEx documentation: \url{https://home.ndexbio.org/data-model/}).
#' 
#' For converting [RCX][RCX-object] objects to JSON, each aspect is processed by a generic function for its
#' aspect class. Those functions return a character only containing the JSON of this aspect, which is then
#' combined by [toCX] to be a valid CX data structure.
#' 
#' To support the conversion for non-standard or own-defined aspects, generic functions for those aspect classes
#' have to be implemented.
#'
#' @param aspect aspects of an [RCX][RCX-object] object
#' @param verbose logical; whether to print what is happening 
#' @param propertyOf character; provide propertyOf (only necessary for [CyVisualProperty])
#' @param ... additional parameters, that might needed for extending
#'
#' @return character; JSON of an aspect
#' @export
#' @seealso [toCX], [writeCX], [jsonToRCX], [readCX]
#'
#' @examples
#' nodes = createNodes(name = c("a","b","c","d","e","f"))
#' rcxToJson(nodes)
rcxToJson = function(aspect, verbose=FALSE, ...){
  UseMethod("rcxToJson", aspect)
}


#' @rdname rcxToJson
#' @export
rcxToJson.default = function(aspect, verbose=FALSE, ...){
  if(verbose) cat(paste0("Don't know what to do with a \"", .aspectClass(aspect), '" aspect!\n'))
  return(NULL)
}


#' @rdname rcxToJson
#' @export
rcxToJson.MetaDataAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert meta-data to JSON...")
  
  if("properties" %in% colnames(aspect)){
    prop = aspect$properties
    prop = vapply(prop, function(p){
      p = ifelse(is.null(p),
                 NA,
                 .convert2json(p, byElement=TRUE))
      return(p)
    },character(1))
    aspect$properties = prop
  }
  
  json = .convert2json(aspect, raw=c("properties"), skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "metaData")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.NodesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert nodes to JSON...")

  map = c(id="@id", 
          name="n", 
          represents="r")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "nodes")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.EdgesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert edges to JSON...")
  
  map = c(id="@id", 
          source="s", 
          target="t",
          interaction="i")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "edges")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.NodeAttributesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert node attributes to JSON...")
  
  aspect$d = .convertDataTypes(aspect)
  aspect$value = .convertValues(aspect)
  
  aspect$dataType = NULL
  aspect$isList = NULL

  map = c(propertyOf="po",
          name="n",
          value="v",
          subnetworkId="s")
  aspect = .renameDF(aspect, map)

  json = .convert2json(aspect, raw=c("v"),skipNa=TRUE)

  json = .addAspectNameToJson(json, "nodeAttributes")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.EdgeAttributesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert edge attributes to JSON...")

  aspect$d = .convertDataTypes(aspect)
  aspect$value = .convertValues(aspect)
  
  aspect$dataType = NULL
  aspect$isList = NULL
  
  map = c(propertyOf="po",
          name="n",
          value="v",
          subnetworkId="s")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, raw=c("v"),skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "edgeAttributes")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.NetworkAttributesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert network attributes to JSON...")

  aspect$d = .convertDataTypes(aspect)
  aspect$value = .convertValues(aspect)
  
  aspect$dataType = NULL
  aspect$isList = NULL
  
  map = c(name="n",
          value="v",
          subnetworkId="s")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, raw=c("v"),skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "networkAttributes")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CartesianLayoutAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert cartesian layout to JSON...")
  
  json = .convert2json(aspect, skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "cartesianLayout")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyGroupsAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert Cytoscape groups to JSON...")
  
  aspect$nodes = .convertRawList(aspect$nodes)
  aspect$externalEdges = .convertRawList(aspect$externalEdges)
  aspect$internalEdges = .convertRawList(aspect$internalEdges)
  
  map = c(id="@id",
          name="n")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, raw=c("nodes","externalEdges","internalEdges"), skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "cyGroups")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyHiddenAttributesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert Cytoscape hidden attributes to JSON...")
  
  aspect$d = .convertDataTypes(aspect)
  aspect$value = .convertValues(aspect)
  
  aspect$dataType = NULL
  aspect$isList = NULL
  
  map = c(name="n",
          value="v",
          subnetworkId="s")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, raw=c("v"),skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "cyHiddenAttributes")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyNetworkRelationsAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert Cytoscape network relations to JSON...")

  aspect$v = ifelse(aspect$isView, "view", NA)
  aspect$isView = NULL
  
  map = c(child="c",
          parent="p")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "cyNetworkRelations")
  if(verbose) cat("done!\n")
  return(json)
}

#TODO: node=all: "nodes":["all"] or "nodes":"all"???
## ask NDEx since no network with this was found
#' @rdname rcxToJson
#' @export
rcxToJson.CySubNetworksAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert Cytoscape subnetworks to JSON...")
  
  aspect$nodes = .convertRawList(aspect$nodes)
  aspect$edges = .convertRawList(aspect$edges)
  
  map = c(id="@id")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, raw=c("nodes","edges"), skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "cySubNetworks")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyTableColumnAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert Cytoscape table column to JSON...")
  
  aspect$d = .convertDataTypes(aspect)
  aspect$dataType = NULL
  aspect$isList = NULL
  
  aspect$appliesTo = vapply(aspect$appliesTo, function(a){
    return(names(.DICT$TCappliesTo[.DICT$TCappliesTo==a]))
  }, character(1))
  
  map = c(appliesTo="applies_to",
          name="n",
          subnetworkId="s")
  aspect = .renameDF(aspect, map)
  
  json = .convert2json(aspect, skipNa=TRUE)
  
  json = .addAspectNameToJson(json, "cyTableColumn")
  if(verbose) cat("done!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyVisualPropertiesAspect = function(aspect, verbose=FALSE, ...){
  if(verbose) cat("Convert Cytoscape visual properties to JSON...\n")
  
  result = vapply(names(aspect), 
                  function(an){rcxToJson(aspect[[an]], verbose, an)},
                  character(1))
  
  json = paste0(result, collapse = ",")
  
  json = paste0("[",json,"]")
  
  json = .addAspectNameToJson(json, "cyVisualProperties")
  if(verbose) cat("Done with Cytoscape visual properties!\n")
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyVisualProperty = function(aspect, verbose=FALSE, propertyOf="", ...){
  if(verbose) cat(paste0("- Convert Cytoscape visual property of ",propertyOf," to JSON...\n"))
  
  result = c()
  no = max(vapply(aspect, length, integer(1)))
  for(n in seq_len(no)){
    tmp = paste0('"properties_of":"',.DICT$VPpropertiesOf[propertyOf],'"')
    
    if(("appliesTo" %in% names(aspect))&&(!is.na(aspect$appliesTo[n]))){
      tmp = c(tmp,
              paste0('"applies_to":',aspect$appliesTo[n]))
    }
    if(("view" %in% names(aspect))&&(!is.na(aspect$view[n]))){
      tmp = c(tmp,
              paste0('"view":',aspect$view[n]))
    }
    if(("properties" %in% names(aspect))&&(all(!is.na(aspect$properties[[n]])))){
      tmp = c(tmp,
              rcxToJson(aspect$properties[[n]], verbose))
    }
    if(("dependencies" %in% names(aspect))&&(all(!is.na(aspect$dependencies[[n]])))){
      tmp = c(tmp,
              rcxToJson(aspect$dependencies[[n]], verbose))
    }
    if(("mappings" %in% names(aspect))&&(all(!is.na(aspect$mappings[[n]])))){
      tmp = c(tmp,
              rcxToJson(aspect$mappings[[n]], verbose))
    }
    
    tmp = paste0(tmp, collapse = ",")
    tmp = paste0("{",tmp,"}")
    result = c(result, tmp)
  }
  
  json = paste0(result, collapse = ",")
  
  if(verbose) cat(paste0("  Done with Cytoscape visual property of ",propertyOf,"!\n"))
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyVisualPropertyProperties = function(aspect, verbose=FALSE, ...){
  if(verbose) cat(paste0("  - Convert Properties to JSON..."))
  
  aspect$name = .convert2json(aspect$name)
  aspect$value = .convert2json(aspect$value)
  
  json = paste0(aspect$name,":",aspect$value, collapse = ",")
  json = paste0('"properties":{',json,"}")
  
  if(verbose) cat(paste0("done!\n"))
  return(json)
}


#' @rdname rcxToJson
#' @export
rcxToJson.CyVisualPropertyDependencies = function(aspect, verbose=FALSE, ...){
  if(verbose) cat(paste0("  - Convert Dependencies to JSON..."))
  
  aspect$name = .convert2json(aspect$name)
  aspect$value = .convert2json(aspect$value)
  
  json = paste0(aspect$name,":",aspect$value, collapse = ",")
  json = paste0('"dependencies":{',json,"}")
  
  if(verbose) cat(paste0("done!\n"))
  return(json)
}

#' @rdname rcxToJson
#' @export
rcxToJson.CyVisualPropertyMappings = function(aspect, verbose=FALSE, ...){
  if(verbose) cat(paste0("  - Convert Mappings to JSON..."))
  
  aspect$name = .convert2json(aspect$name)
  aspect$type = .convert2json(aspect$type)
  aspect$definition = .convert2json(aspect$definition)
  
  json = paste0(aspect$name,':{"type":',aspect$type,',"definition":',aspect$definition,"}", collapse = ",")
  json = paste0('"mappings":{',json,"}")
  
  if(verbose) cat(paste0("done!\n"))
  return(json)
}
