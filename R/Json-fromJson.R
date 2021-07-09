################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Read CX from file, parse the JSON and convert it to an [RCX][RCX-object] object
#' 
#' The `readCX` function combines three sub-task:
#' - read the JSON from file
#' - parse the JSON
#' - process the contained aspects to create an [RCX][RCX-object] object
#' 
#' If any errors occur during this process, the single steps can be performed individually.
#' This also allows to skip certain steps, for example if the JSON data is already availabe as text, 
#' there is no need to save it as file and read it again.
#' 
#' ## Read the JSON from file
#' 
#' The `readJSON` function only read the content of a text file and returns it as a simple character vector.
#' 
#' ## Parse the JSON
#' 
#' The `parseJSON` function uses the [jsonlite] package, to parse JSON text:
#'  
#' `jsonlite::fromJSON(cx, simplifyVector = F)`
#' 
#' The result is a list containing the aspect data as elements.
#' If, for some reason, the JSON is not valid, the [jsonlite] package raises an error.
#' 
#' ## Process the contained aspects to create an [RCX][RCX-object] object
#' 
#' With the `processCX` function, the single elements from the previous list will be processed with the [jsonToRCX] functions, 
#' which creating objects for the single aspects.
#' The standard CX aspects are processed by generic functions named by the aspect names of the CX data structure, e.g.
#' `jsonToRCX.nodeAttributes` for the samely named CX aspect the corresponding `NodeAttributesAspect` in [RCX][RCX-object]
#' (see also \code{vignette("02. The RCX and CX Data Model")} or NDEx documentation: \url{https://home.ndexbio.org/data-model/}).
#' 
#' The CX network may contain additional aspects besides the officially defined ones. 
#' This includes self defined or deprecated aspects, that sill can be found in the networks at the NDEx platform.
#' By default, those aspects are simply omitted.
#' In those cases, the setting *verbose* to `TRUE` is a good idea to see, which aspects cannot be processed this package.
#' 
#' Those not processable aspects can be handled individually, but it is advisable to extend the [jsonToRCX] functions by 
#' implementing own versions for those aspects. 
#' Additionally, the **update** functions have to be implemented to add the newly generated aspect objects
#' to [RCX][RCX-object] object (see e.g. [updateNodes] or [updateEdges]).
#' Therefore, the function also have to be named `"update<aspect-name>`, where aspect-name is the capitalized version of the
#' name used in the CX.
#' (see also \code{vignette("03. Extending the RCX Data Model")}
#'
#' @param file character; the name of the file which the data are to be read from
#' @param json character; raw JSON data
#' @param aspectList list; list containing the aspect data (parsed JSON)
#' @param verbose logical; whether to print what is happening
#'
#' @export
#' @seealso [jsonToRCX], [writeCX]
#' 
#' @examples 
#' cxFile = system.file(
#'   "extdata", 
#'   "Imatinib-Inhibition-of-BCR-ABL-66a902f5-2022-11e9-bb6a-0ac135e8bacf.cx", 
#'   package = "RCX"
#' )
#' 
#' rcx = readCX(cxFile)
#' 
#' ## OR:
#' 
#' json = readJSON(cxFile)
#' aspectList = parseJSON(json)
#' rcx = processCX(aspectList)
readCX = function(file, verbose=F){
  json = readJSON(file, verbose)
  aspectList = parseJSON(json, verbose)
  rcx = processCX(aspectList, verbose)
  return(rcx)
}


#' @describeIn readCX Reads the CX/JSON from file and returns the content as text
#' @export
readJSON = function(file, verbose=F){
  if(verbose) cat(paste0("Read file \"",file,'"...'))
  
  fileCon = file(file)
  json = readLines(fileCon, warn = F)
  close(fileCon)
  
  json = paste0(json,collapse = "\n")
  
  .addClass(json) = "json"
  
  if(verbose) cat("done!\n")
  return(json)
}


#' @describeIn readCX Parses the JSON text and returns a list with the aspect data
#' @export
parseJSON = function(json, verbose=F){
  if(verbose) cat(paste0("Parse json..."))
  
  jsonList = jsonlite::fromJSON(json, simplifyVector = F)
  
  if(verbose) cat("done!\n")
  return(jsonList)
}


#' @describeIn readCX Processes the list of aspect data and creates an [RCX][RCX-object]
#' @export
processCX = function(aspectList, verbose=F){
  ## get the names of the aspects
  jsonNames = sapply(aspectList, names)
  
  posAspects = names(.CLS)
  ## rearrange order of the aspects to be processed
  posAspects = posAspects[!posAspects %in% c("nodes", "metaData", "rcx")]
  posAspects = c("nodes", posAspects, "metaData")
  presAspects = posAspects[posAspects %in% jsonNames]
  notPresAspects = jsonNames[! jsonNames %in% presAspects]
  
  rcx = list()
  rawData = list()
  
  for(acc in c(presAspects, notPresAspects)){
    i = match(acc, jsonNames)
    if(acc=="metaData"){
      ## metaData might be composed of pre- and post-metaData
      for(j in i){
        jsonData = aspectList[[j]]
        .addClass(jsonData) = acc
        params = jsonToRCX(jsonData, verbose)
        
        name = params$name
        
        version = params$version
        names(version) = name
        version = version[!is.na(version)]
        
        consistencyGroup = params$consistencyGroup
        if(!is.null(consistencyGroup)){
          names(consistencyGroup) = name
          consistencyGroup = consistencyGroup[!is.na(consistencyGroup)]
        }
        
        properties = params$properties
        if(!is.null(properties)){
          names(properties) = name
          properties = properties[sapply(properties, function(p){
            if(length(p)==0) return(F)
            return(ifelse(all(is.na(p)), F, T))
          })]
        }

        rcx = updateMetaData(rcx, version, consistencyGroup, properties)
      }
      
    }else{ 
      for(j in i){
        jsonData = aspectList[[j]]
        .addClass(jsonData) = acc
        aspect = jsonToRCX(jsonData, verbose)
        
        if(acc=="nodes"){
          if(verbose) cat(paste0("Create RCX from parsed nodes..."))
          rcx = createRCX(aspect)
          if(verbose) cat("done!\n")
        }else{
          if(!is.null(aspect)){
            if(verbose) cat(paste0("Add aspect \"",acc,"\" to RCX..."))
            cname = sub("Aspect", "", .aspectClass(aspect))
            cname = paste0("update",cname)
            rcx = do.call(cname,
                          list(rcx, aspect))
            if(verbose) cat("done!\n")
          }else{
            if(verbose) cat(paste0("Can't process aspect \"",acc,"\", so skip it..."))
            if(length(i)==1){
              rawData[[acc]] = jsonData
            }else{
              tmp = rawData[[acc]]
              if(is.null(tmp)) tmp = list()
              tmp[[length(tmp)+1]] = jsonData
              rawData[[acc]] = tmp
            }
            if(verbose) cat("done!\n")
          }
        }
      }
    }
  }
  
  return(rcx)
}


#' Convert parsed JSON aspects to RCX
#' 
#' Functions to handle parsed JSON for the different aspects.
#' 
#' These functions will be used in \code{\link{processCX}} to process the JSON data for every aspect.
#' Each aspect is accessible in the CX-JSON by a particular accession name (i.e. its aspect name; see NDEx documentation:
#' \url{https://home.ndexbio.org/data-model/}). 
#' This name is used as class to handle different aspects by method dispatch.
#' This simplifies the extension of RCX for non-standard or self-defined aspects.
#' 
#' The CX-JSON is parsed to R data types using the [jsonlite] package as follows:
#' 
#' `jsonlite::fromJSON(cx, simplifyVector = F)`
#' 
#' This results in a list of lists (of lists...) to avoid automatic data type conversions, which affect the correctness and
#' usability of the data. Simplified JSON data for example [NodeAttributes] would be coerced into a data.frame, 
#' therefore the `value` column looses the format for data types other than `string`.
#' 
#' The *jsonData* will be a list with only one element named by the aspect: 
#' `jsonData$<accessionName>`
#' 
#' To access the parsed data for example nodes, this can be done by `jsonData$nodes`.
#' The single aspects are then created using the corresponding **create** functions and combined to an [RCX][RCX-object] 
#' object using the corresponding **update** functions.
#'
#' @param jsonData nested list from parsed JSON 
#' @param verbose logical; whether to print what is happening
#'
#' @return created aspect or `NULL`
#' @export
#' @seealso [rcxToJson], [toCX], [readCX], [writeCX]
#'
#' @examples
#' nodesJD = list(nodes=list(list("@id"=6, name="EGFR"),
#'                           list("@id"=7, name="CDK3")))
#' class(nodesJD) = c("nodes", class(nodesJD))
#' 
#' jsonToRCX(nodesJD, verbose=TRUE)
jsonToRCX = function(jsonData, verbose){
  UseMethod("jsonToRCX", jsonData)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.default = function(jsonData, verbose){
  if(verbose) cat(paste0("Don't know what to do with a \"", names(jsonData), '" aspect!\n'))
  return(NULL)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.status = function(jsonData, verbose){
  if(verbose) cat(paste0("Ignore \"", names(jsonData), '" aspect, not necessary in RCX!\n'))
  return(NULL)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.numberVerification = function(jsonData, verbose){
  if(verbose) cat(paste0("Ignore \"", names(jsonData), '" aspect, not necessary in RCX!\n'))
  return(NULL)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.metaData = function(jsonData, verbose){
  if(verbose) cat("Parsing meta-data...")
  data = jsonData$metaData
  
  result = list(
    name = .jsonV(data, "name"),
    version = .jsonV(data, "version"),
    consistencyGroup = .jsonV(data, "consistencyGroup", returnAllDefault = F),
    properties = .jsonL(data, "properties", unList=F, returnAllDefault = F)
  )

  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.nodes = function(jsonData, verbose){
  if(verbose) cat("Parsing nodes...")
  data = jsonData$nodes
  
  id = .jsonV(data, "@id")
  name = .jsonV(data, "n", returnAllDefault=F)
  represents = .jsonV(data, "r", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createNodes(id,
                       name,
                       represents)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.edges = function(jsonData, verbose){
  if(verbose) cat("Parsing edges...")
  data = jsonData$edges
  
  id = .jsonV(data, "@id")
  source = .jsonV(data, "s")
  target = .jsonV(data, "t")
  interacts = .jsonV(data, "i", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createEdges(id,
                       source,
                       target,
                       interacts)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.nodeAttributes = function(jsonData, verbose){
  if(verbose) cat("Parsing node attributes...")
  data = jsonData$nodeAttributes
  
  propertyOf = .jsonV(data, "po")
  name = .jsonV(data, "n")
  value = .jsonL(data, "v")
  dataType = .json2RDataType(.jsonV(data, "d"))
  subnetworkId = .jsonV(data, "s", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createNodeAttributes(propertyOf,
                                name,
                                value,
                                dataType = dataType$type,
                                isList = dataType$isList,
                                subnetworkId)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.edgeAttributes = function(jsonData, verbose){
  if(verbose) cat("Parsing edge attributes...")
  data = jsonData$edgeAttributes
  
  propertyOf = .jsonV(data, "po")
  name = .jsonV(data, "n")
  value = .jsonL(data, "v")
  dataType = .json2RDataType(.jsonV(data, "d"))
  subnetworkId = .jsonV(data, "s", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createEdgeAttributes(propertyOf,
                                name,
                                value,
                                dataType = dataType$type,
                                isList = dataType$isList,
                                subnetworkId)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.networkAttributes = function(jsonData, verbose){
  if(verbose) cat("Parsing network attributes...")
  data = jsonData$networkAttributes
  
  propertyOf = .jsonV(data, "po")
  name = .jsonV(data, "n")
  value = .jsonL(data, "v")
  dataType = .json2RDataType(.jsonV(data, "d"))
  subnetworkId = .jsonV(data, "s", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createNetworkAttributes(name,
                                   value,
                                   dataType = dataType$type,
                                   isList = dataType$isList,
                                   subnetworkId)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cartesianLayout = function(jsonData, verbose){
  if(verbose) cat("Parsing cartesian layout...")
  data = jsonData$cartesianLayout
  
  node = .jsonV(data, "node")
  x = .jsonV(data, "x")
  y = .jsonV(data, "y")
  z = .jsonV(data, "z", returnAllDefault=F)
  view = .jsonV(data, "view", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createCartesianLayout(node, x, y, z, view)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cyGroups = function(jsonData, verbose){
  if(verbose) cat("Parsing Cytoscape groups...")
  data = jsonData$cyGroups
  
  id = .jsonV(data, "@id")
  name = .jsonV(data, "n")
  nodes = .jsonL(data, "nodes", returnAllDefault=F)
  externalEdges = .jsonL(data, "external_edges", returnAllDefault=F)
  internalEdges = .jsonL(data, "internal_edges", returnAllDefault=F)
  collapsed = .jsonV(data, "collapsed", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createCyGroups(id,
                          name,
                          nodes,
                          externalEdges,
                          internalEdges,
                          collapsed)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cyHiddenAttributes = function(jsonData, verbose){
  if(verbose) cat("Parsing Cytoscape hidden attributes...")
  data = jsonData$cyHiddenAttributes
  
  name = .jsonV(data, "n")
  value = .jsonL(data, "v")
  dataType = .json2RDataType(.jsonV(data, "d"))
  subnetworkId = .jsonV(data, "s", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createCyHiddenAttributes(name,
                                    value,
                                    dataType = dataType$type,
                                    isList = dataType$isList,
                                    subnetworkId)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cyNetworkRelations = function(jsonData, verbose){
  if(verbose) cat("Parsing Cytoscape network relations...")
  data = jsonData$cyNetworkRelations
  
  child = .jsonV(data, "c")
  parent = .jsonL(data, "p", returnAllDefault=F)
  name = .jsonL(data, "name", returnAllDefault=F)
  isView = ifelse(.jsonL(data, "r")=="view", T, F)
  
  if(verbose) cat("create aspect...")
  result = createCyNetworkRelations(child, parent, name, isView)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cySubNetworks = function(jsonData, verbose){
  if(verbose) cat("Parsing Cytoscape subnetworks...")
  data = jsonData$cySubNetworks
  
  id = .jsonV(data, "@id")
  nodes = .jsonL(data, "nodes", returnAllDefault=F)
  edges = .jsonL(data, "edges", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createCySubNetworks(id,
                               nodes,
                               edges)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cyTableColumn = function(jsonData, verbose){
  if(verbose) cat("Parsing Cytoscape table columns...")
  data = jsonData$cyTableColumn
  
  appliesTo = .jsonL(data, "applies_to")
  appliesTo = ifelse(appliesTo=="node_table", "nodes",
                     ifelse(appliesTo=="edge_table", "edges",
                            ifelse(appliesTo=="network_table", "networks",
                            NA)))
  name = .jsonV(data, "n")
  dataType = .json2RDataType(.jsonV(data, "d"))
  subnetworkId = .jsonV(data, "s", returnAllDefault=F)
  
  if(verbose) cat("create aspect...")
  result = createCyTableColumn(appliesTo,
                               name,
                               dataType = dataType$type,
                               isList = dataType$isList,
                               subnetworkId)
  if(verbose) cat("done!\n")
  return(result)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.cyVisualProperties = function(jsonData, verbose){
  if(verbose) cat("Parsing Cytoscape visual properties...")
  data = jsonData$cyVisualProperties
  
  po = .jsonV(data, "properties_of")
  
  appliesTo = .jsonV(data, "applies_to")
  view = .jsonV(data, "view")
  
  vpPP = .jsonL(data, "properties")
  vpPP = lapply(vpPP,function(x){
    if(!all(is.na(x))) x = createCyVisualPropertyProperties(x)
    return(x)
    })
  vpPD = .jsonL(data, "dependencies")
  vpPD = lapply(vpPD,function(x){
    if(!all(is.na(x))) x = createCyVisualPropertyDependencies(x)
    return(x)
  })
  vpPM = .jsonL(data, "mappings", unList=F)
  vpPM = lapply(vpPM,function(x){
    if(!all(is.na(x))) {
      n = names(x)
      t = sapply(x, function(y){y$type})
      d = sapply(x, function(y){y$definition})
      x = createCyVisualPropertyMappings(t, d, n)
    }
    return(x)
  })
  if(verbose) cat("done!\n")
  
  if(verbose) cat("- Create sub-objects...")
  vpP = list()
  for(p in names(.DICT$VPpropertiesOf)){
    tmp = .filterBy(.DICT$VPpropertiesOf[p], "po", 
                    po, appliesTo, view, vpPP, vpPD, vpPM)
    if(length(tmp)!=0) {
      vpP[[p]] = createCyVisualProperty(properties=tmp$vpPP, 
                                        dependencies=tmp$vpPD,
                                        mappings=tmp$vpPM,
                                        appliesTo = tmp$appliesTo,
                                        view = tmp$view)
    }
  }
  if(verbose) cat("done!\n")

  if(verbose) cat("- Create aspect...")
  result = createCyVisualProperties(network=vpP$network,
                                    nodes=vpP$nodes,
                                    edges=vpP$edges,
                                    defaultNodes=vpP$defaultNodes,
                                    defaultEdges=vpP$defaultEdges)
  if(verbose) cat("done!\n")
  return(result)
}
