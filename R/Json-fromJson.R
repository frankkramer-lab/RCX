################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################

#TODO: add examples
#TODO: raw should return all data as parsed json. an additional parameter should return the failed (includeFailed?)
# better: two functions: additional fromJSONlist, which does the work and fromCX calling that one
#' Convert CX to an [RCX][RCX-object] object
#' 
#' This function parses a CX file or text and creates an [RCX][RCX-object] object from it.
#' 
#' The CX-JSON is parsed using the [jsonlite] package, with the [jsonToRCX] functions creating objects for the single aspects.
#' For standard CX aspects are processed by generic functions named by the aspect names of the CX data structure
#' (see NDEx documentation: \url{https://home.ndexbio.org/data-model/}).
#' 
#' The CX network may contain additional aspects besides the officially defined ones. 
#' This includes self defined or deprecated aspects, that sill can be found in the networks at the NDEx platform.
#' By default, those aspects are simply omitted.
#' 
#' In those cases, the setting *verbose* to `TRUE` is a good idea to see, which aspects cannot be processed this package.
#' If *raw* is set to `TRUE`, those aspects are also returned along the [RCX][RCX-object] object in **raw** format, 
#' i.e. as a named list containing the parsed JSON from
#' 
#' `jsonlite::fromJSON(cx, simplifyVector = F)`
#' 
#' The raw data can be handled individually, but is advisable to extend the [jsonToRCX] functions by implementing own versions
#' for those aspects. 
#' 
#' Additionally, the **update** functions have to be implemented to add the newly generated aspect objects
#' to [RCX][RCX-object] object (see e.g. [updateNodes] or [updateEdges]).
#' Therefore, the function also have to be named `"update<aspect-name>`, where aspect-name is the capitalized version of the
#' name used in the CX.
#'
#' @param cx CX file in JSON format
#' @param verbose logical; whether to print what is happening
#' @param raw logical; whether to return unparsable aspects
#'
#' @return [RCX][RCX-object] object; (or a `list(rcx=<`[RCX][RCX-object]`>, raw=<raw>)` if `raw=TRUE`)
#' @export
#' @seealso [toCX], [rcxToJson], [readRCX], [writeCX]
#'
#' @examples
#' NULL
fromCX = function(cx, verbose=F, raw=F){
  json = jsonlite::fromJSON(cx, simplifyVector = F)
  jsonNames = sapply(json, names)
  
  posAspects = names(.CLS)
  ## rearrange order
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
        jsonData = json[[j]]
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
        jsonData = json[[j]]
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
            if(verbose && raw) cat(paste0("Can't process aspect \"",acc,"\", so return it raw..."))
            if(length(i)==1){
              rawData[[acc]] = jsonData
            }else{
              tmp = rawData[[acc]]
              if(is.null(tmp)) tmp = list()
              tmp[[length(tmp)+1]] = jsonData
              rawData[[acc]] = tmp
            }
            if(verbose && raw) cat("done!\n")
          }
        }
      }
    }
  }
  
  if(raw){
    return(list(rcx=rcx, raw=rawData))
  }else{
    return(rcx)
  }
}


#TODO: add example
#' Read CX from file
#' 
#' These function read a CX network from a file.
#' 
#' These functions return either simply the read JSON from the file, or 
#' convert the CX to an [RCX][RCX-object] by calling [fromCX].
#'
#' @param file character; the name of the file which the data are to be read from
#' @param verbose logical; whether to print what is happening
#' @param raw logical; whether to return unparsable aspects (see [fromCX])
#'
#' @export
#' @seealso [fromCX], [jsonToRCX], [writeCX]
#' 
#' @aliases readCX
#' 
#' @examples 
#' NULL
readRCX = function(file, verbose=F, raw=F){
  cx = readCX(file, verbose)
  rcx = fromCX(cx, verbose, raw)
  return(rcx)
}


#' @rdname readRCX
#' @export
readCX = function(file, verbose=F){
  if(verbose) cat(paste0("Read file \"",file,'"...'))
  
  fileCon = file(file)
  cx = readLines(fileCon, warn = F)
  close(fileCon)
  
  cx = paste0(cx,collapse = "\n")
  
  .addClass(cx) = "json"
  .addClass(cx) = "CX"
  
  if(verbose) cat("done!\n")
  return(cx)
}


#' Convert parsed JSON aspects to RCX
#' 
#' Functions to handle parsed JSON for the different aspects.
#' 
#' These functions will be used in \code{\link{fromCX}} to process the JSON data for every aspect.
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
  if(verbose) cat(paste0("Ignore \"", names(jsonData), '" aspect!\n'))
  return(NULL)
}


#' @rdname jsonToRCX
#' @export
jsonToRCX.numberVerification = function(jsonData, verbose){
  if(verbose) cat(paste0("Ignore \"", names(jsonData), '" aspect!\n'))
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
