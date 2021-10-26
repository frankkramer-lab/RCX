################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Conversion from and to igraph
################################################################################


#' Add attribute data to an igraph object
#' 
#' Not only simply add the name-value pairs, but also:
#' - unlist lists if indicated by isList column
#' - renames name="name" to "attribute$name"
#' - puts subnetwork id at the and of the attribute name
#' - adds a data type as attribute$dataType if not string, boolean or double
#'
#' @param ig igraph object
#' @param attributeRef reference name; "node", "edge" or "network"
#' @param attribute an attribute aspect
#'
#' @return igraph object
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @examples
#' NULL
.addAttributeData = function(ig, attributeRef, attribute){
    if("dataType" %in% names(attribute)){
        attribute$dataType[attribute$dataType == "string"] = NA
        attribute$dataType[attribute$dataType == "boolean"] = NA
        attribute$dataType[attribute$dataType == "double"] = NA
    }
    
    ## process different subnetworks and list types differently
    cols = c("name", "isList", "subnetworkId")
    cols = cols[cols %in% colnames(attribute)]
    attNames = unique(attribute[cols])
    
    if(nrow(attNames)!=0){
        for(ri in seq_len(length(attNames))) {
            attrNewName = attNames[ri,"name"]
            attrName = attrNewName
            attrList = attNames[ri,"isList"]
            attrSub = attNames[ri,"subnetworkId"]
            ## rename the property if it is present in the aspect
            if(attrName == "name") attrNewName = "attribute...name"
            if(attributeRef == "node"){
                if(attrName == "represents") attrNewName = "attribute...represents"
            }
            if(attributeRef == "edge"){
                if(attrName == "interacts") attrNewName = "attribute$interacts"
            }
            ## select the data based on unique values
            selData = attribute
            
            if("subnetworkId" %in% colnames(attribute)) {
                if(is.na(attrSub)){
                    selData = selData[is.na(selData$subnetworkId),]
                }else{
                    ## separate the properties for subnetworks
                    if(!is.na(attrSub)) attrNewName = paste0(attrNewName,"...",attrSub)
            
                    selData = selData[!is.na(selData$subnetworkId),]
                    selData = selData[selData$subnetworkId == attrSub,]
                }
            }
            
            if("isList" %in% colnames(attribute)) selData = selData[selData$isList == attrList,]
            
            selData = selData[selData$name == attrName,]
            
            values = selData$value
            if(!attrList) values = unlist(values)
            
            if(attributeRef == "node"){
                ids = selData$propertyOf
                igIds = match(ids, igraph::V(ig)$id)
                ig = igraph::set_vertex_attr(ig, attrNewName, igIds, values)
                
                ## add data type if necessary
                if(("dataType" %in% names(attribute)) && (any(!is.na(selData$dataType)))) {
                    ig = igraph::set_vertex_attr(ig, paste0(attrNewName,"...dataType"), igIds, selData$dataType)
                }
            }
            
            if(attributeRef == "edge"){
                ids = selData$propertyOf
                igIds = match(ids, igraph::E(ig)$id)
                ig = igraph::set_edge_attr(ig, attrNewName, igIds, values)
                
                ## add data type if necessary
                if(("dataType" %in% names(attribute)) && (any(!is.na(selData$dataType)))) {
                    ig = igraph::set_edge_attr(ig, paste0(attrNewName,"...dataType"), igIds, selData$dataType)
                }
            }
            
            if(attributeRef == "network"){
                ig = igraph::set_graph_attr(ig, attrNewName, values)
                
                ## add data type if necessary
                if(("dataType" %in% names(attribute)) && (any(!is.na(selData$dataType)))) {
                    ig = igraph::set_graph_attr(ig, paste0(attrNewName,"...dataType"), selData$dataType)
                }
            }
        }
    }
    
    return(ig)
}


getAttributeData = function(attributeRef, igAttributes){
    
}

##########################################################################################
## iGraph
##########################################################################################


#' Convert an RCX object from and to an igraph object
#' 
#' Convert an [RCX][RCX-object] object to an [igraph][igraph::igraph()] object
#' 
#' @details 
#' In the [igraph][igraph::igraph()] object the attributes are not separated from the graph like in [RCX][RCX-object].
#' Therefore, for converting an [RCX][RCX-object] object to an [igraph][igraph::igraph()] object, and back,
#' some adjustments in the naming of the attributes have to be made.
#' 
#' For nodes the `name` can be present in the [nodes][Nodes] aspect, as name in the [nodeAttributes][NodeAttributes] aspect.
#' Also `name` is used in [igraph][igraph::igraph()] for naming the vertices.
#' To avoid collisions in the conversion, the [nodes][Nodes] name is saved in [igraph][igraph::igraph()] as `nodeName`, 
#' while the [nodeAttributes][NodeAttributes] property `name` is saved as `"attribute...name"`.
#' These names are also used for the conversion back to [RCX][RCX-object], but here the `name` used in the 
#' [nodes][Nodes] aspect can be changed by the `nodeName` parameter.
#' 
#' Similar to the node name, if `"represents"` is present as property in [nodeAttributes][NodeAttributes] its name is changed to
#' `"attribute...represents"`.
#' 
#' The conversion of [edges][Edges] works analogously:
#' If `"interaction"` is present as property in [edgeAttributes][EdgeAttributes] its name is changed to `"attribute...interaction"`.
#' 
#' [Nodes] and [edges][Edges] must have IDs in the [RCX][RCX-object], but not in the [igraph][igraph::igraph()] object.
#' To define an [vertex][igraph::vertex_attr] or [edge][igraph::vertex_attr] attribute to be used as ID, the parameters
#' `nodeId` and `edgeId` can be used to define ether an attribute name (default:"id") or set it to `NULL` to generate ID automatically.
#' 
#' The attributes also may have a special data type assigned.
#' The data type then is saved by adding `"...dataType"` to the attribute name.
#' 
#' The [cartesian layout][CartesianLayout] is also stored in the [igraph][igraph::igraph()] object.
#' To make those [igraph vertex attributes][igraph::vertex_attr_names()] distinguishable from [nodeAttributes][NodeAttributes]
#' they are named `"cartesianLayout...x"`, `"cartesianLayout...y"` and `"cartesianLayout...z"`.
#' 
#' In the [RCX][RCX-object] attributes it is also possible to define a [subnetwork][CySubNetworks], to which an attribute applies.
#' Those attributes are added with `"...123"` added to its name, where `"123"` is the [subnetwork id][CySubNetworks].
#' The [subnetwork id][CySubNetworks] itself are added as [igraph graph attributes][igraph::graph_attr_names()], and are named
#' `subnetwork...123...nodes"` and `"subnetwork...123...edges"`, where `"123"` is the [subnetwork id][CySubNetworks].
#' 
#' Altogether, the conventions look as follows:
#' `"[attribute...]<name>[...<subnetwork>][...dataType]"`
#' 
#'
#' @param rcx [RCX][RCX-object] object
#' @param directed logical; whether the graph is directed
#'
#' @return [igraph][igraph::igraph()] or [RCX][RCX-object] object
#' @export
#' @seealso [graphNEL]
#' 
#' @name Igraph
#'
#' @example man-roxygen-examples/CX_load.R
#' @example man-roxygen-examples/igraph.R
toIgraph = function(rcx, directed=FALSE){
    fname = "toIgraph"
    if(missing(rcx)) .stop("paramMissingRCX")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    
    aspects = names(rcx)
    ig = NULL
    if(! "edges" %in% aspects) .stop("igraphEdgesRequired", fname)
    edges = rcx$edges
    ## reordering columns so that source and target are the first two
    edgeNames = colnames(edges)
    edgeNames = c("source","target", 
                  (edgeNames[!edgeNames %in% c("source","target")]))
    edges = edges[edgeNames]
    ## rename columns to use id as name
    nodes = rcx$nodes
    nodes$nodeName = nodes$name
    nodes$name = nodes$id
    
    ig = igraph::graph_from_data_frame(edges,
                                       directed = directed,
                                       vertices = nodes)
    ig = igraph::set_vertex_attr(ig, "id", value = igraph::vertex_attr(ig, "name"))
    
    if("nodeAttributes" %in% aspects){
        attributes = rcx$nodeAttributes
        ig = .addAttributeData(ig, "node", attributes)
    }
    
    if("edgeAttributes" %in% aspects){
        attributes = rcx$edgeAttributes
        ig = .addAttributeData(ig, "edge", attributes)
    }
    
    if("networkAttributes" %in% aspects){
        attributes = rcx$networkAttributes
        ig = .addAttributeData(ig, "network", attributes)
    }
    
    if("cySubNetworks" %in% aspects){
        subNetworks = rcx$cySubNetworks
        for (subNetI in seq_len(nrow(subNetworks))) {
            subNet = subNetworks[subNetI,]
            if(!is.null(subNet$nodes)){
                ig = igraph::set_graph_attr(
                    ig, 
                    paste0("subnetwork...",subNet$id,"...nodes"), 
                    subNet$nodes)
            }
            if(!is.null(subNet$edges)){
                ig = igraph::set_graph_attr(
                    ig, 
                    paste0("subnetwork...",subNet$id,"...edges"), 
                    subNet$edges)
            }
        }
    }
    
    if("cartesianLayout" %in% aspects){
        attributes = rcx$cartesianLayout
        
        if(! "view" %in% colnames(attributes)){
            attributes$view = NA    
        }
        
        attNames = unique(attributes$view)
        
        if(length(attNames)!=0){
            for(v in attNames){
                attrX = "cartesianLayout...x"
                attrY = "cartesianLayout...y"
                attrZ = "cartesianLayout...z"
                
                selData = attributes
                if(is.na(v)){
                    selData = selData[is.na(selData$view),]
                }else{
                    selData = selData[!is.na(selData$view),]
                    selData = selData[selData$view == v,]
                    
                    attrX = paste0(attrX,"...", v)
                    attrY = paste0(attrY,"...", v)
                    attrZ = paste0(attrZ,"...", v)
                }
                
                ids = selData$node
                igIds = match(ids, igraph::V(ig)$id)
                
                ig = igraph::set_vertex_attr(ig, attrX, igIds, selData$x)
                ig = igraph::set_vertex_attr(ig, attrY, igIds, selData$y)
                if(("z" %in% colnames(selData)) && (any(!is.na(selData$z)))){
                    ig = igraph::set_vertex_attr(ig, attrZ, igIds, selData$z)
                }
            }
        }
    }
    
    return(ig)
}


#' @rdname Igraph
#' @export
#'
#' @param ig [igraph][igraph::igraph()] object
#' @param nodeId character; igraph attribute name used for [node][Nodes] ids
#' @param nodeName character; igraph attribute name used for [node][Nodes] names
#' @param nodeIgnore character; igraph attribute names that should be ignored
#' @param edgeId character; igraph attribute name used for [edge][Edges] ids
#' @param edgeInteraction character; igraph attribute name used for [edge][Edges] interaction
#' @param edgeIgnore character; igraph attribute names that should be ignored
#' @param suppressWarning logical; whether to suppress a warning message, if the validation of the [RCX][RCX-object] object fails
fromIgraph = function(ig, 
                      nodeId="id", nodeName="nodeName", nodeIgnore=c("name"), 
                      edgeId="id", edgeInteraction="edgeInteraction", edgeIgnore=c(),
                      suppressWarning=FALSE){
    fname = "fromIgraph"
    if(! "igraph" %in% class(ig)) .stop("wrongClass",c(.formatLog("ig", fname), "igraph"))
    
    attrNames = igraph::vertex_attr_names(ig)
    
    ## Nodes:
    ## filter name and represents and treat it differently
    tmpName = igraph::vertex_attr(ig, nodeName)
    tmpRep = igraph::vertex_attr(ig, "represents")
    if(is.null(nodeId)) {
        tmpId = seq_len(length(tmpName))
    }else{
        tmpId = igraph::vertex_attr(ig, nodeId)
    }
    nodes = createNodes(id=tmpId, name = tmpName, represents = tmpRep)
    
    
    ## Node Attributes:
    attrNames = attrNames[! attrNames %in% nodeIgnore]
    ## filter core node attributes
    attrNames = attrNames[! attrNames %in% c(nodeId, nodeName, "represents")]
    
    ## Cartesian Layout:
    ## filter for cartesian layout
    cartAcc = attrNames[startsWith(attrNames,"cartesianLayout")]
    attrNames = attrNames[! attrNames %in% cartAcc]
    
    cartesianLayout = NULL
    if(length(cartAcc)!=0){
        tmpCart = list()
        for(cart in cartAcc){
            tmpSpl = strsplit(cart, split = "\\.\\.\\.")[[1]]
            tmpName = tmpSpl[2]
            tmpSub = "NA"
            if(length(tmpSpl)==3) tmpSub = tmpSpl[3]
            
            tmpList = list()
            if(! is.null(tmpCart[[tmpSub]])) tmpList = tmpCart[[tmpSub]]
            tmpList[[tmpName]] = igraph::vertex_attr(ig, cart)
            tmpCart[[tmpSub]] = tmpList
        }
        
        tmpCartLay = lapply(names(tmpCart), function(tmpN){
            tmpX = tmpCart[[tmpN]][["x"]]
            tmpSel = ! is.na(tmpX)
            tmpY = tmpCart[[tmpN]][["y"]]
            tmpSel = tmpSel | (! is.na(tmpY))
            tmpZ = tmpCart[[tmpN]][["z"]]
            if(! is.null(tmpZ)) tmpSel = tmpSel | (! is.na(tmpZ))
            
            tmpView = tmpN
            if(tmpView=="NA") {
                tmpView = NULL
            }else{
                tmpView = rep(as.numeric(tmpView), length(tmpSel))
            }
            
            tmpCL = createCartesianLayout(node = tmpId[tmpSel], 
                                          x = tmpX[tmpSel], 
                                          y = tmpY[tmpSel],
                                          z = tmpZ[tmpSel], 
                                          view = tmpView[tmpSel])
        })
        
        cartesianLayout = tmpCartLay[[1]]
        
        if(length(tmpCartLay)>1){
            for(i in seq(2,length(tmpCartLay))){
                cartesianLayout = updateCartesianLayout(cartesianLayout, tmpCartLay[[i]])
            }
        }
    }
    
    
    ## the remaining attributes must be nodeAttributes
    ## trim datatypes from attributes
    tmpDataTypes = attrNames[endsWith(attrNames, "...dataType")]
    attrNames = attrNames[! attrNames %in% tmpDataTypes]
    tmpDataTypes = gsub("\\.\\.\\.dataType", "", tmpDataTypes)
    
    ## process all remaining attributes
    nodeAttributes = NULL
    if(length(attrNames)!=0){
        tmpNodeAttr = lapply(attrNames, function(a){
            tmpAcc = a
            ## revert convention for name in nodeAttributes
            a = gsub("attribute\\.\\.\\.", "", a)
            
            ## get all values that are not NA
            tmpVal = igraph::vertex_attr(ig, tmpAcc)
            tmpSel = !is.na(tmpVal)
            
            tmpList = is.list(tmpVal)
            if(tmpList) tmpSel = ! sapply(tmpVal, is.null)
            
            tmpSplit = strsplit(a, split = "\\.\\.\\.")[[1]]
            tmpName = rep(tmpSplit[1], length(tmpSel))
            tmpSub = NULL
            if(length(tmpSplit)==2) tmpSub = rep(as.numeric(tmpSplit[2]), length(tmpSel))[tmpSel]
            
            ## assign the correct data types
            if(tmpAcc %in% tmpDataTypes) {
                tmpDT = igraph::vertex_attr(ig, paste0(tmpAcc,"...dataType"))
            }else{
                tmpValUnlist = ifelse(tmpList, unlist(tmpVal), tmpVal)
                
                if(is.logical(tmpValUnlist)) {
                    tmpDT = rep("boolean", length(tmpSel))
                }else if(is.numeric(tmpValUnlist)) {
                    tmpDT = rep("double", length(tmpSel))
                }else{
                    tmpDT = rep("string", length(tmpSel))
                }
            }
            
            tmpList = rep(tmpList, length(tmpSel))
            
            tmpAttr = createNodeAttributes(propertyOf = tmpId[tmpSel],
                                           name = tmpName[tmpSel],
                                           value = tmpVal[tmpSel],
                                           dataType = tmpDT[tmpSel],
                                           isList = tmpList[tmpSel],
                                           subnetworkId = tmpSub)
            return(tmpAttr)
        })
        
        nodeAttributes = tmpNodeAttr[[1]]
        if(length(tmpNodeAttr)>1){
            for(i in seq(2, length(tmpNodeAttr))){
                nodeAttributes = updateNodeAttributes(nodeAttributes, tmpNodeAttr[[i]])
            }
        }
    }
    
    ## Edges:
    ## necessary for getting the internal ids
    if(is.null(edgeId)){
        tmpCols = c("from", "to")
    }else{
        tmpCols = c("from", "to", edgeId)
    }
    tmpData = unique(igraph::as_long_data_frame(ig)[tmpCols])
    tmpSource = tmpId[tmpData$from]
    tmpTarget = tmpId[tmpData$to]
    
    ## filter interaction and treat it differently
    tmpInter = igraph::edge_attr(ig, "interaction")
    if(is.null(edgeId)){
        tmpEId = seq_len(length(tmpSource))
    }else{
        tmpEId = tmpData[,edgeId]
    }
    
    edges = createEdges(id=tmpEId, source = tmpSource, target = tmpTarget, interaction = tmpInter)
    
    
    ## Edge Attribures:
    attrNames = igraph::edge_attr_names(ig)
    edgeIgnore = c(edgeId, "interaction", edgeIgnore)
    attrNames = attrNames[! attrNames %in% edgeIgnore]
    
    ## trim datatypes from attributes
    tmpDataTypes = attrNames[endsWith(attrNames, "...dataType")]
    attrNames = attrNames[! attrNames %in% tmpDataTypes]
    tmpDataTypes = gsub("\\.\\.\\.dataType", "", tmpDataTypes)
    
    
    ## process all remaining attributes
    edgeAttributes = NULL
    if(length(attrNames)!=0){
        tmpEdgeAttr = lapply(attrNames, function(a){
            tmpAcc = a
            a = gsub("attribute\\.\\.\\.", "", a)
            
            tmpVal = igraph::edge_attr(ig, tmpAcc)
            tmpSel = !is.na(tmpVal)
            tmpList = is.list(tmpVal)
            if(tmpList) tmpSel = ! sapply(tmpVal, is.null)
            
            tmpSplit = strsplit(a, split = "\\.\\.\\.")[[1]]
            tmpName = rep(tmpSplit[1], length(tmpSel))
            tmpSub = NULL
            if(length(tmpSplit)==2) tmpSub = rep(as.numeric(tmpSplit[2]), length(tmpSel))[tmpSel]
            
            if(tmpAcc %in% tmpDataTypes) {
                tmpDT = igraph::vertex_attr(ig, paste0(tmpAcc,"...dataType"))
            }else{
                tmpValUnlist = ifelse(tmpList, unlist(tmpVal), tmpVal)
                
                if(is.logical(tmpValUnlist)) {
                    tmpDT = rep("boolean", length(tmpSel))
                }else if(is.numeric(tmpValUnlist)) {
                    tmpDT = rep("double", length(tmpSel))
                }else{
                    tmpDT = rep("string", length(tmpSel))
                }
            }
            
            tmpList = rep(tmpList, length(tmpSel))
            
            tmpAttr = createEdgeAttributes(propertyOf = tmpEId[tmpSel],
                                           name = tmpName[tmpSel],
                                           value = tmpVal[tmpSel],
                                           dataType = tmpDT[tmpSel],
                                           isList = tmpList[tmpSel],
                                           subnetworkId = tmpSub)
            return(tmpAttr)
        })
        
        edgeAttributes = tmpEdgeAttr[[1]]
        if(length(tmpEdgeAttr)>1){
            for(i in seq(2,length(tmpEdgeAttr))) {
                edgeAttributes = updateEdgeAttributes(edgeAttributes, tmpEdgeAttr[[i]])
            }
        }
    }
    
    ## Network Attribures:
    networkAttributes = NULL
    attrNames = igraph::graph_attr_names(ig)
    
    ## trim datatypes from attributes
    tmpDataTypes = attrNames[endsWith(attrNames, "...dataType")]
    attrNames = attrNames[! attrNames %in% tmpDataTypes]
    tmpDataTypes = gsub("\\.\\.\\.dataType", "", tmpDataTypes)
    
    ## special handling of the subnetworks
    tmpSubNetworks = attrNames[startsWith(attrNames, "subnetwork...")]
    attrNames = attrNames[! attrNames %in% tmpSubNetworks]
    cySubNetworks = NULL
    if(length(tmpSubNetworks)!=0){
        snids = as.numeric(unique(sapply(strsplit(tmpSubNetworks,"\\.\\.\\."), function(x){x[2]})))
        for (snid in snids) {
            nodesData = NULL
            tmpNodesName = paste0("subnetwork...",snid,"...nodes")
            if(tmpNodesName %in% tmpSubNetworks){
                nodesData = igraph::graph_attr(ig, tmpNodesName)
            }
            edgesData = NULL
            tmpEdgesName = paste0("subnetwork...",snid,"...edges")
            if(tmpNodesName %in% tmpSubNetworks){
                edgesData = igraph::graph_attr(ig, tmpEdgesName)
            }
            
            ## create subnetworks aspect
            tmpSubnetwork = createCySubNetworks(snid, nodes = nodesData, edges = edgesData)
            
            ## add it to the list
            if(is.null(cySubNetworks)){
                cySubNetworks = tmpSubnetwork
            }else{
                cySubNetworks = updateCySubNetworks(cySubNetworks, tmpSubnetwork, keepOldIds = FALSE)
            }
        }
    }
    
    ## process all attributes
    tmpNetAttr = lapply(attrNames, function(a){
        tmpAcc = a
        
        tmpVal = igraph::graph_attr(ig, tmpAcc)
        tmpSel = !is.na(tmpVal)
        tmpList = is.list(tmpVal)
        if(tmpList) tmpSel = ! sapply(tmpVal, is.null)
        
        tmpSplit = strsplit(tmpAcc, split = "\\$")[[1]]
        tmpName = rep(tmpSplit[1], length(tmpSel))
        tmpSub = NULL
        if(length(tmpSplit)==2) tmpSub = rep(as.numeric(tmpSplit[2]), length(tmpSel))[tmpSel]
        
        if(tmpAcc %in% tmpDataTypes) tmpDT = igraph::graph_attr(ig, paste0(tmpAcc,"...dataType"))
        tmpValUnlist = ifelse(tmpList, unlist(tmpVal), tmpVal)
        if(is.logical(tmpValUnlist)) {
            tmpDT = rep("boolean", length(tmpSel))
        }else if(is.numeric(tmpValUnlist)) {
            tmpDT = rep("double", length(tmpSel))
        }else{
            tmpDT = rep("string", length(tmpSel))
        }
        
        tmpList = rep(tmpList, length(tmpSel))
        
        tmpAttr = createNetworkAttributes(name = tmpName[tmpSel],
                                          value = tmpVal[tmpSel],
                                          dataType = tmpDT[tmpSel],
                                          isList = tmpList[tmpSel],
                                          subnetworkId = tmpSub)
        return(tmpAttr)
    })
    
    if(length(tmpNetAttr)>0){
        networkAttributes = tmpNetAttr[[1]]
        if(length(tmpNetAttr)>1){
            for(i in seq(2, length(tmpNetAttr))) {
                networkAttributes = updateNetworkAttributes(networkAttributes, tmpNetAttr[[i]])
            }
        }
    }
    
    ## Create RCX
    rcx = createRCX(nodes = nodes, edges = edges,
                    nodeAttributes = nodeAttributes,
                    edgeAttributes = edgeAttributes,
                    networkAttributes = networkAttributes,
                    cartesianLayout = cartesianLayout,
                    cySubNetworks = cySubNetworks,
                    checkReferences = FALSE)
    
    if((! validate(rcx, FALSE)) && (! suppressWarning)) warning("RCX object didn't validate!")
    return(rcx)
}

