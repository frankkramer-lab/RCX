##########################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##   cyVisualProperties
##########################################################################################
# property          |options          |values          |description
# ------------------|-----------------|----------------|------------------------------------------------
# "properties_of"   |Required         |string, dictionary|to indicate the element type these properties belong to, allowed values are: "network", "nodes:default", "edges:default", "nodes", "edges"
# "applies_to"      |Required         |integer         |the identifier of the element these properties apply to, reference to CX internal [node id "id"](#nodeid) or [edge id "id"](#edgeid)
# "view"            |Optional         |integer         |[Cytoscape view id](#cytoscape)
# "properties"      |Optional         |named list of strings |pairs of the actual properties, e.g. "NODE_BORDER_STROKE" : "SOLID", "NODE_BORDER_WIDTH" : "1.5", "NODE_FILL_COLOR" : "#FF3399"
# "dependencies"    |Optional         |named list of strings |pairs of the dependencies, e.g. "nodeCustomGraphicsSizeSync" : "true", "nodeSizeLocked" : "false"
# "mappings"        |Optional         |named list of named list of strings, dictionary | e.g. "NODE_FILL_COLOR" : {"type" : "CONTINUOUS", "definition" : "COL=gal1RGexp,T=double,L=0=#0066CC,E=0=#0066CC,G=0=#0066CC,OV=0=-2.426,L=1=#FFFFFF,E=1=#FFFFFF,G=1=#FFFFFF,OV=1=1.225471493171426E-7,L=2=#FFFF00,E=2=#FFFF00,G=2=#FFFF00,OV=2=2.058"}
##########################################################################################
# CyVisualPropertiesAspect
#     network=CyVisualProperty
#         appliesTo
#         view
#         properties=CyVisualPropertyProperties
#         dependencies=CyVisualPropertyDependencies
#         mappings=CyVisualPropertyMappings
#     nodes=CyVisualProperty
#         appliesTo
#         view
#         properties=CyVisualPropertyProperties
#         dependencies=CyVisualPropertyDependencies
#         mappings=CyVisualPropertyMappings
#     edges=CyVisualProperty
#         appliesTo
#         view
#         properties=CyVisualPropertyProperties
#         dependencies=CyVisualPropertyDependencies
#         mappings=CyVisualPropertyMappings
#     defaultNodes=CyVisualProperty
#         appliesTo
#         view
#         properties=CyVisualPropertyProperties
#         dependencies=CyVisualPropertyDependencies
#         mappings=CyVisualPropertyMappings
#     defaultEdges=CyVisualProperty
#         appliesTo
#         view
#         properties=CyVisualPropertyProperties
#         dependencies=CyVisualPropertyDependencies
#         mappings=CyVisualPropertyMappings
##########################################################################################    


#' Create a object for properties of Cytoscape Visual Properties (object used in CyVisualProperty)
#' 
#' This function is used to create aspects for mappings in [Cytoscape visual properties][CyVisualProperties]. 
#' Networks, nodes, edges, and default nodes and edges mappings are realized as \code{\link{`r .CLSvp$property`}} objects, that each consist of 
#' properties (**this here**), dependencies (\code{\link{`r .CLSvp$dependencies`}} objects) and mappings (\code{\link{`r .CLSvp$mappings`}} objects).
#' 
#' @note If *name* is not provided, the *names(value)* is used instead to infer the names.
#' 
#' @template cy-visual-properties-description
#' @template cy-visual-properties-structure
#'
#' @param value character or named character; value of the property
#' @param name character (optional); name of the property
#'
#' @return `r .CLSvp$properties` object
#' 
#' @name CyVisualPropertyProperties
#' @seealso \code{\link{updateCyVisualProperty}}, \code{\link{updateCyVisualProperties}}
#'
#' @export
#' @example man-roxygen-examples/cy-visual-property-properties-create.R
createCyVisualPropertyProperties = function(value, name=NULL){
    fname="createCyVisualPropertyProperties"
    if(missing(value))  .stop("paramMissing", "value is required!")
    
    vpp = .createCyVpPorD(name, value, fname)
    
    .addClass(vpp) = .CLSvp$properties
    return(vpp)
}


#' Create a object for dependency of Cytoscape Visual Properties (object used in CyVisualProperty)
#' 
#' This function is used to create aspects for mappings in [Cytoscape visual properties][CyVisualProperties]. 
#' Networks, nodes, edges, and default nodes and edges mappings are realized as \code{\link{`r .CLSvp$property`}} objects, that each consist of 
#' properties (\code{\link{`r .CLSvp$properties`}} objects), dependencies (**this here**) and mappings (\code{\link{`r .CLSvp$mappings`}} objects).
#' 
#' @note If *name* is not provided, the *names(value)* is used instead to infer the names.
#'
#' @template cy-visual-properties-description
#'
#' @param value character or named character; value of the dependencies
#' @param name character (optional); name of the dependencies
#'
#' @return `r .CLSvp$dependencies` object
#' 
#' @name CyVisualPropertyDependencies
#' @seealso \code{\link{updateCyVisualProperty}}, \code{\link{updateCyVisualProperties}}
#' 
#' @export
#' @example man-roxygen-examples/cy-visual-property-dependencies-create.R
createCyVisualPropertyDependencies = function(value, name=NULL){
    fname="createCyVisualPropertyDependencies"
    if(missing(value))  .stop("paramMissing", "value is required!")
    
    vpd  = .createCyVpPorD(name, value, fname)
    
    .addClass(vpd) = .CLSvp$dependencies
    return(vpd)
}


#' Create an object for mappings of Cytoscape Visual Properties (object used in CyVisualProperty)
#' 
#' This function is used to create objects for mappings in [Cytoscape visual properties][CyVisualProperties]. 
#' Networks, nodes, edges, and default nodes and edges mappings are realized as \code{\link{`r .CLSvp$property`}} objects, that each consist of 
#' properties (\code{\link{`r .CLSvp$properties`}} objects), dependencies (\code{\link{`r .CLSvp$dependencies`}} objects) and mappings (**this here**).
#' 
#' @note If *name* is not provided, the *names(type)* is used instead to infer the names.
#'
#' @template cy-visual-properties-description
#' @template cy-visual-properties-structure
#' 
#' @param type character or named character; value of the mappings
#' @param definition character; definitions of the mappings
#' @param name character (optional); names of the mappings
#'
#' @return `r .CLSvp$mappings` object
#' 
#' @name CyVisualPropertyMappings
#' @seealso \code{\link{updateCyVisualProperty}}, \code{\link{updateCyVisualProperties}}
#'
#' @export
#' @example man-roxygen-examples/cy-visual-property-mappings-create.R
createCyVisualPropertyMappings = function(type, definition, name=NULL){
    fname="createCyVisualPropertyMappings"
    if(missing(type) || missing(definition))  .stop("paramMissing", "type and definition are required!")
    if(is.null(name)) {
        if(is.null(names(type))) .stop("paramMissing", "type with names is required!")
        name = names(type)
        type = unname(type)
    }
    
    .checkCharacter(name, "name", fname)
    .checkCharacter(type, "type", fname)
    .checkCharacter(definition, "definition", fname)
    
    .checkSameLength(fname,
                     type, definition, name)
    
    vpm = data.frame(name=name,
                     type=type,
                     definition=definition,
                     stringsAsFactors = F,
                     check.names = F)
    
    .checkUniqueDF(vpm, "name", fname)
    
    .addClass(vpm) = .CLSvp$mappings
    return(vpm)
}


#TODO: Find some examples where appliesTo and view are actually used
#' Cytoscape visual property (object used in CyVisualProperties aspect)
#' 
#' This function is used to create Cytoscape visual property objects, that define networks, nodes, edges, and default nodes and edges in a
#' \code{\link{CyVisualProperties}} aspect.
#' 
#' @template cy-visual-properties-description
#' @template cy-visual-properties-structure
#'
#' @param properties a single or a list of \code{\link{CyVisualPropertyProperties}} object (optional); 
#' @param dependencies a single or a list of\code{\link{CyVisualPropertyDependencies}} object (optional); 
#' @param mappings a single or a list of\code{\link{CyVisualPropertyMappings}} object (optional); 
#' @param appliesTo integer (optional); might refer to the IDs of a [subnetwork][CySubNetworks] aspect, but CX documantation is unclear
#' @param view integer (optional); might refer to the IDs of a [subnetwork][CySubNetworks] aspect that is a view, but CX documantation is unclear
#'
#' @return \code{\link{`r .CLSvp$property`}} object
#' @export
#' @name CyVisualProperty
#' @seealso \code{\link{updateCyVisualProperty}}, \code{\link{updateCyVisualProperties}}
#'
#' @example man-roxygen-examples/cy-visual-property-create.R
createCyVisualProperty = function(properties=NULL, dependencies=NULL, mappings=NULL, appliesTo=NULL, view=NULL){
    fname="createCyVisualProperty"
    if(all(is.null(properties), is.null(dependencies), is.null(mappings)))  .stop("paramAllNull", "properties, dependencies or mappings")

    if(!is.null(appliesTo)){
        if(is.logical(appliesTo) && all(is.na(appliesTo))) appliesTo = as.numeric(appliesTo)
        .checkNumeric(appliesTo, "appliesTo", fname)
    } 
    
    if(!is.null(view)){
        if(is.logical(view) && all(is.na(view))) view = as.numeric(view)
        .checkNumeric(view, "view", fname)
    } 
    
    cvp = list()

    ## check for uniquenes of appliesTo and view
    if((!is.null(appliesTo)) && (!is.null(view))){
        key = data.frame(appliesTo=appliesTo,
                         view=view,
                         stringsAsFactors = F,
                         check.names = F)
        .checkUniqueDF(key,c("appliesTo","view"), fname)
        
        cvp$appliesTo=appliesTo
        cvp$view=view
    }else if(!is.null(appliesTo)){
        .checkUnique(appliesTo, "appliesTo", fname)
        cvp$appliesTo=appliesTo
        cvp$view=rep(NA, length(appliesTo))
    }else if(!is.null(view)){
        .checkUnique(view, "view", fname)
        cvp$view=view
        cvp$appliesTo=rep(NA, length(view))
    }


    if(!is.null(properties)) {
        if(.paramClass(properties, .CLSvp$properties)) {
            properties=list(properties)
        }
        .checkAllClass(properties, .CLSvp$properties, "properties", fname)
        cvp$properties=properties
    }
    if(!is.null(dependencies)) {
        if(.paramClass(dependencies, .CLSvp$dependencies)) {
            dependencies=list(dependencies)
        }
        .checkAllClass(dependencies, .CLSvp$dependencies, "dependencies", fname)
        cvp$dependencies=dependencies
    }
    if(!is.null(mappings)) {
        if(.paramClass(mappings, .CLSvp$mappings)) {
            mappings=list(mappings)
        }
        .checkAllClass(mappings, .CLSvp$mappings, "mappings", fname)
        cvp$mappings=mappings
    }


    ## Fill missing attributes with NA
    no = max(sapply(cvp, length))

    if(is.null(cvp$properties)) cvp$properties = as.list(rep(NA, no))
    if(is.null(cvp$dependencies)) cvp$dependencies = as.list(rep(NA, no))
    if(is.null(cvp$mappings)) cvp$mappings = as.list(rep(NA, no))
    if(is.null(cvp$view)) cvp$view = as.numeric(rep(NA, no))
    if(is.null(cvp$appliesTo)) cvp$appliesTo = as.numeric(rep(NA, no))

    .checkSameLength(fname,
                     cvp$properties, cvp$dependencies, cvp$mappings, cvp$appliesTo, cvp$view)
    
    orderAV = order(cvp$appliesTo,cvp$view, na.last = F)
    cvp$properties = cvp$properties[orderAV]
    cvp$dependencies = cvp$dependencies[orderAV]
    cvp$mappings = cvp$mappings[orderAV]
    cvp$view = cvp$view[orderAV]
    cvp$appliesTo = cvp$appliesTo[orderAV]

    .addClass(cvp) = .CLSvp$property
    return(cvp)
}


#' Cytoscape visual properties (aspect)
#' 
#' This function is used to create Cytoscape visual properties aspects, that consists of \code{\link{`r .CLSvp$property`}} objects for 
#' networks, nodes, edges, and default nodes and edges.
#' 
#' @template cy-visual-properties-description
#' @template cy-visual-properties-structure
#'
#' @param network \code{\link{`r .CLSvp$property`}} object (optional); the visual properties of networks
#' @param nodes \code{\link{`r .CLSvp$property`}} object (optional); the visual properties of nodes
#' @param edges \code{\link{`r .CLSvp$property`}} object (optional); the visual properties of edges
#' @param defaultNodes \code{\link{`r .CLSvp$property`}} object (optional); the default visual properties of nodes
#' @param defaultEdges \code{\link{`r .CLSvp$property`}} object (optional); the default visual properties of edges
#'
#' @return *`r .CLS$cyVisualProperties`* object
#' @seealso \code{\link{updateCyVisualProperties}}, \code{\link{updateCyVisualProperty}}, \code{\link{getCyVisualProperty}}
#' @export
#' @name CyVisualProperties
#'
#' @example man-roxygen-examples/cy-visual-properties-create.R
createCyVisualProperties = function(network=NULL, nodes=NULL, edges=NULL, defaultNodes=NULL, defaultEdges=NULL){
    fname="createCyVisualProperties"
    if(all(is.null(network), 
           is.null(nodes), 
           is.null(edges), 
           is.null(defaultNodes), 
           is.null(defaultEdges)))  
        .stop("paramAllNull", "network, nodes, edges, defaultNodes or defaultEdges")
    
    
    vpp  = list()
    if(!is.null(network)) {
        .checkClass(network, .CLSvp$property, "network", fname)
        vpp$network=network
    }
    if(!is.null(nodes)) {
        .checkClass(nodes, .CLSvp$property, "nodes", fname)
        vpp$nodes=nodes
    }
    if(!is.null(edges)) {
        .checkClass(edges, .CLSvp$property, "edges", fname)
        vpp$edges=edges
    }
    if(!is.null(defaultNodes)) {
        .checkClass(defaultNodes, .CLSvp$property, "defaultNodes", fname)
        vpp$defaultNodes=defaultNodes
    }
    if(!is.null(defaultEdges)) {
        .checkClass(defaultEdges, .CLSvp$property, "defaultEdges", fname)
        vpp$defaultEdges=defaultEdges
    }
    
    .addClass(vpp) = .CLS$cyVisualProperties
    return(vpp)
}


##########################################################################################
## Functions for adding Cytoscape Visual Properties aspects and its objects
##########################################################################################


#' Update Cytoscape Visual Property objects and sub-objects (used in CyVisualProperties aspect)
#' 
#' This function is used to add Cytoscape visual property objects (\code{\link{`r .CLSvp$property`}}) and its sub-objects 
#' (\code{\link{`r .CLSvp$properties`}}, \code{\link{`r .CLSvp$dependencies`}} and \code{\link{`r .CLSvp$mappings`}}) to each other. 
#' Cytoscape visual property objects define networks, nodes, edges, and default nodes and edges in a \code{\link{CyVisualProperties}} aspect.
#' 
#' @template cy-visual-property-structure
#' 
#' @template cy-visual-properties-property-adding 
#'
#' @param cyVisualProperty object; (to which it will be added)
#' @param additionalProperty object; (that will be added)
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in `name` (and `subnetworkId` if present) column are found
#' @param .log character (optional); name of the calling function used in error logging
#' 
#' @return \code{\link{`r .CLSvp$property`}}, \code{\link{`r .CLSvp$properties`}}, \code{\link{`r .CLSvp$dependencies`}} or
#' \code{\link{`r .CLSvp$mappings`}} objects
#' @seealso \code{\link{getCyVisualProperty}}, \code{\link{updateCyVisualProperties}}
#' @export
#' @example man-roxygen-examples/cy-visual-property-update.R
updateCyVisualProperty = function(cyVisualProperty, additionalProperty, replace=T, stopOnDuplicates=F, .log=c()){
    UseMethod("updateCyVisualProperty", cyVisualProperty)
}


#' @rdname updateCyVisualProperty
#' @export
updateCyVisualProperty.CyVisualPropertyProperties = function(cyVisualProperty, additionalProperty, replace=T, stopOnDuplicates=F, .log=c()){
    fname="updateCyVisualProperty"
    if(missing(cyVisualProperty)) .stop("paramMissing", "cyVisualProperty")
    if(missing(additionalProperty)) .stop("paramMissing", "additionalProperty")
    .checkClass(cyVisualProperty, .CLSvp$properties, "cyVisualProperty", fname)
    .checkClass(additionalProperty, .CLSvp$properties, "additionalProperty", fname)
    
    result = .mergeAttributesAspect(cyVisualProperty,
                                    additionalProperty,
                                    replace, stopOnDuplicates,
                                    required="name",
                                    optional=c(),
                                    .log=.formatData(.log))
    
    .addClass(result) = .CLSvp$properties
    
    return(result)
}


#' @rdname updateCyVisualProperty
#' @export
updateCyVisualProperty.CyVisualPropertyDependencies = function(cyVisualProperty, additionalProperty, replace=T, stopOnDuplicates=F, .log=c()){
    fname="updateCyVisualProperty"
    if(missing(cyVisualProperty)) .stop("paramMissing", "cyVisualProperty")
    if(missing(additionalProperty)) .stop("paramMissing", "additionalProperty")
    .checkClass(cyVisualProperty, .CLSvp$dependencies, "cyVisualProperty", fname)
    .checkClass(additionalProperty, .CLSvp$dependencies, "additionalProperty", fname)
    
    result = .mergeAttributesAspect(cyVisualProperty,
                                    additionalProperty,
                                    replace, stopOnDuplicates,
                                    required="name",
                                    optional=c(),
                                    .log=.formatData(.log))
    
    .addClass(result) = .CLSvp$dependencies
    
    return(result)
}


#' @rdname updateCyVisualProperty
#' @export
updateCyVisualProperty.CyVisualPropertyMappings = function(cyVisualProperty, additionalProperty, replace=T, stopOnDuplicates=F, .log=c()){
    fname="updateCyVisualProperty"
    if(missing(cyVisualProperty)) .stop("paramMissing", "cyVisualProperty")
    if(missing(additionalProperty)) .stop("paramMissing", "additionalProperty")
    .checkClass(cyVisualProperty, .CLSvp$mappings, "cyVisualProperty", fname)
    .checkClass(additionalProperty, .CLSvp$mappings, "additionalProperty", fname)
    
    result = .mergeAttributesAspect(cyVisualProperty,
                                    additionalProperty,
                                    replace, stopOnDuplicates,
                                    required="name",
                                    optional=c(),
                                    .log=.formatData(.log))
    
    .addClass(result) = .CLSvp$mappings
    
    return(result)
}



#' Get a Cytoscape visual property (object used in CyVisualProperties aspect) by appliesTo and view
#'
#' This function helps filtering \code{\link{CyVisualProperty}} objects by appliesTo and view attributes (i.e. a unique combination of both).
#' If nothing matches the searched pattern `NULL` is returned.
#'
#' @template cy-visual-properties-description
#' @template cy-visual-properties-structure
#'
#' @param cyVisualProperty \code{\link{CyVisualProperty}} object
#' @param appliesTo integer (optional); value of appliesTo to filter for
#' @param view integer (optional); value of view to filter for
#' 
#' @return \code{\link{`r .CLSvp$property`}} object containing only one element, or `NULL`
#' @export
#' @seealso \code{\link{updateCyVisualProperty}}, \code{\link{updateCyVisualProperties}}
#'
#' @example man-roxygen-examples/cy-visual-property-get.R
getCyVisualProperty = function(cyVisualProperty, appliesTo=NA, view=NA){
    fname="getCyVisualProperty"
    if(missing(cyVisualProperty)) .stop("paramMissing", "cyVisualProperty")
    .checkClass(cyVisualProperty, .CLSvp$property, "cyVisualProperty", fname)
    
    filterKey = paste(cyVisualProperty$appliesTo,
                      cyVisualProperty$view,
                      sep = ":")
    key = paste(appliesTo, view, sep = ":")
    i = which(filterKey==key)
    
    if(length(i)==0) return(NULL)

    result = list(
        appliesTo=cyVisualProperty$appliesTo[i],
        view=cyVisualProperty$view[i],
        properties=cyVisualProperty$properties[i],
        dependencies=cyVisualProperty$dependencies[i],
        mappings=cyVisualProperty$mappings[i]
    )
    
    .addClass(result) = .CLSvp$property
    
    return(result)
}


#' @rdname updateCyVisualProperty
#' @export
updateCyVisualProperty.CyVisualProperty = function(cyVisualProperty, additionalProperty, replace=T, stopOnDuplicates=F, .log=c()){
    fname="updateCyVisualProperty"
    if(missing(cyVisualProperty)) .stop("paramMissing", "cyVisualProperty")
    if(missing(additionalProperty)) .stop("paramMissing", "additionalProperty")
    .checkClass(cyVisualProperty, .CLSvp$property, "cyVisualProperty", fname)
    .checkClass(additionalProperty, .CLSvp$property, "additionalProperty", fname)
    
    cyVPfilterKey = paste(cyVisualProperty$appliesTo,
                          cyVisualProperty$view,
                          sep = ":")
    addPfilterKey = paste(additionalProperty$appliesTo,
                          additionalProperty$view,
                          sep = ":")
    
    uniqKeys = unique(cyVPfilterKey, addPfilterKey)
    
    tmpAppliesTo = c()
    tmpView = c()
    tmpProperties = list()
    tmpDependencies = list()
    tmpMappings = list()
    
    results = NULL
    
    for (i in 1:length(uniqKeys)){
        key = uniqKeys[i]
        indexInCyVP = which(cyVPfilterKey==key)
        indexInAddP = which(addPfilterKey==key)
        
        if(length(indexInCyVP)!=0 && length(indexInAddP)!=0){
            tmpAppliesTo[i] = cyVisualProperty$appliesTo[indexInCyVP]
            tmpView[i] = cyVisualProperty$view[indexInCyVP]
            .logAV = paste0("<appliesTo=",tmpAppliesTo[i],",view=",tmpView[i],">")
            
            prop = NULL
            if(all(is.na(cyVisualProperty$properties[[indexInCyVP]]))){
                prop = additionalProperty$properties[[indexInAddP]]
            }else if(all(is.na(additionalProperty$properties[[indexInAddP]]))){
                prop = cyVisualProperty$properties[[indexInCyVP]]
            }else{
                prop = updateCyVisualProperty(
                    cyVisualProperty$properties[[indexInCyVP]],
                    additionalProperty$properties[[indexInAddP]],
                    replace,
                    stopOnDuplicates,
                    c(.log,paste0("properties",.logAV)))
            }
            
            deps = NULL
            if(all(is.na(cyVisualProperty$dependencies[[indexInCyVP]]))){
                deps = additionalProperty$dependencies[[indexInAddP]]
            }else if(all(is.na(additionalProperty$dependencies[[indexInAddP]]))){
                deps = cyVisualProperty$dependencies[[indexInCyVP]]
            }else{
                deps = updateCyVisualProperty(
                    cyVisualProperty$dependencies[[indexInCyVP]],
                    additionalProperty$dependencies[[indexInAddP]],
                    replace,
                    stopOnDuplicates,
                    c(.log,paste0("dependencies",.logAV)))
            }
            
            mapp = NULL
            if(all(is.na(cyVisualProperty$mappings[[indexInCyVP]]))){
                mapp = additionalProperty$mappings[[indexInAddP]]
            }else if(all(is.na(additionalProperty$mappings[[indexInAddP]]))){
                mapp = cyVisualProperty$mappings[[indexInCyVP]]
            }else{
                mapp = updateCyVisualProperty(
                    cyVisualProperty$mappings[[indexInCyVP]],
                    additionalProperty$mappings[[indexInAddP]],
                    replace,
                    stopOnDuplicates,
                    c(.log,paste0("mappings",.logAV)))
            }

            tmpProperties[[i]] = prop
            tmpDependencies[[i]] = deps
            tmpMappings[[i]] = mapp
        }else if(length(indexInCyVP)!=0){
            tmpAppliesTo[i] = cyVisualProperty$appliesTo[indexInCyVP]
            tmpView[i] = cyVisualProperty$view[indexInCyVP]
            tmpProperties[[i]] = cyVisualProperty$properties[[indexInCyVP]]
            tmpDependencies[[i]] = cyVisualProperty$dependencies[[indexInCyVP]]
            tmpMappings[[i]] = cyVisualProperty$mappings[[indexInCyVP]]
        }else{
            tmpAppliesTo[i] = additionalProperty$appliesTo[indexInAddP]
            tmpView[i] = additionalProperty$view[indexInAddP]
            tmpProperties[[i]] = additionalProperty$properties[[indexInAddP]]
            tmpDependencies[[i]] = additionalProperty$dependencies[[indexInAddP]]
            tmpMappings[[i]] = additionalProperty$mappings[[indexInAddP]]
        }
    }
    
    result = createCyVisualProperty(properties = tmpProperties,
                                    dependencies = tmpDependencies,
                                    mappings = tmpMappings,
                                    appliesTo = tmpAppliesTo,
                                    view = tmpView)
    
    
    # for (i in 1:length(uniqKeys)){
    #     key = uniqKeys[i]
    #     indexInCyVP = which(cyVPfilterKey==key)
    #     indexInAddP = which(addPfilterKey==key)
    #     
    #     if(length(indexInCyVP)!=0 && length(indexInAddP)!=0){
    #         tmpAppliesTo[i] = cyVisualProperty$appliesTo[indexInCyVP]
    #         tmpView[i] = cyVisualProperty$view[indexInCyVP]
    #         .logAV = paste0("<appliesTo=",tmpAppliesTo[i],",view=",tmpView[i],">")
    #         
    #         
    #         prop = updateCyVisualProperty(
    #             cyVisualProperty$properties[[indexInCyVP]],
    #             additionalProperty$properties[[indexInAddP]],
    #             replace,
    #             stopOnDuplicates,
    #             c(.log,paste0("properties",.logAV)))
    #         
    #         deps = updateCyVisualProperty(
    #             cyVisualProperty$dependencies[[indexInCyVP]],
    #             additionalProperty$dependencies[[indexInAddP]],
    #             replace,
    #             stopOnDuplicates,
    #             c(.log,paste0("dependencies",.logAV)))
    #         
    #         mapp = updateCyVisualProperty(
    #             cyVisualProperty$mappings[[indexInCyVP]],
    #             additionalProperty$mappings[[indexInAddP]],
    #             replace,
    #             stopOnDuplicates,
    #             c(.log,paste0("mappings",.logAV)))
    #         
    #         tmpProperties[[i]] = prop
    #         tmpDependencies[[i]] = deps
    #         tmpMappings[[i]] = mapp
    #     }else if(length(indexInCyVP)!=0){
    #         tmpAppliesTo[i] = cyVisualProperty$appliesTo[indexInCyVP]
    #         tmpView[i] = cyVisualProperty$view[indexInCyVP]
    #         tmpProperties[[i]] = cyVisualProperty$properties[[indexInCyVP]]
    #         tmpDependencies[[i]] = cyVisualProperty$dependencies[[indexInCyVP]]
    #         tmpMappings[[i]] = cyVisualProperty$mappings[[indexInCyVP]]
    #     }else{
    #         tmpAppliesTo[i] = additionalProperty$appliesTo[indexInAddP]
    #         tmpView[i] = additionalProperty$view[indexInAddP]
    #         tmpProperties[[i]] = additionalProperty$properties[[indexInAddP]]
    #         tmpDependencies[[i]] = additionalProperty$dependencies[[indexInAddP]]
    #         tmpMappings[[i]] = additionalProperty$mappings[[indexInAddP]]
    #     }
    # }
    # 
    # result = createCyVisualProperty(properties = tmpProperties,
    #                                 dependencies = tmpDependencies,
    #                                 mappings = tmpMappings,
    #                                 appliesTo = tmpAppliesTo,
    #                                 view = tmpView)
    
    .addClass(result) = .CLSvp$property
    
    return(result)
}


#' Update Cytoscape Visual Properties (aspect)
#' 
#' This function is used to add [Cytoscape visual properties][CyVisualProperties] aspects to each other or to an 
#' [RCX][RCX-object] object. 
#' In a \code{\link{CyVisualProperties}} aspect, \code{\link{`r .CLSvp$property`}} objects define networks, nodes, edges, and 
#' default nodes and edges.
#' 
#' @template cy-visual-property-structure
#' 
#' @details 
#' \code{\link{CyVisualProperties}} aspects consist of \code{\link{`r .CLSvp$property`}} objects for each entry: networks, nodes, edges, and 
#' default nodes and edges. Two \code{\link{CyVisualProperties}} aspects are merged by adding its entries individually.
#' 
#' @template cy-visual-properties-property-adding 
#'
#' @param x [RCX][RCX-object] or \code{\link{CyVisualProperties}} object; (to which it will be added)
#' @param cyVisualProperties \code{\link{CyVisualProperties}} object; (that will be added)
#' @param ... additional parameters
#' @param replace logical; if existing values are updated (or ignored)
#' @param stopOnDuplicates logical; whether to stop, if duplicates in `name` (and `subnetworkId` if present) column are found
#' @param checkReferences logical; whether to check if references to other aspects are present in the [RCX][RCX-object] object
#' 
#' @return \code{\link{CyVisualProperties}} or [RCX][RCX-object] object with added Cytoscape visual properties
#' @seealso \code{\link{updateCyVisualProperty}}, \code{\link{getCyVisualProperty}}
#' @export
#' @example man-roxygen-examples/cy-visual-properties-update.R
updateCyVisualProperties = function(x, cyVisualProperties, replace=T, stopOnDuplicates=F, ...){
    UseMethod("updateCyVisualProperties", x)
}


#' @rdname updateCyVisualProperties
#' @export
updateCyVisualProperties.CyVisualPropertiesAspect = function(x, cyVisualProperties, replace=T, stopOnDuplicates=F, ...){
    cyVisualPropertiesOld = x
    fname="updateCyVisualProperties"
    if(missing(cyVisualPropertiesOld)) .stop("paramMissing", "x")
    if(missing(cyVisualProperties)) .stop("paramMissing", "cyVisualProperties")
    .checkClass(cyVisualPropertiesOld, .CLS$cyVisualProperties, "x", fname)
    .checkClass(cyVisualProperties, .CLS$cyVisualProperties, "cyVisualProperties", fname)
    
    for(p in names(.DICT$VPpropertiesOf)) {
        if(is.null(cyVisualPropertiesOld[[p]])){
            cyVisualPropertiesOld[[p]] = cyVisualProperties[[p]]
        }else{
            if(! is.null(cyVisualProperties[[p]])){
                cat("\n")
                cat(p)
                cat("\n")
                print(cyVisualProperties[[p]])
                cyVisualPropertiesOld[[p]] = updateCyVisualProperty(cyVisualPropertiesOld[[p]], cyVisualProperties[[p]], 
                                                                    replace, stopOnDuplicates, c("VisualProperties",p))
            }
        }
    }
    
    .addClass(cyVisualPropertiesOld) = .CLS$cyVisualProperties
    
    return(cyVisualPropertiesOld)
}


#' @rdname updateCyVisualProperties
#' @export
updateCyVisualProperties.RCX = function(x, cyVisualProperties, replace=T, stopOnDuplicates=F, checkReferences=T, ...){
    rcx = x
    fname="updateCyVisualProperties"
    if(missing(rcx)) .stop("paramMissingRCX")
    if(missing(cyVisualProperties)) .stop("paramMissing", "cyVisualProperties")
    .checkClass(rcx, .CLS$rcx, "rcx", fname)
    .checkClass(cyVisualProperties, .CLS$cyVisualProperties, "cyVisualProperties", fname)
    
    if((! is.null(cyVisualProperties$appliesTo)) && (checkReferences)){
        .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
        ids = unique(unlist(cyVisualProperties$appliesTo))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$cySubNetworks$id, c("cyVisualProperties$appliesTo", "rcx$cySubNetworks$id"), fname)
    }
    
    if((! is.null(cyVisualProperties$view)) && (checkReferences)){
        .checkRefPresent(rcx, "cySubNetworks", .CLS$cySubNetworks, "rcx$cySubNetworks", fname)
        ids = unique(unlist(cyVisualProperties$view))
        ids = ids[!is.na(ids)]
        .checkRefs(ids, rcx$cySubNetworks$id, c("cyVisualProperties$view", "rcx$cySubNetworks$id"), fname)
    }
    
    if(is.null(rcx$cyVisualProperties)){
        rcx$cyVisualProperties = cyVisualProperties
    }else{
        rcx$cyVisualProperties = updateCyVisualProperties(rcx$cyVisualProperties, cyVisualProperties, 
                                                          replace, stopOnDuplicates)
    }
    
    return(rcx)
}

