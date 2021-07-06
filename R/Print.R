################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Print functions for RCX and aspect classes
#' 
#' These functions attempt to print [RCX][RCX-object] and aspect objects in a more readable form.
#'
#' @param x aspect or [RCX][RCX-object] object
#' @param ... further arguments passed to or from other methods. See [base::print()]
#'
#' @seealso [summary]
#' @name custom-print
#' @export
#' @examples 
#' rcx = createRCX(createNodes())
#' print(rcx)
print.MetaDataAspect = function(x, ...){
    cat("Meta-data:\n")
    .removeClass(x) = .CLS$metaData
    if("properties" %in% colnames(x)){
        x$properties = lapply(x$properties, function(p){
            result = ""
            if(length(p)!=0) result = paste0(names(p),"=",p, collapse = ",")
            return(result)
        })
    }
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.NodesAspect = function(x, ...){
    cat("Nodes:\n")
    .removeClass(x) = .CLS$nodes
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.EdgesAspect = function(x, ...){
    cat("Edges:\n")
    .removeClass(x) = .CLS$edges
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.NodeAttributesAspect = function(x, ...){
    cat("Node attributes:\n")
    .removeClass(x) = .CLS$nodeAttributes
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.EdgeAttributesAspect = function(x, ...){
    cat("Edge attributes:\n")
    .removeClass(x) = .CLS$edgeAttributes
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.NetworkAttributesAspect = function(x, ...){
    cat("Network attributes:\n")
    .removeClass(x) = .CLS$networkAttributes
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CartesianLayoutAspect = function(x, ...){
    cat("Cartesian layout:\n")
    .removeClass(x) = "CartesianLayoutAspect"
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyGroupsAspect = function(x, ...){
    cat("Cytoscape Groups:\n")
    .removeClass(x) = "CyGroupsAspect"
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyVisualPropertyProperties = function(x, ...){
    cat("[[Properties]]\n")
    .removeClass(x) = "CyVisualPropertyProperties"
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyVisualPropertyDependencies = function(x, ...){
    cat("[[Dependencies]]\n")
    .removeClass(x) = "CyVisualPropertyDependencies"
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyVisualPropertyMappings = function(x, ...){
    cat("[[Mappings]]\n")
    .removeClass(x) = "CyVisualPropertyMappings"
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyVisualProperty = function(x, fields=c("all"), ...){
    if((!fields=="all") && !.elementsInDict(fields, "VPpropertyFields")) .stop("paramWrongValue", c("fields", .DICT$VPpropertyFields))
    first = ifelse(length(x$appliesTo)!=1,TRUE,FALSE)
    for(i in 1:length(x$appliesTo)){
        if(first){
            first=FALSE
        }else{
            cat("\n")
        }
        # if(is.na(x$appliesTo[i]) && is.na(x$view[i])) {
        #     cat("<DEFAULT>\n")
        # }else if((!is.na(x$appliesTo[i])) && (!is.na(x$view[i]))){
        #     cat(paste0("<APPLIES TO: ",x$appliesTo[i],", VIEW: ",x$view[i],">\n"))
        # }else{
        #     if(!is.na(x$appliesTo[i])) cat(paste0("<APPLIES TO: ",x$appliesTo[i],">\n"))
        #     if(!is.na(x$view[i])) cat(paste0("<VIEW: ",x$view[i],">\n"))
        # }
        cat(paste0("<APPLIES TO: ",x$appliesTo[i],", VIEW: ",x$view[i],">\n"))
        
        if(((fields=="all") || ("properties" %in% fields)) && .paramClass(x$properties[[i]],.CLSvp$properties)) print(x$properties[[i]], ...)
        if(((fields=="all") || ("dependencies" %in% fields)) && .paramClass(x$dependencies[[i]],.CLSvp$dependencies)) print(x$dependencies[[i]], ...)
        if(((fields=="all") || ("mappings" %in% fields)) && .paramClass(x$mappings[[i]],.CLSvp$mappings)) print(x$mappings[[i]], ...)
    }
}

#' @rdname custom-print
#' @param propertyOf character; Which properties should be shown, one of `r .pasteC(.DICT$VPpropertiesOf)` or `all`
#' @param fields character; Which fields should be shown, one of `r .pasteC(.DICT$VPpropertyFields)` or `all`
#' @export
print.CyVisualPropertiesAspect = function(x, propertyOf="all", fields="all", ...){
    if((!propertyOf=="all") && !.elementsInDict(propertyOf, "VPpropertiesOf")) .stop("paramWrongValue", c("fields", .DICT$VPpropertiesOf))
    if((!fields=="all") && !.elementsInDict(fields, "VPpropertyFields")) .stop("paramWrongValue", c("fields", .DICT$VPpropertyFields))

    if((propertyOf!="all") && (propertyOf %in% names(x))){
        elements = propertyOf
    }else{
        elements = .DICT$VPpropertiesOf[.DICT$VPpropertiesOf %in% names(x)]
    }
    
    cat("Cytoscape visual properties:\n")
    for(n in elements){
        cat(paste0("[[cyVisualProperties]][[",n,"]] = Cytoscape visual property:\n"))
        print(x[[n]], fields, ...)
    }
}

#' @rdname custom-print
#' @export
print.CyHiddenAttributesAspect = function(x, ...){
    cat("Cytoscape hidden attributes:\n")
    .removeClass(x) = .CLS$cyHiddenAttributes
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyNetworkRelationsAspect = function(x, ...){
    cat("Cytoscape network relations:\n")
    .removeClass(x) = .CLS$cyNetworkRelations
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CySubNetworksAspect = function(x, ...){
    cat("Cytoscape subnetworks:\n")
    .removeClass(x) = .CLS$cySubNetworks
    print(x, ...)
}

#' @rdname custom-print
#' @export
print.CyTableColumnAspect = function(x, ...){
    cat("Cytoscape table column lables and types:\n")
    .removeClass(x) = .CLS$cyTableColumn
    print(x, ...)
}

#' @rdname custom-print
#' @param inofficial logical; if `FALSE` only the official aspects are printed
#' @export
print.RCX = function(x, inofficial=T, ...){
    aspects = names(x)
    #firstly print all official aspects in the order given by .CLS
    ordered = names(.CLS)
    ordered = ordered[ordered %in% aspects]
    remaining = aspects[!aspects %in% ordered]
    first = T
    for(n in ordered){
        if(!first) cat("\n")
        cat(paste0("[[",n,"]] = "))
        print(x[[n]], ...)
        first = F
    }
    ## then print all remaining (self defined)
    if(inofficial){
        for(n in remaining){
            if(!first) cat("\n")
            cat(paste0("[[",n,"]] = "))
            print(x[[n]], ...)
            first = F
        }
    }
}








