################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Validate the RCX networks and aspects
################################################################################


#' Validate RCX and its aspects
#' 
#' Validate RCX objects and its aspects.
#' 
#' @details
#' Different tests are performed on aspects and the RCX network.
#' This includes checks of the correct aspect structure, data types, uniqueness of IDs and attribute names, 
#' presence of NA values, and references between the aspects.
#'
#' @param x object to validate; [RCX][RCX-object] object or an aspect
#' @param verbose logical; whether to print the test results.
#'
#' @return logical; whether the object passed all tests.
#'
#' @export
#' @example man-roxygen-examples/CX_load.R
#' @example man-roxygen-examples/validate.R
validate = function(x, verbose=T){
    UseMethod("validate", x)
}


#' @describeIn validate Default
#' @export
validate.default = function(x, verbose=T) {
    if(verbose) cat("Default case is not validated!\n")
    return(TRUE)
}


#' @describeIn validate Nodes
#' @export
validate.NodesAspect = function(x, verbose=T){
    aspect = x
    if(verbose) cat("Checking Nodes Aspect:\n")

    idColumn = "id"
    allowedColumns = c(idColumn, "name", "represents")

    pass = .test_IsClass(aspect, "nodes", verbose)

    test = .test_RequiredColumnsPresent(aspect, idColumn, verbose)
    pass = pass & test
    ## next tests only possible if id column is present
    if(test){
        pass = pass & .test_ContainsNA(aspect, idColumn, verbose)
        pass = pass & .test_OneNodePresent(aspect, idColumn, verbose)
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
        pass = pass & .test_IsNumeric(aspect, idColumn, verbose)
        pass = pass & .test_IsPos(aspect, idColumn, verbose)
    }
    pass = pass & .test_NoMergeColumn(aspect, "oldId", verbose)
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Nodes Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Edges
#' @export
validate.EdgesAspect = function(x, verbose=T){
    aspect = x
    if(verbose) cat("Checking Edges Aspect:\n")

    idColumn = "id"
    sourceColumn = "source"
    targetColumn = "target"
    requiredColumns = c(idColumn, sourceColumn, targetColumn)
    allowedColumns = c(requiredColumns, "name", "interaction")

    pass = .test_IsClass(aspect, "edges", verbose)

    test = .test_RequiredColumnsPresent(aspect, requiredColumns, verbose)
    pass = pass & test
    if(test){
        pass = pass & .test_ContainsNA(aspect, idColumn, verbose)
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
        pass = pass & .test_IsNumeric(aspect, idColumn, verbose)
        pass = pass & .test_IsPos(aspect, idColumn, verbose)

        pass = pass & .test_ContainsNA(aspect, sourceColumn, verbose)
        pass = pass & .test_IsNumeric(aspect, sourceColumn, verbose)
        pass = pass & .test_IsPos(aspect, sourceColumn, verbose)

        pass = pass & .test_ContainsNA(aspect, targetColumn, verbose)
        pass = pass & .test_IsNumeric(aspect, targetColumn, verbose)
        pass = pass & .test_IsPos(aspect, targetColumn, verbose)
    }
    pass = pass & .test_NoMergeColumn(aspect, "oldId", verbose)
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Edges Aspect:", pass, " ", T)
    invisible(pass)
}



#' Helper for validating node- and edge-attributes aspects 
#'
#' @param aspect an RCX aspect
#' @param verbose logical; whether to print the test results.
#'
#' @return logical; whether the test passed
#' 
#' @note Internal function only for convenience
#' @keywords internal
.validateAttributesAspect = function(aspect, verbose=T){
    if("subnetworkId" %in% colnames(aspect)){
        idColumn = c("propertyOf","name","subnetworkId")
    }else{
        idColumn = c("propertyOf","name")
    }
    
    ## allowed and required!
    allowedColumns = c(idColumn, "value", "dataType", "isList")

    pass = .test_RequiredColumnsPresent(aspect, allowedColumns, verbose)
    ## next tests only possible if required columns are present
    if(pass){
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)

        pass = pass & .test_ContainsNA(aspect, "propertyOf", verbose)
        pass = pass & .test_IsNumeric(aspect, "propertyOf", verbose)
        pass = pass & .test_IsPos(aspect, "propertyOf", verbose)

        pass = pass & .test_IsCharacter(aspect, "name", verbose)

        pass = pass & .test_IsList(aspect, "value", verbose)

        pass = pass & .test_ContainsNA(aspect, "dataType", verbose)
        pass = pass & .test_IsCharacter(aspect, "dataType", verbose)
        pass = pass & .test_DataTypeColumn(aspect, "dataType", verbose)
        
        pass = pass & .test_ContainsNA(aspect, "isList", verbose)
        pass = pass & .test_IsLogical(aspect, "isList", verbose)

        if("subnetworkId" %in% colnames(aspect)){
            pass = pass & .test_IsNumeric(aspect, "subnetworkId", verbose)
        }
    }
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    invisible(pass)
}


#' @describeIn validate Node attributes
#' @export
validate.NodeAttributesAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Node Attributes Aspect:\n")

    pass = .test_IsClass(aspect, "nodeAttributes", verbose)

    pass = pass & .validateAttributesAspect(aspect, verbose)

    if(verbose) .log(">> Node Attributes Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Edge attributes
#' @export
validate.EdgeAttributesAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Edge Attributes Aspect:\n")

    pass = .test_IsClass(aspect, "edgeAttributes", verbose)

    pass = pass & .validateAttributesAspect(aspect, verbose)

    if(verbose) .log(">> Edge Attributes Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Network attributes
#' @export
validate.NetworkAttributesAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Network Attributes Aspect:\n")

    pass = .test_IsClass(aspect, "networkAttributes", verbose)

    if("subnetworkId" %in% colnames(aspect)){
        idColumn = c("name","subnetworkId")
    }else{
        idColumn = c("name")
    }
    allowedColumns = c(idColumn, "value", "dataType", "isList")

    test = .test_RequiredColumnsPresent(aspect, allowedColumns, verbose)
    pass = pass & test
    ## next tests only possible if required columns are present
    if(test){
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)

        pass = pass & .test_IsCharacter(aspect, "name", verbose)

        pass = pass & .test_IsList(aspect, "value", verbose)
        
        pass = pass & .test_ContainsNA(aspect, "dataType", verbose)
        pass = pass & .test_IsCharacter(aspect, "dataType", verbose)
        pass = pass & .test_DataTypeColumn(aspect, "dataType", verbose)
        
        pass = pass & .test_ContainsNA(aspect, "isList", verbose)
        pass = pass & .test_IsLogical(aspect, "isList", verbose)

        if("subnetworkId" %in% colnames(aspect)){
            pass = pass & .test_IsNumeric(aspect, "subnetworkId", verbose)
        }
    }
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Network Attributes Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cartesian layout
#' @export
validate.CartesianLayoutAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cartesian Layout Aspect:\n")

    pass = .test_IsClass(aspect, "cartesianLayout", verbose)

    if("view" %in% colnames(aspect)){
        idColumn = c("node","view")
    }else{
        idColumn = c("node")
    }
    requiredColumns = c(idColumn, "x", "y")
    allowedColumns = c(requiredColumns, "z")


    test = .test_RequiredColumnsPresent(aspect, requiredColumns, verbose)
    pass = pass & test
    ## next tests only possible if required columns are present
    if(test){
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)

        pass = pass & .test_IsNumeric(aspect, "node", verbose)
        pass = pass & .test_ContainsNA(aspect, "node", verbose)

        pass = pass & .test_IsNumeric(aspect, "x", verbose)
        pass = pass & .test_ContainsNA(aspect, "x", verbose)

        pass = pass & .test_IsNumeric(aspect, "y", verbose)
        pass = pass & .test_ContainsNA(aspect, "y", verbose)

        if("z" %in% colnames(aspect)) pass = pass & .test_IsNumeric(aspect, "z", verbose)
        if("view" %in% colnames(aspect)) pass = pass & .test_IsNumeric(aspect, "view", verbose)
    }
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Cartesian Layout Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape Groups
#' @export
validate.CyGroupsAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Groups Aspect:\n")

    idColumn = "id"
    requiredColumns = c(idColumn, "name")
    atLeastOneColumns = c("nodes","externalEdges","internalEdges")
    allowedColumns = c(requiredColumns, atLeastOneColumns, "collapsed")

    pass = .test_IsClass(aspect, "cyGroups", verbose)

    test = .test_RequiredColumnsPresent(aspect, idColumn, verbose)
    pass = pass & test
    ## next tests only possible if id column is present
    if(test){
        pass = pass & .test_ContainsNA(aspect, idColumn, verbose)
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
        pass = pass & .test_IsNumeric(aspect, idColumn, verbose)
        pass = pass & .test_IsPos(aspect, idColumn, verbose)

        pass = pass & .test_IsCharacter(aspect, "name", verbose)
        pass = pass & .test_ContainsNA(aspect, "name", verbose)
    }

    pass = pass & .test_AtLeastOneColumnPresent(aspect, atLeastOneColumns, verbose)

    presentColumns = atLeastOneColumns[atLeastOneColumns %in% colnames(aspect)]
    for(col in presentColumns){
        pass = pass & .test_IsList(aspect, col, verbose)
        pass = pass & .test_ListAllNumeric(aspect, col, verbose)
    }

    if("collapsed" %in% colnames(aspect)) pass = pass & .test_IsLogical(aspect, "collapsed", verbose)

    pass = pass & .test_NoMergeColumn(aspect, "oldId", verbose)
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Cytoscape Groups Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape Visual Properties
#' @export
validate.CyVisualPropertiesAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Visual Properties Aspect:\n")
    
    optionalColumns = names(.DICT$VPpropertiesOf)

    pass = .test_IsClass(aspect, "cyVisualProperties", verbose)
    
    test = .test_IsNamedList(aspect, optionalColumns, verbose)
    test = test & .test_ListAllOfClass(aspect, .CLSvp$property, verbose)
    test = test & .test_AtLeastOneElementPresent(aspect, optionalColumns, verbose)
    pass = pass & test
    ## next tests only possible if required columns are present
    if(test){
        for(subaspect in names(aspect)){
            if(verbose) cat(paste0("For ",subaspect,": "))
            pass = pass & validate(aspect[[subaspect]], verbose)
        }
    }
    
    if(verbose) .log(">> Cytoscape Visual Properties Aspect:", pass, " ", T)
    invisible(pass)
}

#' @describeIn validate Cytoscape Visual Properties
#' @export
validate.CyVisualProperty = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Visual Property sub-aspect:\n")
    
    requiredColumns = c("appliesTo","view")
    optionalColumns = c("properties", "dependencies", "mappings")
    allowedColumns = c(requiredColumns, optionalColumns)
    
    pass = .test_IsCVPclass(aspect, "property", verbose)
    test = .test_IsNamedList(aspect, allowedColumns, verbose)
    pass = pass & test
    
    ## next tests only possible if required columns are present
    if(test){
        pass = pass & .test_ElementIsNumeric(aspect, "appliesTo", verbose)
        pass = pass & .test_ElementIsNumeric(aspect, "view", verbose)
        pass = pass & .test_IsUniqueInLists(aspect, requiredColumns, verbose)
    }    
    
    test = .test_AtLeastOneElementPresent(aspect, optionalColumns, verbose)
    pass = pass & test
    
    ## next tests only possible if allowed columns are present
    if(test){    
        if(("properties" %in% names(aspect)) && !is.na(aspect$properties)) {
            if(verbose) cat("  Checking Cytoscape Visual Property Properties:\n")
            pass = pass & .validateListOfCyVisualPropertyPandD(aspect, "properties", verbose)
        }
        
        if(("dependencies" %in% names(aspect)) && !is.na(aspect$dependencies)) {
            if(verbose) cat("  Checking Cytoscape Visual Property Dependencies:\n")
            pass = pass & .validateListOfCyVisualPropertyPandD(aspect, "dependencies", verbose)
        }
        
        if(("mappings" %in% names(aspect)) && !is.na(aspect$mappings)) {
            if(verbose) cat("  Checking Cytoscape Visual Property Mappings:\n")
            pass = pass & .test_ElementIsList(aspect, "mappings", verbose)
            
            mappings = aspect$mappings
            requiredColumns2 = c("name", "type", "definition")
            
            pass = pass & .test_ListAllOfClass(mappings, .CLSvp[["mappings"]], verbose)
            test2 = .test_ListRequiredColumnsPresent(mappings, requiredColumns2, verbose)
            pass = pass & test2
            
            if(test2){
                pass = pass & .test_ListAllCharacter(mappings, "name", verbose)
                pass = pass & .test_ListAllContainsNA(mappings, "name", verbose)
                pass = pass & .test_ListAllUnique(mappings, "name", verbose)
                
                pass = pass & .test_ListAllCharacter(mappings, "type", verbose)
                pass = pass & .test_ListAllContainsNA(mappings, "type", verbose)
                
                pass = pass & .test_ListAllCharacter(mappings, "definition", verbose)
                pass = pass & .test_ListAllContainsNA(mappings, "definition", verbose)
            }
            
            pass = pass & .test_ListAllowedColumnsPresent(mappings, requiredColumns2, verbose)
        }
        
    }
    
    if(verbose) .log(">> Cytoscape Visual Property sub-aspect:", pass, " ", F)
    invisible(pass)
}


#' Cytoscape visual property: List of property and dependency
#' 
#' For both properties the checks are the same.
#'
#' @param aspect either [CyVisualPropertyProperties] or [CyVisualPropertyDependencies] object
#' @param property character; name of the property
#' @param verbose logical; whether to print the test results.
#'
#' @return logical; whether the object passed all tests.
#' 
#' @note Internal function only for convenience
#' @keywords internal
.validateCyVisualPropertyPandD = function(aspect, property, verbose=T){
    pass = .test_IsCVPclass(aspect, property, verbose)

    requiredColumns = c("name", "value")
    
    pass = pass & .test_RequiredColumnsPresent(aspect, requiredColumns, verbose)
    ## next tests only possible if required columns are present
    if(pass){
        pass = pass & .test_IsCharacter(aspect, "name", verbose)
        pass = pass & .test_ContainsNA(aspect, "name", verbose)
        pass = pass & .test_IsUnique(aspect, "name", verbose)

        pass = pass & .test_IsCharacter(aspect, "value", verbose)
        pass = pass & .test_ContainsNA(aspect, "value", verbose)
    }

    pass = pass & .test_AllowedColumnsPresent(aspect, requiredColumns, verbose)

    invisible(pass)
}


#' @describeIn dot-validateCyVisualPropertyPandD List of property and dependency objects
.validateListOfCyVisualPropertyPandD = function(aspect, property, verbose=T){
    pass = .test_ElementIsList(aspect, property, verbose)
    
    vp = aspect[[property]]
    requiredColumns2 = c("name", "value")
    
    pass = pass & .test_ListAllOfClass(vp, .CLSvp[[property]], verbose)
    test2 = .test_ListRequiredColumnsPresent(vp, requiredColumns2, verbose)
    pass = pass & test2
    
    if(test2){
        pass = pass & .test_ListAllCharacter(vp, "name", verbose)
        pass = pass & .test_ListAllContainsNA(vp, "name", verbose)
        pass = pass & .test_ListAllUnique(vp, "name", verbose)
        
        pass = pass & .test_ListAllCharacter(vp, "value", verbose)
        pass = pass & .test_ListAllContainsNA(vp, "value", verbose)
    }
    
    pass = pass & .test_ListAllowedColumnsPresent(vp, requiredColumns2, verbose)
    
    invisible(pass)
}


#' @describeIn validate Cytoscape visual property: Properties
#' @export
validate.CyVisualPropertyProperties = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Properties of Cytoscape Visual Properties Aspect:\n")
    
    pass = .validateCyVisualPropertyPandD(aspect, "properties", verbose)

    if(verbose) .log(">> Properties of Cytoscape Visual Properties Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape visual property: Dependencies
#' @export
validate.CyVisualPropertyDependencies = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Dependencies of Cytoscape Visual Properties Aspect:\n")

    pass = .validateCyVisualPropertyPandD(aspect, "dependencies", verbose)

    if(verbose) .log(">> Dependencies of Cytoscape Visual Properties Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape visual property: Mappings
#' @export
validate.CyVisualPropertyMappings = function(x, verbose=T){
    aspect = x
    if(verbose) cat("Checking Mappings of Cytoscape Visual Properties Aspect:\n")

    pass = .test_IsCVPclass(aspect, "mappings", verbose)
    
    requiredColumns = c("name", "type", "definition")
    pass = .test_RequiredColumnsPresent(aspect, requiredColumns, verbose)
    ## next tests only possible if required columns are present
    if(pass){
        pass = pass & .test_IsCharacter(aspect, "name", verbose)
        pass = pass & .test_ContainsNA(aspect, "name", verbose)
        pass = pass & .test_IsUnique(aspect, "name", verbose)
        
        pass = pass & .test_IsCharacter(aspect, "type", verbose)
        pass = pass & .test_ContainsNA(aspect, "type", verbose)
        
        pass = pass & .test_IsCharacter(aspect, "definition", verbose)
        pass = pass & .test_ContainsNA(aspect, "definition", verbose)
    }
    
    pass = pass & .test_AllowedColumnsPresent(aspect, requiredColumns, verbose)

    if(verbose) .log(">> Mappings of Cytoscape Visual Properties Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape hidden attributes
#' @export
validate.CyHiddenAttributesAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Hidden Attributes Aspect:\n")

    pass = .test_IsClass(aspect, "cyHiddenAttributes", verbose)

    if("subnetworkId" %in% colnames(aspect)){
        idColumn = c("name","subnetworkId")
    }else{
        idColumn = c("name")
    }
    allowedColumns = c(idColumn, "value", "dataType", "isList")
    
    test = .test_RequiredColumnsPresent(aspect, allowedColumns, verbose)
    pass = pass & test
    ## next tests only possible if required columns are present
    if(test){
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
        
        pass = pass & .test_IsCharacter(aspect, "name", verbose)
        
        pass = pass & .test_IsList(aspect, "value", verbose)
        
        pass = pass & .test_ContainsNA(aspect, "dataType", verbose)
        pass = pass & .test_IsCharacter(aspect, "dataType", verbose)
        pass = pass & .test_DataTypeColumn(aspect, "dataType", verbose)
        
        pass = pass & .test_ContainsNA(aspect, "isList", verbose)
        pass = pass & .test_IsLogical(aspect, "isList", verbose)
        
        if("subnetworkId" %in% colnames(aspect)){
            pass = pass & .test_IsNumeric(aspect, "subnetworkId", verbose)
        }
    }
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Cytoscape Hidden Attributes Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape network relations
#' @export
validate.CyNetworkRelationsAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Network Relations Aspect:\n")

    pass = .test_IsClass(aspect, "cyNetworkRelations", verbose)

    idColumn = "child"
    requiredColumns = idColumn
    allowedColumns = c(requiredColumns, "isView", "parent", "name")

    test = .test_RequiredColumnsPresent(aspect, requiredColumns, verbose)
    pass = pass & test
    ## next tests only possible if required columns are present
    if(test){
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
    }
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if("isView" %in% colnames(aspect)){
        pass = pass & .test_IsLogical(aspect, "isView", verbose)
        pass = pass & .test_ContainsNA(aspect, "isView", verbose)
    }

    if("parent" %in% colnames(aspect)){
        pass = pass & .test_IsNumeric(aspect, "parent", verbose)
    }

    if("name" %in% colnames(aspect)){
        pass = pass & .test_IsCharacter(aspect, "name", verbose)
    }

    if(verbose) .log(">> Cytoscape Network Relations Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape sub-networks
#' @export
validate.CySubNetworksAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Subnetworks Aspect:\n")

    idColumn = "id"
    atLeastOneColumns = c("nodes","edges")
    allowedColumns = c(idColumn, atLeastOneColumns)

    pass = .test_IsClass(aspect, "cySubNetworks", verbose)

    test = .test_RequiredColumnsPresent(aspect, idColumn, verbose)
    pass = pass & test
    ## next tests only possible if id column is present
    if(test){
        pass = pass & .test_ContainsNA(aspect, idColumn, verbose)
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
        pass = pass & .test_IsNumeric(aspect, idColumn, verbose)
        pass = pass & .test_IsPos(aspect, idColumn, verbose)
    }

    pass = pass & .test_AtLeastOneColumnPresent(aspect, atLeastOneColumns, verbose)

    presentColumns = atLeastOneColumns[atLeastOneColumns %in% colnames(aspect)]
    for(col in presentColumns){
        pass = pass & .test_IsList(aspect, col, verbose)
        pass = pass & .test_ListAllNumericOrInDict(aspect, col, "SN", verbose)
    }

    pass = pass & .test_NoMergeColumn(aspect, "oldId", verbose)
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Cytoscape Subnetworks Aspect:", pass, " ", T)
    invisible(pass)
}


#' @describeIn validate Cytoscape table column aspect
#' @export
validate.CyTableColumnAspect = function(x, verbose=T){
    aspect = x
    
    if(verbose) cat("Checking Cytoscape Table Column Aspect:\n")

    pass = .test_IsClass(aspect, "cyTableColumn", verbose)

    idColumn = c("appliesTo","name")
    if("subnetworkId" %in% colnames(aspect)){
        idColumn = c(idColumn,"subnetworkId")
    }
    allowedColumns = c(idColumn, "dataType", "isList")
    
    test = .test_RequiredColumnsPresent(aspect, allowedColumns, verbose)
    pass = pass & test
    ## next tests only possible if required columns are present
    if(test){
        pass = pass & .test_IsUnique(aspect, idColumn, verbose)
        
        pass = pass & .test_IsCharacter(aspect, "appliesTo", verbose)
        pass = pass & .test_ContainsNA(aspect, "appliesTo", verbose)
        pass = pass & .test_ValuesInSet(aspect, "appliesTo", .DICT$TCappliesTo, F, verbose)

        pass = pass & .test_IsCharacter(aspect, "name", verbose)
        pass = pass & .test_ContainsNA(aspect, "name", verbose)

        pass = pass & .test_ContainsNA(aspect, "dataType", verbose)
        pass = pass & .test_IsCharacter(aspect, "dataType", verbose)
        pass = pass & .test_DataTypeColumn(aspect, "dataType", verbose)
        
        pass = pass & .test_ContainsNA(aspect, "isList", verbose)
        pass = pass & .test_IsLogical(aspect, "isList", verbose)
        
        if("subnetworkId" %in% colnames(aspect)){
            pass = pass & .test_IsNumeric(aspect, "subnetworkId", verbose)
        }
    }
    pass = pass & .test_AllowedColumnsPresent(aspect, allowedColumns, verbose)

    if(verbose) .log(">> Cytoscape Table Column Aspect:", pass, " ", T)
    invisible(pass)
}

#' @describeIn validate The whole RCX object with all its aspects
#' @export
validate.RCX = function(x, verbose=T){
    rcx = x
    
    testNodes = .test_AspectExist(rcx, "nodes", F)
    pass = testNodes

    testEdges = .test_AspectExist(rcx, "edges", F)
    testNodeAttributes = .test_AspectExist(rcx, "nodeAttributes", F)
    testEdgeAttributes = .test_AspectExist(rcx, "edgeAttributes", F)
    testNetworkAttributes = .test_AspectExist(rcx, "networkAttributes", F)
    testCartesianLayout = .test_AspectExist(rcx, "cartesianLayout", F)
    testCyGroups = .test_AspectExist(rcx, "cyGroups", F)
    testCyVisualProperties = .test_AspectExist(rcx, "cyVisualProperties", F)
    testCyHiddenAttributes = .test_AspectExist(rcx, "cyHiddenAttributes", F)
    testCyNetworkRelations = .test_AspectExist(rcx, "cyNetworkRelations", F)
    testCySubNetworks = .test_AspectExist(rcx, "cySubNetworks", F)
    testCyTableColumn = .test_AspectExist(rcx, "cyTableColumn", F)

    if(testNodes) {
        valNodes = validate(rcx$nodes, verbose)
        pass = pass & valNodes
    }
    if(testEdges) {
        valEdges = validate(rcx$edges, verbose)
        pass = pass & valEdges
    }
    if(testNodeAttributes) {
        valNodeAttributes = validate(rcx$nodeAttributes, verbose)
        pass = pass & valNodeAttributes
    }
    if(testEdgeAttributes) {
        valEdgeAttributes = validate(rcx$edgeAttributes, verbose)
        pass = pass & valEdgeAttributes
    }
    if(testNetworkAttributes) {
        valNetworkAttributes = validate(rcx$networkAttributes, verbose)
        pass = pass & valNetworkAttributes
    }
    if(testCartesianLayout) {
        valCartesianLayout = validate(rcx$cartesianLayout, verbose)
        pass = pass & valCartesianLayout
    }
    if(testCyGroups) {
        valCyGroups = validate(rcx$cyGroups, verbose)
        pass = pass & valCyGroups
    }
    if(testCyVisualProperties) {
        valCyVisualProperties = validate(rcx$cyVisualProperties, verbose)
        pass = pass & valCyVisualProperties
    }
    if(testCyHiddenAttributes) {
        valCyHiddenAttributes = validate(rcx$cyHiddenAttributes, verbose)
        pass = pass & valCyHiddenAttributes
    }
    if(testCyNetworkRelations) {
        valCyNetworkRelations = validate(rcx$cyNetworkRelations, verbose)
        pass = pass & valCyNetworkRelations
    }
    if(testCySubNetworks) {
        valCySubNetworks = validate(rcx$cySubNetworks, verbose)
        pass = pass & valCySubNetworks
    }
    if(testCyTableColumn) {
        valCyTableColumn = validate(rcx$cyTableColumn, verbose)
        pass = pass & valCyTableColumn
    }
    
    leftAspects = names(rcx)[! names(rcx) %in% names(aspectClasses)]
    if(length(leftAspects)!=0){
        if(verbose) cat("Checking Custom Aspects:\n")
        test = TRUE
        for(la in leftAspects) {
           test = test & validate(rcx[[la]], verbose) 
        }
        if(verbose) .log(">> Cytoscape Table Column Aspect:", test, " ", T)
        pass = pass & test
    }

    if(verbose) cat("Checking RCX:\n")
    pass = pass & .test_IsClass(rcx, "rcx", verbose)
    pass = pass & .test_AspectExist(rcx, "nodes", verbose)
    if(testNodes & verbose) .log("- Validate nodes aspect", valNodes)

    if(testEdges){
        if(verbose) .log("- Validate edges aspect", valEdges)

        test = testNodes && valNodes
        if(verbose) .log("  - Reference aspect (nodes) present and correct", test)
        if(test){
            pass = pass & .test_IdsInAspect(rcx$edges$source,
                                            rcx$nodes,
                                            "id",
                                            paste0(.CLS$edges,"$source ids in ",.CLS$nodes,"$id"),
                                            verbose)
            pass = pass & .test_IdsInAspect(rcx$edges$target,
                                            rcx$nodes,
                                            "id",
                                            paste0(.CLS$edges,"$target ids in ",.CLS$nodes,"$id"),
                                            verbose)
        }
    }

    if(testNodeAttributes){
        if(verbose) .log("- Validate node attributes aspect", valNodeAttributes)

        test = testNodes && valNodes
        if(verbose) .log("  - Reference aspect (nodes) present and correct", test)
        if(test){
            pass = pass & .test_IdsInAspect(rcx$nodeAttributes$propertyOf,
                                            rcx$nodes,
                                            "id",
                                            paste0(.CLS$nodeAttributes,"$propertyOf ids in ",.CLS$nodes,"$id"),
                                            verbose)
        }
        if("subnetworkId" %in% colnames(rcx$nodeAttributes)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                ids = rcx$nodeAttributes$subnetworkId
                ids = ids[!is.na(ids)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$nodeAttributes,"$subnetworkId ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(testEdgeAttributes){
        if(verbose) .log("- Validate edge attributes aspect", valEdgeAttributes)

        test = testEdges && valEdges
        if(verbose) .log("  - Reference aspect (edges) present and correct", test)
        if(test){
            pass = pass & .test_IdsInAspect(rcx$edgeAttributes$propertyOf,
                                            rcx$edges,
                                            "id",
                                            paste0(.CLS$edgeAttributes,"$propertyOf ids in ",.CLS$edges,"$id"),
                                            verbose)
        }
        if("subnetworkId" %in% colnames(rcx$edgeAttributes)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                ids = rcx$edgeAttributes$subnetworkId
                ids = ids[!is.na(ids)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$edgeAttributes,"$subnetworkId ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(testNetworkAttributes) {
        if(verbose) .log("- Validate network attributes aspect", valNetworkAttributes)
        if("subnetworkId" %in% colnames(rcx$networkAttributes)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                ids = rcx$networkAttributes$subnetworkId
                ids = ids[!is.na(ids)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$networkAttributes,"$subnetworkId ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(testCartesianLayout){
        if(verbose) .log("- Validate cartesian layout aspect", valCartesianLayout)
        
        test = testNodes && valNodes
        if(verbose) .log("  - Reference aspect (nodes) present and correct", test)
        if(test){
            pass = pass & .test_IdsInAspect(rcx$cartesianLayout$node,
                                            rcx$nodes,
                                            "id",
                                            paste0(.CLS$cartesianLayout,"$node ids in ",.CLS$nodes,"$id"),
                                            verbose)
        }
        if("view" %in% colnames(rcx$cartesianLayout)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                ids = rcx$cartesianLayout$view
                ids = ids[!is.na(ids)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$cartesianLayout,"$view ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(testCyGroups){
        if(verbose) .log("- Validate cytoscape groups aspect", valCyGroups)

        atLeastOneColumns = c(nodes="nodes",externalEdges="edges",internalEdges="edges")
        for(col in names(atLeastOneColumns)){
            test = F
            if(atLeastOneColumns[col]=="nodes"){
                test = testNodes & valNodes
            }else if(atLeastOneColumns[col]=="edges"){
                test = testEdges & valEdges
            }
            pass = pass & test
            if(verbose) .log(paste0("  - Reference aspect (",atLeastOneColumns[col],") present and correct"), test)
            if(test){
                ids = unique(unlist(rcx$cyGroups[,col]))
                ids = ids[!is.na(ids)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx[[atLeastOneColumns[col]]],
                                                "id",
                                                paste0(.CLS$cyGroups,"$",col," ids in ",.CLS[[atLeastOneColumns[col]]],"$id"),
                                                verbose)
            }
        }
    }

    if(testCyVisualProperties) {
        if(verbose) .log("- Validate cytoscape visual property aspect", valCyVisualProperties)

        pos = rcx$cyVisualProperties$aspect$propertiesOf
        index = pos=="nodes" || pos=="nodes:default"
        ids = unique(rcx$cyVisualProperties$aspect[index,"appliesTo"])
        if(length(ids)!=0){
            test = testNodes & valNodes
            pass = pass & test
            if(verbose) .log("  - Reference aspect (nodes) present and correct", test)
            if(test){
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$nodes,
                                                "id",
                                                paste0(.CLS$cyVisualProperties,"$aspect$appliesTo ids in ",.CLS$nodes,"$id"),
                                                verbose)
            }
        }

        index = pos=="edges" || pos=="edges:default"
        ids = unique(rcx$cyVisualProperties$aspect[index,"appliesTo"])
        if(length(ids)!=0){
            test = testEdges & valEdges
            pass = pass & test
            if(verbose) .log("  - Reference aspect (edges) present and correct", test)
            if(test){
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$edges,
                                                "id",
                                                paste0(.CLS$cyVisualProperties,"$aspect$appliesTo ids in ",.CLS$edges,"$id"),
                                                verbose)
            }
        }

    }

    if(testCyHiddenAttributes) {
        if(verbose) .log("- Validate cytoscape hidden attributes aspect", valCyHiddenAttributes)
        
        if("subnetworkId" %in% colnames(rcx$cyHiddenAttributes)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                pass = pass & .test_IdsInAspect(rcx$cyHiddenAttributes$subnetworkId,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$cyHiddenAttributes,"$subnetworkId ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(testCyNetworkRelations) {
        if(verbose) .log("- Validate cytoscape network relations aspect", valCyNetworkRelations)
        
        if("subnetworkId" %in% colnames(rcx$cyNetworkRelations)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                pass = pass & .test_IdsInAspect(rcx$cyNetworkRelations$child,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$cyNetworkRelations,"$child ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(testCySubNetworks){
        if(verbose) .log("- Validate cytoscape subnetworks aspect", valCySubNetworks)

        if(valCySubNetworks){
            test = testNodes & valNodes
            if(test){
                ids = unique(unlist(rcx$cySubNetworks[,"nodes"]))
                ids = ids[!is.na(ids)]
                ids = ids[!(ids %in% .DICT$SN)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$nodes,
                                                "id",
                                                paste0(.CLS$cySubNetworks,"$nodes ids in ",.CLS$nodes,"$id"),
                                                verbose)
            }

            test = testEdges & valEdges
            if(test){
                ids = unique(unlist(rcx$cySubNetworks[,"edges"]))
                ids = ids[!is.na(ids)]
                ids = ids[!(ids %in% .DICT$SN)]
                pass = pass & .test_IdsInAspect(ids,
                                                rcx$edges,
                                                "id",
                                                paste0(.CLS$cySubNetworks,"$edges ids in ",.CLS$edges,"$id"),
                                                verbose)
            }
        }
    }

    if(testCyTableColumn) {
        if(verbose) .log("- Validate cytoscape table column aspect", valCyTableColumn)
        
        if("subnetworkId" %in% colnames(rcx$cyTableColumn)){
            test = testCySubNetworks && valCySubNetworks
            if(verbose) .log("  - Reference aspect (subnetworks) present and correct", test)
            pass = pass & test
            if(test){
                pass = pass & .test_IdsInAspect(rcx$cyTableColumn$subnetworkId,
                                                rcx$cySubNetworks,
                                                "id",
                                                paste0(.CLS$cyTableColumn,"$subnetworkId ids in ",.CLS$cySubNetworks,"$id"),
                                                verbose)
            }  
        }
    }

    if(verbose) {
        .log(">> RCX:", pass, " ")
        invisible(pass)
    }else{
        return(pass)
    }
}
