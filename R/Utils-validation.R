################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Helping tests
#' 
#' Tests for validating RCX objects and its aspects.
#' 
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @param rcx RCX object
#' @param aspect one RCX aspect
#' @param columns character; list of columns
#' @param column character; column name
#' @param names character; names of list
#' @param cls character; class name in .CLS or .CLSvp
#' @param dic character; key in .DICT
#' @param ids numeric; ids
#' @param ignoreNA logical (default=TRUE); ignore NA values
#' @param verbose logical (default=FALSE); also log the results
#' @param info character (default=""); additional message for verbose
#'
#' @return logical; pass or fail the test
#'
#' @name dot_test
NULL

#' @describeIn dot_test checks if aspect has all required columns
.test_RequiredColumnsPresent = function(aspect, columns, verbose=FALSE){
    pass =  all(columns %in% colnames(aspect))
    if(verbose) .log(paste0('- All required columns present (',.pasteC(columns),')'), pass)
    return(pass)
}

#' @describeIn dot_test checks if all list elements have all required columns
.test_ListRequiredColumnsPresent = function(aspect, columns, verbose=FALSE){
    pass =  all(sapply(aspect, function(x){all(columns %in% colnames(x))}))
    if(verbose) .log(paste0('- All required columns present (',.pasteC(columns),')'), pass)
    return(pass)
}

#' @describeIn dot_test checks if only allowed columns are set
.test_AllowedColumnsPresent = function(aspect, columns, verbose=FALSE){
    pass = all(colnames(aspect) %in% columns)
    if(verbose) .log(paste0("- Only allowed columns present (",.pasteC(columns),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if all list elements have only allowed columns
.test_ListAllowedColumnsPresent = function(aspect, columns, verbose=FALSE){
    pass = all(colnames(aspect) %in% columns)
    if(verbose) .log(paste0("- Only allowed columns present (",.pasteC(columns),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if column with old ids is not present (would be a merge artefact)
.test_NoMergeColumn = function(aspect, column, verbose=FALSE){
    pass = ! column %in% colnames(aspect)
    if(verbose) .log(paste0("- No merge artefacts present (i.e. column with old ids: ",.pasteC(column),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if at least one specified column is present
.test_AtLeastOneColumnPresent = function(aspect, columns, verbose=FALSE){
    pass = any(colnames(aspect) %in% columns)
    if(verbose) .log(paste0("- At least one of the columns present (",.pasteC(columns),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if at least one specified element is present
.test_AtLeastOneElementPresent = function(aspect, element, verbose=FALSE){
    pass = any(names(aspect) %in% element)
    if(pass){
        pass = any(sapply(aspect[names(aspect) %in% element], function(x){!is.na(x)}))
    }
    if(verbose) .log(paste0("- At least one of the elements present (",.pasteC(element),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if at least one element (node) is present in the specified column
.test_OneNodePresent = function(nodesAspect, column, verbose=FALSE){
    pass = length(nodesAspect[,column])>0
    if(verbose) .log(paste0("- At least one node present"), pass)
    return(pass)
}

#' @describeIn dot_test checks if all elements in specified column are unique
.test_IsUnique = function(aspect, column, verbose=FALSE){
    pass = length(aspect[,column])==length(unique(aspect[,column]))
    text = ifelse(length(column)==1, "Column", "Combination of columns")
    if(verbose) .log(paste0("- ",text," (",.pasteC(column),") contains only unique values"), pass)
    return(pass)
}

#' @describeIn dot_test checks for all list elements if all elements in specified column are unique
.test_ListAllUnique = function(aspect, column, verbose=FALSE){
    pass = all(sapply(aspect, function(x){length(x[,column])==length(unique(x[,column]))}))
    if(verbose) .log(paste0("- All list elements (",.pasteC(column),") contain only unique values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if all elements in specified column are unique
.test_IsUniqueInLists = function(aspect, column, verbose=FALSE){
    aspect = as.data.frame(aspect[column])
    pass = nrow(aspect)==nrow(unique(aspect))
    text = ifelse(length(column)==1, "Column", "Combination of columns")
    if(verbose) .log(paste0("- ",text," (",.pasteC(column),") contains only unique values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if all elements in specified column are unique
.test_ListAllUniqueInLists = function(aspect, column, verbose=FALSE){
    pass = all(sapply(aspect, function(x){
        .test_IsUniqueInLists(x, column, FALSE)
    }))
    text = ifelse(length(column)==1, "Column", "Combination of columns")
    if(verbose) .log(paste0("- ",text," (",.pasteC(column),") contains only unique values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of type logical
.test_IsLogical = function(aspect, column, verbose=FALSE){
    pass = is.logical(aspect[,column])
    if(verbose) .log(paste0("- Column (",column,") only contains logical values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of type numeric
.test_IsNumeric = function(aspect, column, verbose=FALSE){
    pass = is.numeric(aspect[,column])
    if(verbose) .log(paste0("- Column (",column,") only contains numeric values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of type numeric
.test_ElementIsNumeric = function(aspect, element, verbose=FALSE){
    pass = is.numeric(aspect[[element]])
    if(verbose) .log(paste0("- List element (",element,") only contains numeric values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of type character
.test_IsCharacter = function(aspect, column, verbose=FALSE){
    pass = is.character(aspect[,column])
    if(verbose) .log(paste0("- Is the column (",column,") a character vector"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified list element are all of type character
.test_ListAllCharacter = function(aspect, element, verbose=FALSE){
    pass = all(sapply(aspect[[element]], function(x){is.character(x)}))
    if(verbose) .log(paste0("- All list elements of ",element," contain only character values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of type list
.test_IsList = function(aspect, column, verbose=FALSE){
    pass = is.list(aspect[,column])
    if(verbose) .log(paste0("- Is the column (",column,") a list"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of type list
.test_ElementIsList = function(aspect, element, verbose=FALSE){
    pass = is.list(aspect[[element]])
    if(verbose) .log(paste0("- Is ",element," a list"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column are positive integers
.test_IsPos = function(aspect, column, verbose=FALSE){
    pass = all(aspect[,column]>=0)
    if(verbose) .log(paste0("- Column (",column,") only contains positive (>=0) values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of the specified class in .CLS
.test_IsClass = function(x, cls, verbose=FALSE){
    pass = .CLS[[cls]] %in% class(x)
    if(verbose) .log(paste0("- Is object of class \"",.CLS[[cls]],"\""), pass)
    return(pass)
}

#' @describeIn dot_test checks if the aspect is a list with specified names
.test_IsNamedList = function(aspect, names, verbose=FALSE){
    pass = is.list(aspect)
    if(verbose) .log(paste0("- Is object a list"), pass)
    if(pass) pass = all(names(aspect) %in% names)
    if(verbose) .log(paste0("- Is a named list (",.formatParams(names, con = "or"),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is of the specified class in .CLSvp
.test_IsCVPclass = function(x, cls, verbose=FALSE){
    pass = .CLSvp[[cls]] %in% class(x)
    if(verbose) .log(paste0("- Is object of class \"",.CLSvp[[cls]],"\""), pass)
    return(pass)
}

#' @describeIn dot_test checks if the all elements in the list are of class in .CLSvp
.test_ListOfCVPclass = function(x, cls, verbose=FALSE){
    pass = all(sapply(x, function(y){.CLSvp[[cls]] %in% class(y)}))
    if(verbose) .log(paste0("- All objects of class \"",.CLSvp[[cls]],"\""), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column contains any NA values
.test_ContainsNA = function(aspect, column, verbose=FALSE){
    pass = all(!is.na(aspect[,column]))
    if(verbose) .log(paste0("- Column (",column,") doesn't contain any NA values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified list element contains any NA values
.test_ListAllContainsNA = function(aspect, element, verbose=FALSE){
    pass = all(sapply(aspect[[element]], function(x){all(!is.na(x))}))
    if(verbose) .log(paste0("- All list elements of ",element," don't contain any NA values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is a list with only numeric values (NAs and NULLs are not considered)
.test_ListAllNumeric = function(aspect, column, verbose=FALSE){
    pass = all(sapply(aspect[,column], function(x){is.numeric(x)||is.na(x)||is.null(x)}))
    if(verbose) .log(paste0("- The list (column: ",column,") contains only numeric values"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is a list with only numeric values (NAs and NULLs are not considered) or in .DICT
.test_ListAllNumericOrInDict = function(aspect, column, dic, verbose=FALSE){
    pass = all(sapply(aspect[,column], function(x){is.numeric(x)||is.na(x)||is.null(x)||(x %in% .DICT[[dic]])}))
    if(verbose) .log(paste0("- The list (column: ",column,") contains only numeric values or dictionary entries (",.pasteC(.DICT[[dic]]),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column is a list with only numeric values (NAs and NULLs are not considered) or in .DICT
.test_ListAllOfClass = function(aspect, cls, verbose=FALSE){
    pass = all(sapply(aspect, function(x){cls %in% class(x)}))
    if(verbose) .log(paste0("- The list only contains entries of class \"",cls,"\""), pass)
    return(pass)
}

#' @describeIn dot_test checks if the rcx object contains the specified apsect
.test_AspectExist = function(rcx, aspect, verbose=FALSE){
    pass = aspect %in% names(rcx)
    if(verbose) .log(paste0("- ",aspect," aspect is present"), pass)
    return(pass)
}

#' @describeIn dot_test checks if all provided ids are present in the specified column of an aspect
.test_IdsInAspect = function(ids, aspect, column, info="", verbose=FALSE){
    pass = all(ids %in% aspect[,column])
    if(verbose) .log(paste0("  - All id references exist (",info,")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the specified column of an aspect only contains values of the provided set
.test_ValuesInSet = function(aspect, column, set, ignoreNA=TRUE, verbose=FALSE){
    values = aspect[,column]
    if(ignoreNA) values = values[!is.na(values)]
    pass = all(values %in% set)
    if(verbose) .log(paste0("- All values of ",column," are in the allowed set (",.pasteC(set),")"), pass)
    return(pass)
}

#' @describeIn dot_test checks if the `dataType` column of an aspect only contains JSON data types.
.test_DataTypeColumn = function(aspect, column, verbose=FALSE){
    pass = .test_ValuesInSet(aspect, column, .JSONdataTypes, FALSE, verbose)
    return(pass)
}




