################################################################################
## Authors:
##   Florian Auer
##
## Description:
##    Helper functions: Checks for parameters, ids, elements and lists
################################################################################


#' Checks
#' 
#' Different functions to check parameters, ids, elements and lists
#' 
#' @details 
#' The `.check*` functions perform a test and stop with a custom error on fail. 
#' All other functions perform a test and return the result.
#' 
#' The used **.DICT:** looks as follows:
#' @eval paste0("@details", paste0(" * ", names(.DICT), ": ", vapply(names(.DICT), function(x){.pasteC(.DICT[[x]])},character(1)), collapse="\n"))
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param param some parameter.
#' @param A,B vectors.
#' @param L list.
#' @param DF data.frame.
#' @param cls character; class name.
#' @param clss character vector; list of class names.
#' @param cols column names.
#' @param key name of the dictionary entry in `.DICT`.
#' @param ... list of some vectors.
#' @param name character; for logging the used name for the parameter.
#' @param cname character; for logging the name of the calling function.
#'
#' @return logical
#'
#' @examples
#' NULL
#' @name checks
NULL

#' @describeIn checks checks if the object `param` is of class `cls`.
.paramClass = function(param, cls){
    pass = cls %in% class(param)
    return(pass)
}

#' @describeIn checks checks if the object `param` is of class `cls`.
.checkClass = function(param, cls, name, cname=c()){
    pass = .paramClass(param, cls)
    if(!pass) .stop("wrongClass",c(.formatLog(name, cname), cls))
}

#' @describeIn checks checks if all elements of the list `L` are of class `cls`.
.checkAllClass = function(L, cls, name, cname=c()){
    L[is.na(L)] = NULL
    test = vapply(L,function(x){.paramClass(x, cls)}, logical(1))
    pass = all(test)
    if(!pass) .stop("wrongClass",c(.formatLog(name, cname), cls))
}

#' @describeIn checks checks if `param` is any class of `clss`.
.checkClassOneOf = function(param, clss, name, cname=c()){
    pass = .aspectClass(param) %in% clss
    if(!pass) .stop("wrongClassOf",c(.formatLog(name, cname), .formatParams(clss, "or")))
}

#' @describeIn checks checks if `param` is character.
.checkCharacter = function(param, name, cname=c()){
    pass = is.character(param)
    if(!pass) .stop("paramNotChar",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` is numeric.
.checkNumeric = function(param, name, cname=c()){
    pass = is.numeric(param)
    if(!pass) .stop("paramNotNum",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` is logical.
.checkLogical = function(param, name, cname=c()){
    pass = is.logical(param)
    if(!pass) .stop("paramNotLog",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` is a list.
.checkList = function(param, name, cname=c()){
    pass = is.list(param)
    if(!pass) .stop("paramNotList",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` has names.
.paramNamed = function(param){
    pass = !is.null(names(param))
    return(pass)
}

#' @describeIn checks checks if `param` has names.
.checkNamed = function(param, name, cname=c()){
    pass = .paramNamed(param)
    if(!pass) .stop("paramNotNamed",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` has names and is character.
.checkNamedCharacter = function(param, name, cname=c()){
    .checkCharacter(param, name, cname)
    .checkNamed(param, name, cname)
}

#' @describeIn checks checks if `param` has names and is numeric.
.checkNamedNumeric = function(param, name, cname=c()){
    .checkNumeric(param, name, cname)
    .checkNamed(param, name, cname)
}

#' @describeIn checks checks if `param` has names and is logical.
.checkNamedLogical = function(param, name, cname=c()){
    .checkLogical(param, name, cname)
    .checkNamed(param, name, cname)
}

#' @describeIn checks checks if `param` has names and is a list.
.checkNamedList = function(param, name, cname=c()){
    .checkList(param, name, cname)
    .checkNamed(param, name, cname)
}

#' @describeIn checks checks if `param` is greater than 0 if not NA.
.paramNonNeg = function(param){
    param = param[!is.na(param)]
    pass = all(param>=0)
    return(pass)
}

#' @describeIn checks checks if `param` is greater than 0 if not NA.
.checkNonNeg = function(param, name, cname=c()){
    pass = .paramNonNeg(param)
    if(!pass) .stop("paramNonNed",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` is not NA.
.paramNoNa = function(param){
    pass = all(! is.na(param))
    return(pass)
}

#' @describeIn checks checks if `param` is not NA.
.checkNoNa = function(param, name, cname=c()){
    pass = all(! is.na(param))
    if(!pass) .stop("paramNa",.formatLog(name, cname))
}

#' @describeIn checks checks if `param` is an unique id.
.checkIsUniqueId = function(param, name, cname=c()){
    .checkIsId(param, name, cname)
    .checkUnique(param, name, cname)
    return(TRUE)
}

#' @describeIn checks checks if `param` is an id.
.checkIsId = function(param, name, cname=c()){
    .checkNumeric(param, name, cname)
    .checkNonNeg(param, name, cname)
    .checkNoNa(param, name, cname)
    return(TRUE)
}

#TODO: needed? Would be checkIsOptionalId()
#' @describeIn checks checks if `param` is an optional id.
.paramIsOptionalId = function(param, name){
    if(! .paramNonNeg(param)) .stop('idNonNeg', name)
    if(! is.numeric(param)) .stop("idNotNum", name)
    return(TRUE)
}

#' @describeIn checks checks if all elements in `...` have the same number of elements.
.checkSameLength = function(cname, ...){
    if(missing(cname) || is.null(cname)) stop("name of the calling funtion has to be set!")
    params = list(...)
    names(params) = lapply(substitute(list(...))[-1], deparse)
    
    paramsPrintLength = vapply(params, function(x){ifelse(is.null(x),"<ignored>",as.character(length(x)))}, character(1))
    ## ignore NULL element (i.e. parameter not set)
    params[vapply(params, is.null, logical(1))] = NULL
    paramsLength = vapply(params, length, integer(1))

    test = length(unique(paramsLength))==1
    if(!test) .stop("paramDifferentLength",
                    .formatLog(
                        paste0(cname, 
                               "(",
                               paste(names(params), paramsLength, 
                                     sep=":", collapse = ", ")
                               ,")")))
    return(test)
}

#' @describeIn checks checks if any element in `...` is not NULL.
.paramAnyNotNull = function(name, ...){
    if(missing(name) || is.null(name)) stop("name of the calling funtion has to be set!")
    params = list(...)

    pass = any(!vapply(params, is.null, logical(1)))
    return(pass)
}

#' @describeIn checks checks if any element in `...` is not NULL.
.checkAnyNotNull = function(name, cname=c(), ...){
    if(missing(name) || is.null(name)) .stop("paramMissing", "name")
    params = list(...)
    pass = .paramAnyNotNull(name, ...)
    if(!pass) .stop("paramAllNull", .formatLog(.formatParams(name, "or"), cname))
}

#' @describeIn checks checks if the elements in `A` are unique.
.elementsUnique = function(A){
    pass = length(A)==length(unique(A))
    return(pass)
}

#' @describeIn checks checks if the elements in `A` are unique.
.checkUnique = function(A, name, cname=c()){
    if(!.elementsUnique(A)) .stop("paramNotUnique",.formatLog(name, cname))
}

#' @describeIn checks checks if the elements in the columns `cols` of `DF` are unique.
.elementsUniqueDF = function(DF, cols){
    pass = !any(unlist(plyr::dlply(DF, cols,function(x){nrow(x)>1})))
    return(pass)
}

#' @describeIn checks checks if the elements in the columns `cols` of `DF` are unique.
.checkUniqueDF = function(DF, cols, cname=c()){
    pass = .elementsUniqueDF(DF, cols)
    if(!pass) .stop("paramNotUnique", .formatLog(cols, cname))
}

#' @describeIn checks checks if the elements of `A` are in `.DICT[[key]]`.
.elementsInDict = function(A, key){
    pass = all(A %in% .DICT[[key]])
    return(pass)
}

#' @describeIn checks checks if all elements of `A` are present in `B`.
.elementsBContainsAllA = function(A, B){
    pass = all(A %in% B)
    return(pass)
}

#' @describeIn checks checks if all elements of `A` are present in `B`.
.checkBContainsAllA = function(A, B, name, cname=c()){
    pass = .elementsBContainsAllA(A, B)
    if(!pass) .stop("paramWrongValue", c(.formatLog(name, cname),.formatParams(B, con="or")))
}

#' @describeIn checks checks if `B` contains all elements of `A`, aka. references.
.checkRefs = function(A, B, name, cname=c()){
    if(length(A)==0){
        pass = TRUE
    }else{
        pass = .elementsBContainsAllA(A, B)
    }
    if(!pass) .stop("idRefNotFound", c(.formatLog(name[1], cname), .formatQuote(name[2])))
}

#' @describeIn checks checks if a referred aspect of class `cls` is accessible by `key` in `A`.
.checkRefPresent = function(A, key, cls, name, cname=c()){
    pass = key %in% names(A)
    if(!pass) .stop("idRefNotPresent", c(.formatQuote(cls), .formatLog(name, cname)))
    .checkClass(A[[key]], cls, name, cname)
}

#' @describeIn checks checks if all elements of a list `L` are numeric.
.listAllNumeric = function(L){
    pass = all(vapply(L, function(x){is.numeric(x)||is.na(x)||is.null(x)}, logical(1)))
    return(pass)
}

#' @describeIn checks checks if all elements of a list `L` are numeric.
.checkAllNumeric = function(L, name, cname=c()){
    pass = .listAllNumeric(L)
    if(!pass) .stop("paramListAllWrongClass", c(.formatLog(name, cname),.formatQuote("numeric")))
}

#' @describeIn checks checks if all elements of a list `L` are numeric or in `.DICT[[key]]`.
.listAllNumericOrInDict = function(L, key){
    pass = all(vapply(L, function(x){is.numeric(x)||is.na(x)||is.null(x)||(x %in% .DICT[[key]])}, logical(1)))
    return(pass)
}

#' @describeIn checks checks if all elements of a list `L` are numeric or in `.DICT[[key]]`.
.checkAllNumericOrInDict = function(L, key, name, cname=c()){
    pass = .listAllNumericOrInDict(L, key)
    if(!pass) .stop("paramListAllWrongClass",c(.formatLog(name, cname),"numeric"))
}
