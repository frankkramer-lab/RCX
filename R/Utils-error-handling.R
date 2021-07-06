################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Error codes used in this package
#' 
#' This function returns the error message to a given (internal) error code. For some codes, additional information for the message is needed.
#' 
#' @eval paste0("@section Details: \\subsection{List of error codes:}{\n", paste0("\\subsection{", names(.errorCodes(NULL)), "}{\n\\preformatted{", unlist(.errorCodes(NULL)), "}}", collapse="\n"), "}")
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param code character; Error code.
#' @param info character; Additional information used in some error codes.
#'
#' @return Full text for a given error code.
.errorCodes = function(code, info=c("<info[1]>", "<info[2]>")){
    # if(is.null(code)) info=c("<info[1]>", "<info[2]>")
    errorCodes = list(ErrorCodeNotFound=paste("\n##############################",
                                               "## !!ERROR CODE NOT FOUND!! ##",
                                               "##############################",
                                               "requested error code: ",info[1],
                                               sep="\n"),
                      paramMissingRCX="RCX object is missing!",
                      paramMissing=paste0("Missing arguments: ",info[1]),
                      idNonNeg=paste0("Provided IDs (",info[1],") must be non-neagtive!"),
                      idNotNum=paste0("Provided IDs (",info[1],") must be numeric!"),
                      idRefNotFound=paste0("Provided IDs of ",info[1]," don't exist in ",info[2]),
                      idRefNotPresent=paste0(info[1]," not present as ",info[2]),
                      paramAllNull=paste0("At least one argument of ",info[1]," must be set!"),
                      paramNotUnique=paste0("Elements of ",info[1]," must not contain duplicates!"),
                      paramNonNeg=paste0("All elements of ",info[1]," must be non-neagtive!"),
                      paramNa=paste0("Argument ",info[1]," must not contain any NA values!"),
                      paramNotNum=paste0("All elements of ",info[1]," must be numeric!"), #
                      paramNotChar=paste0("All elements of ",info[1]," must be characters!"),
                      paramNotLog=paste0("All elements of ",info[1]," must be logical!"),
                      paramNotList=paste0("Argument ",info[1]," must be a list!"),
                      paramNotNamed=paste0("Object ",info[1]," must have names!"),
                      paramDifferentLength=paste0("Arguments must have the same length!\n  ",info[1]),
                      paramListAllWrongClass=paste0('Not all elements of the list ',info[1],' are of class "',info[2],'"!'),
                      paramWrongValue=paste0('Argument ',info[1],' only can take following values: ',info[2]),
                      wrongClass=paste0('Class of object ',info[1],' is not "',info[2],'"!'), #
                      wrongClassOf=paste0('Class of object ',info[1],' is not one of ',info[2],'!'), #
                      validationFail=paste0("Aspect (",info[1],") failed validation!\nCheck if the aspect is valid: validate(",info[1],")"),
                      igraphEdgesRequired="RCX object requires edges to be converted to an igraph object!",
                      e404="THIS ERROR SHOULD NEVER HAPPEN!!!")
    if(is.null(code)) return(errorCodes[sort(names(errorCodes))])
    return(errorCodes[[code]])
}


#' Customized stop() function
#' 
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @inheritSection .errorCodes Details
#'
#' @param code character; Error code.
#' @param info character; Addidional information used in some error codes.
#' @param msg character; 
#'
#' @examples
#' \dontrun{
#' .stop("paramMissingRCX")
#' #Error: .stop
#' #    RCX object is missing!
#' 
#' .stop("paramNotUnique","idParamName")
#' #Error: .stop
#' #    Provided IDs (idParamName) contain duplicates!
#' 
#' .stop("wrongClass",c("nodesAspect", "NodesAspect"))
#' #Error: .stop
#' #    Class of object "nodesAspect" is not "NodesAspect"!
#' }
.stop = function(code, info=NULL, msg=NULL){
    ## get the name calling function
    callFuntionName = sys.call(1)[[1]]
    ## get the full text for the error code
    text = .errorCodes(code, info)
    ## if the error code is not found, raise an error with that code
    if(is.null(text)) .stop("ErrorCodeNotFound", code)
    stop(paste0(callFuntionName, "\n  ", text, msg), call. = F)
}


