.onLoad <- function(libname, pkgname) {
  RCX.options = options()$RCX.options
  doWrite = FALSE
  
  if(is.null(RCX.options)){
    RCX.options = list(aspectClasses=aspectClasses, extensions=list())
    doWrite = TRUE
  }
  
  if(is.null(RCX.options$extensions)){
    RCX.options$extensions=list()
    doWrite = TRUE
  }
  
  if(doWrite) options(RCX.options = RCX.options)

  invisible()
}
