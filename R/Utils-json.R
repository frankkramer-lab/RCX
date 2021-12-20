################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Return data as a vector from a JSON list
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param data json list
#' @param acc accession name
#' @param default default return value
#' @param returnAllDefault whether to return the vector if all values are the default value (or `NULL` instead)
#'
#' @return vector
#'
#' @examples
#' testData = list(list(n="CDKN1B"),
#'                 list(n="ROCK1", r="BLA"),
#'                 list(n="SHC1", r="BLUBB"),
#'                 list(n="IRS1"))
#' RCX:::.jsonV(testData, "r")
.jsonV = function(data, acc, default=NA, returnAllDefault=TRUE){
  cls = class(unlist(lapply(data, 
                            function(a){
                              if(!acc %in% names(a)) return(NA)
                              return(a[[acc]])
                            })))
  result = vapply(data, 
                  function(a){
                    if(!acc %in% names(a)) return(methods::
                                                    as(default, cls))
                    return(a[[acc]])
                  },
                  methods::as(TRUE, cls))
  if(!returnAllDefault){
    if(is.na(default)){
      if(all(is.na(result))) result = NULL
    }else{
      if(all(result==default)) result = NULL
    }
  }
  return(result)
}


#' Return data as a list from a JSON list
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param data json list
#' @param acc accession name
#' @param default default return value
#' @param unList logical; whether to unlist the list elements (e.g. for a list of lists return a list of vectors)
#' @param returnAllDefault whether to return the vector if all values are the default value (or `NULL` instead)
#'
#' @return list
#'
#' @examples
#' testData = list(list(n="CDKN1B"),
#'                 list(n="ROCK1", r="BLA"),
#'                 list(n="SHC1", r="BLUBB"),
#'                 list(n="IRS1"))
#' RCX:::.jsonL(testData, "r")
.jsonL = function(data, acc, default=as.character(NA), unList=TRUE, returnAllDefault=TRUE){
  result = lapply(data, function(a){
    if(!acc %in% names(a)) return(default)
    a = a[[acc]]
    if(unList) a = unlist(a) 
    return(a)
  })
  if(!returnAllDefault){
    if(is.na(default)){
      
      test = vapply(result, 
                    function(x){
                      if(is.list(x)){
                        if(length(x)==0) return(TRUE)
                        return(all(vapply(x, is.na, logical(1))))
                      } 
                      return(all(is.na(x)))
                    },
                    logical(1))
      
      if(all(test)) {
        result = NULL
      }
    }else{
      if(all(vapply(result, 
                    function(x){
                      return(x==default)
                    }, 
                    logical(1)))) {
        result = NULL
      }
    }
  }
  return(result)
}


#' Get the data type and isList from JSON data
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param dataType data type column from jsonToRCX => .jsonV
#' @param default default value for `NA` values (by default the values remain `NA`)
#'
#' @return `list(type=<character vector>, isList=<logical vector>)`
#'
#' @examples
#' jsonD = c("boolean", "double", "integer", "long", "string", 
#'           "list_of_boolean", "list_of_double", "list_of_integer", 
#'           "list_of_long", "list_of_string")
#' 
#' RCX:::.json2RDataType(jsonD)
.json2RDataType = function(dataType, default="string"){
  isList = ifelse(is.na(dataType),FALSE,startsWith(dataType, "list_of_"))
  
  typeSet = !is.na(dataType)
  dataType[!typeSet] = default
  dataType[typeSet] = sub("list_of_","",dataType[typeSet])
  
  return(list(type=dataType, isList=isList))
}


#' Filter several parameters for elements, that match to a given name in a given param
#'
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @param name character; matching value
#' @param param character; in which param in ...
#' @param ... several parameters
#'
#' @return list with only matching elements of all parameters
#'
#' @examples
#' po=c("match","not","some","other","match")
#' prop=c(1,2,3,4,5)
#' dep=c("bla","blubb","bla","blubb","bla")
#' map=list("BLA","BLUBB","BLA","BLUBB","BLA") 
#' 
#' RCX:::.filterBy("match", "po", po, prop, dep, map)
.filterBy = function(name, param, ...){
  params = list(...)
  names(params) = lapply(substitute(list(...))[-1], deparse)
  
  i = params[[param]] == name
  if(!any(i)) return(list())
  return(lapply(params, function(x){x[i]}))
}


#' Rename data.frame columns by key-value pairs in rnames
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param df data.frame
#' @param rnames named character vector; `names(rnames)=colnames(df)`
#'
#' @return df with new colnames; or NULL on error
#'
#' @examples
#' nodes = data.frame(id=c(0,1,2),
#'                    name=c("CDK1",NA,"CDK3"),
#'                    represents=c(NA,"bla",NA))
#' rnames = c(id="@id", name="n", represents="r")
#' RCX:::.renameDF(nodes, rnames)
.renameDF = function(df, rnames) {
  if(!is.data.frame(df)) return(NULL)
  dfNames = colnames(df)
  colnames(df) = vapply(dfNames, 
                        function(n){
                          if(n %in% names(rnames)) n = rnames[n]
                          return(n)
                        },
                        character(1))
  return(df)
}


#' Add the aspect name to the JSON
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param json character; preformated json
#' @param name character; name of the aspect
#'
#' @return character; character of json object
#'
#' @examples
#' json = '{bla:"BLA", blubb:"BLUBB"}'
#' RCX:::.addAspectNameToJson(json, "foo")
.addAspectNameToJson = function(json, name){
  json = paste0('{"',name,'":',json,"}")
  return(json)
}


#' Convert data to json by R class 
#'
#' @note Internal function only for convenience
#' @keywords internal
#' 
#' @param x data element
#' @param raw character; names of columns not to format (e.g. because it is already converted)
#'
#' @return character; json
#' @name convert2json
#'
#' @examples
#' NULL
.convert2json = function(x, ...){
  UseMethod(".convert2json",x)
}

#' @rdname convert2json
.convert2json.character = function(x){
  ## escape characters
  x = gsub(r"(\)",r"(\\)",x, fixed=TRUE)
  x = gsub("\n",r"(\\n)",x, fixed=TRUE)
  x = gsub("\t",r"(\\t)",x, fixed=TRUE)
  x = gsub("\r",r"(\\r)",x, fixed=TRUE)
  x = gsub(r"(")",r"(\")",x, fixed=TRUE)
  
  isNa = is.na(x)
  x[!isNa] = paste0('"',x[!isNa],'"')
  return(x)
}

#' @rdname convert2json
.convert2json.numeric = function(x){
  return(as.character(x))
}

#' @rdname convert2json
.convert2json.integer = .convert2json.numeric


#' @rdname convert2json
.convert2json.logical = function(x){
  x[!is.na(x)] = ifelse(x[!is.na(x)],'"true"','"false"')
  return(x)
}

#' @rdname convert2json
.convert2json.list = function(x, raw=c(), byElement=FALSE, skipNa=TRUE){
  if(skipNa) x[is.na(x)] = NULL
  x[vapply(x,is.null,logical(1))] = NULL
  
  nx = names(x)
  
  inRaw = nx %in% raw
  x[!inRaw] = vapply(x[!inRaw], 
                     function(x){.convert2json(x)},
                     character(1))
  
  if(byElement){
    result = paste0('{"name":"',nx,'","value":',x,'}', collapse = ",")
    result = paste0("[",result,"]")
    # result = paste0('{"',nx,'":',x,'}', collapse = ",")
    # result = paste0("[",result,"]")
  }else{
    result = paste0('"',nx,'":',x, collapse = ",")
    result = paste0("{",result,"}")
  }
  return(result)
}

#' @rdname convert2json
.convert2json.data.frame = function(x, raw=c(), skipNa=TRUE){
  for(col in colnames(x)){
    if(!col %in% raw) x[,col] = .convert2json(x[,col])
  }
  
  result = apply(x, 1, function(row){
    row = .convert2json(as.list(row), 
                        raw=colnames(x), 
                        skipNa=skipNa)
    return(row)
  })
  
  result = paste0(result, collapse = ",")
  result = paste0("[",result,"]")
  return(result)
}


#' Convert data types in `data.frame(dataType,isList)` to character of NDEx data types
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param df data.frame with dataType and isList: `data.frame(dataType,isList)`
#' @param cols named character; column names of dataType and isList in df
#'
#' @return character; NDEx data types (e.g. "string" or "list_of_integer")
#' @name convert-data-types-and-values
#'
#' @examples
#' df = data.frame(dataType=c("string","boolean","double","integer","long",
#'                            "string","boolean","double","integer","long"),
#'                 isList=c(FALSE,FALSE,FALSE,FALSE,FALSE,
#'                          TRUE,TRUE,TRUE,TRUE,TRUE))
#' df$value = list("string",TRUE,3.14,314,314,
#'                 c("str","ing"),c(TRUE,FALSE),c(3.14,1.0),c(314,666),c(314,666))
#' RCX:::.convertDataTypes(df)
#' RCX:::.convertValues(df)
.convertDataTypes = function(df, cols=c(dataType="dataType", isList="isList")){
  result = df[,cols["dataType"]]
  result = paste0(ifelse(df[,cols["isList"]],"list_of_",""), result)
  return(result)
}

#' @rdname convert-data-types-and-values
.convertValues = function(df, cols=c(value="value", isList="isList")){
  result = vapply(df[,cols["value"]], 
                  function(v){
                    if(is.null(v)){
                      v=""
                    }else{
                      v = .convert2json(v)
                    }
                    v = paste0(v, collapse = ",")
                    return(v)
                  },
                  character(1))
  result = ifelse(df[,cols["isList"]], paste0("[",result,"]"), result)
  return(result)
}


#' Convert a list of vectors to a character vector with pasted elements
#' 
#' @note Internal function only for convenience
#' @keywords internal
#'
#' @param l unnamed list
#' @param keepNa logical; whether to keep `NA` values or replace it with an empty list
#'
#' @return character
#'
#' @examples
#' l  = list(NA,c(2,3), 5)
#' RCX:::.convertRawList(l)
.convertRawList = function(l, keepNa=TRUE){
  result = NULL
  if(!is.null(l)){
    result = vapply(l, 
                    function(v){
                      if((length(v)==1)&&(is.na(v))){
                        if(!keepNa){
                          v = "[]"
                        }else{
                          v = as.character(NA)
                        }
                      }else{
                        v = .convert2json(v)
                        v = paste0(v, collapse = ",")
                        v = paste0("[",v,"]")
                      } 
                      return(v)
                    },
                    character(1))
  }
  return(result)
}


