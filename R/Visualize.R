################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


#' Visualize a Network
#' 
#' Visualize [RCX][RCX-object] and CX networks in RStudio or in an external browser.
#' 
#' @details 
#' This function uses the Java Script library used by the NDEx platform (\url{https://ndexbio.org/}) to visualize
#' the [RCX][RCX-object] or CX network from [toCX]. 
#' In the first case, the [RCX][RCX-object] is converted to CX (JSON) using [toCX].
#' 
#' By default the visualization is opened in RStudio in the *Viewer* panel.
#' If this function is not executed in RStudio, the visualization is opened in the standard web-browser.
#' This also can be forced from within RStudio using *openExternal*.
#' 
#' If the network contains the necessary Cytoscape styles (see \url{http://manual.cytoscape.org/en/stable/Styles.html})
#' the network is visualized as seen on the NDEx platform.
#' 
#' To define the layout of the network the coordinate from [CartesianLayout] are used to determine the location of 
#' the nodes. If this aspect is missing, or the the coordinates should be ignored, the *layout* parameter can be used
#' to set a different layout.
#' 
#' *layout* follows therefore the definition of Cytoscape.js (see \url{https://js.cytoscape.org/#layouts}).
#' A simple definition can be setting only the *name* of the desired layout, e.g. `random`.
#' Additional options can be passed as named list, where the values are passed without quoting. 
#' This allows for even passing Java Script functions to Cytoscape.js.
#' 
#' The visualization can also be saved as HTML file using the [writeHTML] function instead of this one.
#'
#' @param x network; [RCX][RCX-object] or CX object
#' @param layout named character or list; e.g. `c(name="random")`
#' @param openExternal logical; whether to open in an external browser instead of the RStudio viewer
#' 
#' @return `NULL`
#'
#' @export
#' @seealso [rcxToJson], [readCX], [writeCX]
#'
#' @examples
#' ## prepare RCX
#' rcx = createRCX(
#'   createNodes(name = c("a","b","c")), 
#'   createEdges(
#'     source=c(0,0,1), 
#'     target=c(1,2,2)
#'   )
#' )
#' 
#' \donttest{
#' ## visualize the network
#'  visualize(rcx)
#' 
#' ## force a different layout
#'  visualize(rcx, c(name="cose"))
#' 
#' ## force a different layout with Java Script parameters
#'  visualize(rcx, layout = c(name="random",animate="true"))
#' 
#' ## even pass a Java Script function
#'  visualize(
#'    rcx, 
#'    layout = c(
#'      name="random",
#'      animate="true",
#'      animateFilter="function ( node, i ){ return true; }"
#'    )
#'  )
#' 
#' ## open the visualization in an external browser
#'  visualize(
#'    rcx, 
#'    layout = c(name="cose"),
#'    openExternal = TRUE
#'  )
#' }
visualize = function(x, layout=NULL, openExternal=FALSE){
  UseMethod("visualize", x)
}


#' @rdname visualize
#' @export
visualize.RCX = function(x, layout=NULL, openExternal=FALSE) {
  cx = toCX(x)
  visualize(cx, layout, openExternal)
  return(invisible(NULL))
}


#' @rdname visualize
#' @export
visualize.CX = function(x, layout=NULL, openExternal=FALSE) {
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  
  writeHTML(x, htmlFile, layout=layout)
  
  viewer <- getOption("viewer")
  if (is.null(viewer) || openExternal)
    utils::browseURL(htmlFile)
  else
    viewer(htmlFile)
  
  return(invisible(NULL))
}


#' Save network visualization as HTML file
#' 
#' Save an interactive single page visualization of [RCX][RCX-object] and CX networks as an HTML file 
#' containing all necessary Java Script.
#' 
#' @details 
#' This function uses the Java Script library used by the NDEx platform (\url{https://ndexbio.org/}) to visualize
#' the [RCX][RCX-object] or [CX][readCX] network. 
#' The [RCX][RCX-object] is therefore converted to CX (JSON) using [toCX].
#' 
#' If the network contains the necessary Cytoscape styles (see \url{http://manual.cytoscape.org/en/stable/Styles.html})
#' the network is visualized as seen on the NDEx platform.
#' 
#' To define the layout of the network the coordinate from [CartesianLayout] are used to determine the location of 
#' the nodes. If this aspect is missing, or the the coordinates should be ignored, the *layout* parameter can be used
#' to set a different layout.
#' 
#' *layout* follows therefore the definition of Cytoscape.js (see \url{https://js.cytoscape.org/#layouts}).
#' A simple definition can be setting only the *name* of the desired layout, e.g. `random`.
#' Additional options can be passed as named list, where the values are passed without quoting. 
#' This allows for even passing Java Script functions to Cytoscape.js.
#' 
#' To visualize the network in RStudio the [visualize] function can be used instead.
#'
#' @param x network; [RCX][RCX-object] or CX object
#' @param file character; path, where the html file should be saved
#' @param layout named character or list; e.g. `c(name="random")` 
#' @param verbose logical; whether to print what is happening 
#' 
#' @return file character; path, where the html file has been saved
#'
#' @export
#' @seealso [rcxToJson], [readCX], [writeCX]
#'
#' @examples
#' ## prepare RCX
#' rcx = createRCX(
#'   createNodes(name = c("a","b","c")), 
#'   createEdges(
#'     source=c(0,0,1), 
#'     target=c(1,2,2)
#'   )
#' )
#' 
#' cx = toCX(rcx)
#' 
#' htmlFile = tempfile(fileext = ".html")
#' 
#' ## save the html
#' writeHTML(rcx, htmlFile)
#' 
#' ## or
#' writeHTML(cx, htmlFile)
#' 
#' ## force a different layout
#' writeHTML(rcx, htmlFile, c(name="cose"))
#' 
#' ## force a different layout with Java Script parameters
#' writeHTML(rcx, htmlFile, layout = c(name="random",animate="true"))
#' 
#' ## even pass a Java Script function
#' writeHTML(
#'    rcx, 
#'    htmlFile, 
#'    layout = c(
#'      name="random",
#'      animate="true",
#'      animateFilter="function ( node, i ){ return true; }"
#'    )
#' )
writeHTML = function(x, file, layout=NULL, verbose=FALSE){
  UseMethod("writeHTML", x)
}


#' @rdname writeHTML
#' @export
writeHTML.RCX = function(x, file, layout=NULL, verbose=FALSE){
  if(verbose) cat("Convert RCX to CX (JSON)\n")
  cx = toCX(x, verbose)
  res = writeHTML(cx, file, layout, verbose)
  return(res)
}


#' @rdname writeHTML
#' @export
writeHTML.CX = function(x, file, layout=NULL, verbose=FALSE){
  if(!endsWith(file,".html")) file = paste0(file,".html")
  htmlFile = file
  
  if(!is.null(layout)){
    lnames = names(layout)
    if(! "name" %in% lnames) layout$name = "none"
    layoutTmp = paste0("name:'",layout["name"],"'")
    layout = layout[names(layout)!="name"]
    if(length(layout)!=0){
      layoutTmp = c(layoutTmp, 
                    paste0(names(layout),":",layout))
    }
    layout = paste0(layoutTmp, collapse = ",")
    layout = paste0("var layout = {",layout,"};")
  }
  
  html = c(
    .html$html.part1,
    x,
    .html$html.part2,
    ifelse(is.null(layout),"var layout = cx2Js.getDefaultLayout();", layout),
    .html$html.part3
  )
  
  if(verbose) cat(paste0('Writing html file to: ',htmlFile,'\n'))
  writeLines(html, htmlFile)
  
  return(file)
}
