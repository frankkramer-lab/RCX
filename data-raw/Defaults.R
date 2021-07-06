################################################################################
## Authors:
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##
## Description:
##    Default values used and needed for RCX definition and usage
################################################################################

## Default classes of the RCX related objects
.CLS = list(rcx = "RCX",
            metaData = "MetaDataAspect",
            nodes = "NodesAspect",
            edges = "EdgesAspect",
            nodeAttributes = "NodeAttributesAspect",
            edgeAttributes = "EdgeAttributesAspect",
            networkAttributes = "NetworkAttributesAspect",
            cartesianLayout = "CartesianLayoutAspect",
            cyGroups = "CyGroupsAspect",
            cyVisualProperties = "CyVisualPropertiesAspect",
            cyHiddenAttributes = "CyHiddenAttributesAspect",
            cyNetworkRelations = "CyNetworkRelationsAspect",
            cySubNetworks = "CySubNetworksAspect",
            cyTableColumn = "CyTableColumnAspect")


## Default subclasses for CyVisualPropertiesAspect
.CLSvp = list(property = "CyVisualProperty",
              #propertyList = "CyVisualPropertyList",
              properties = "CyVisualPropertyProperties",
              dependencies = "CyVisualPropertyDependencies",
              mappings = "CyVisualPropertyMappings")


## Default property names for IDs of aspects
.IDProp = list(nodes = "id",
               edges = "id",
               cyGroups = "id",
               cySubNetworks = "id")

## 
.VPref = list(network="cySubNetworks", 
              nodes="nodes", edges="edges", 
              defaultNodes="nodes", defaultEdges="edges")

## Default mapping from RCX to CX aspect property names
## The properties follow a different naming convention in CX and RCX.
## RCX names ==> CX names
.RCX2CX = list(nodes=c(name="n",
                       represents="r"),
               edges=c(source="s",
                       target="t",
                       interaction="i"),
               nodeAttributes=c(propertyOf="po",
                                name="n",
                                value="v",
                                dataType="d",
                                subnetworkIs="s"),
               edgeAttributes=c(propertyOf="po",
                                name="n",
                                value="v",
                                dataType="d",
                                subnetworkIs="s"),
               networkAttributes=c(name="n",
                                   value="v",
                                   dataType="d",
                                   subnetworkIs="s"),
               cyGroups=c(name="n",
                          nodes="nodes",
                          externalEdges="external_edges",
                          internalEdges="internal_edges",
                          collapsed="collapsed"),
               cyHiddenAttributes=c(name="n",
                                    value="v",
                                    dataType="d",
                                    subnetworkIs="s"),
               cyNetworkRelations=c('@id'="c",
                                    parentNetwork="p",
                                    type="r",
                                    name="name"))


## Default mapping from CX to RCX aspect property names
## The properties follow a different naming convention in CX and RCX.
## Just reverse of .RCX2CX (uses .switchRCXandCX to generate it from .RCX2CX)
## CX names ==> RCX names
switchFunction = function(x, .list=FALSE){
  if(.list) x = unlist(x)
  rcx = names(x)
  names(rcx) = x
  if(.list) rcx = as.list(rcx)
  return(rcx)
}
.CX2RCX = plyr::llply(.RCX2CX,switchFunction)

.CLSnames = switchFunction(.CLS, TRUE)


.JSONdataTypes = c("boolean",
                   "integer",
                   "long",
                   "double",
                   "string")


## values for dictionary columns
.DICT = list(VPpropertiesOf=c(network="network", 
                              nodes="nodes", edges="edges", 
                              defaultNodes="nodes:default", defaultEdges="edges:default"),
             VPpropertyFields=c("properties", "dependencies", "mappings"),
             SN=c("all"),
             TCappliesTo=c(node_table="nodes", edge_table="edges", network_table="networks"))
             # TCappliesTo=c("node_table", "edge_table", "network_table"))

.html = list(
  html.part1 = readLines("data-raw/html/index.part1.html"),
  html.part2 = readLines("data-raw/html/index.part2.html"),
  html.part3 = readLines("data-raw/html/index.part3.html")
)



usethis::use_data(.CLS, .CLSnames, 
                  .CLSvp,
                  .IDProp,
                  .VPref,
                  .RCX2CX, .CX2RCX, 
                  .JSONdataTypes, 
                  .DICT, 
                  .html,
                  internal = TRUE,
                  overwrite = TRUE)




