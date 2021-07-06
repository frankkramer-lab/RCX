## minimal example
rcx = createRCX(createNodes())

## create by aspect
nodes = createNodes(name = c("a","b","c"))
edges = createEdges(source=c(0,0), target=c(1,2))

nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=c("a","b")
)

edgeAttributes = createEdgeAttributes(
  propertyOf=c(0,0), 
  name=c("A", "B"), 
  value=c("a","b")
)

networkAttributes = createNetworkAttributes(
  name=c("A","B"),
  value=list("a",3.14)
)

cartesianLayout = createCartesianLayout(
  node=c(0, 1),
  x=c(5.5, 110.1), 
  y=c(200.3, 210.2)
)

cyGroups = createCyGroups(
  name = c("Group One", "Group Two"),
  nodes = list(c(0,1), 0)                   
)

vpPropertyP = createCyVisualPropertyProperties(c(NODE_BORDER_STROKE="SOLID"))
vpPropertyD = createCyVisualPropertyDependencies(c(nodeSizeLocked="false"))
vpPropertyM = createCyVisualPropertyMappings(c(NODE_FILL_COLOR="CONTINUOUS"), 
                                             "COL=directed,T=boolean,K=0=true,V=0=ARROW")
vpProperty = createCyVisualProperty(properties=vpPropertyP, 
                                    dependencies=vpPropertyD, 
                                    mappings=vpPropertyM)

cyVisualProperties = createCyVisualProperties(nodes=vpProperty)

cyHiddenAttributes = createCyHiddenAttributes(
  name=c("A","B"),
  value=list(c("a1","a2"), "b")
)

cyNetworkRelations = createCyNetworkRelations(
  child = c(0,1),
  name = c("Network A", NA)
)

cySubNetworks = createCySubNetworks(
  nodes = list("all", c(0,1,2)),
  edges = list("all", c(0,1))                    
)

cyTableColumn = createCyTableColumn(
  appliesTo=c("nodes","edges","networks"),
  name=c("weight","weight","collapsed"),
  dataType=c("double","double","boolean")
)

rcx = createRCX(nodes, edges,
                nodeAttributes, edgeAttributes,
                networkAttributes, 
                cartesianLayout,
                cyGroups, 
                cyVisualProperties, 
                cyHiddenAttributes, 
                cyNetworkRelations, 
                cySubNetworks, 
                cyTableColumn)

## create all at once
rcx = createRCX(
  createNodes(name = c("a","b","c")), 
  createEdges(source=c(0,0), target=c(1,2)),
  createNodeAttributes(
    propertyOf=c(1,1),
    name=c("A","B"),
    value=c("a","b")
  ), 
  createEdgeAttributes(
    propertyOf=c(0,0), 
    name=c("A", "B"), 
    value=c("a","b")
  ),
  networkAttributes = createNetworkAttributes(
    name=c("A","B"),
    value=list("a",3.14)
  ), 
  cartesianLayout = createCartesianLayout(
    node=c(0, 1),
    x=c(5.5, 110.1), 
    y=c(200.3, 210.2)
  ),
  createCyGroups(
    name = c("Group One", "Group Two"),
    nodes = list(c(0,1), 0)                   
  ), 
  createCyVisualProperties(
    nodes=createCyVisualProperty(
      properties=createCyVisualPropertyProperties(
        c(NODE_BORDER_STROKE="SOLID")
      ), 
      dependencies=createCyVisualPropertyDependencies(
        c(nodeSizeLocked="false")
      ), 
      mappings=createCyVisualPropertyMappings(
        c(NODE_FILL_COLOR="CONTINUOUS"), 
        "COL=directed,T=boolean,K=0=true,V=0=ARROW")
    )
  ), 
  createCyHiddenAttributes(
    name=c("A","B"),
    value=list(c("a1","a2"), "b")
  ), 
  createCyNetworkRelations(
    child = c(0,1),
    name = c("Network A", NA)
  ), 
  createCySubNetworks(
    nodes = list("all", c(0,1,2)),
    edges = list("all", c(0,1))                    
  ), 
  createCyTableColumn(
    appliesTo=c("nodes","edges","networks"),
    name=c("weight","weight","collapsed"),
    dataType=c("double","double","boolean")
  )
)
