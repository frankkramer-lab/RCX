## a minimal example
edgeAttributes = createEdgeAttributes(
  propertyOf=1, 
  name="A", 
  value="a"
)

## defining several properties at once
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1), 
  name=c("A", "B"), 
  value=c("a","b")
)

## with characters and numbers mixed
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list("a",3.14)
)

## force the number to be characters
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list("a",3.14),
  dataType=c("character","character")
)

## with a list as input for one value
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b")
)

## force "B" to be a list as well
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b"),
  isList=c(TRUE,TRUE)
)

## with a subnetwork
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1),
  name=c("A","A"),
  value=c("a","a with subnetwork"),
  subnetworkId=c(NA,1)
)

## with all parameters
edgeAttributes = createEdgeAttributes(
  propertyOf=c(1,1,1,1),
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)
