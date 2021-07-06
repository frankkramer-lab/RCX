## a minimal example
nodeAttributes = createNodeAttributes(
  propertyOf=1, 
  name="A", 
  value="a"
)

## defining several properties at once
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1), 
  name=c("A", "B"), 
  value=c("a","b")
)

## with characters and numbers mixed
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list("a",3.14)
)

## force the number to be characters
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list("a",3.14),
  dataType=c("string","string")
)

## with a list as input for one value
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b")
)

## force "B" to be a list as well
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1),
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b"),
  isList=c(TRUE,TRUE)
)

## with a subnetwork
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1),
  name=c("A","A"),
  value=c("a","a with subnetwork"),
  subnetworkId=c(NA,1)
)

## with all parameters
nodeAttributes = createNodeAttributes(
  propertyOf=c(1,1,1,1,1,1),
  name=c("A","A","b","d","i","l"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             TRUE,
             3.14,
             314,
             314),
  dataType=c("string","string","boolean","double","integer","long"), 
  isList=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE),
  subnetworkId=c(NA,1,NA,NA,NA,NA)
)
