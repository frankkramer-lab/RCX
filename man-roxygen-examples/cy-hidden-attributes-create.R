## a minimal example
hiddenAttributes = createCyHiddenAttributes(
  name="A", 
  value="a"
)

## defining several properties at once
hiddenAttributes = createCyHiddenAttributes(
  name=c("A", "B"), 
  value=c("a","b")
)

## with characters and numbers mixed
hiddenAttributes = createCyHiddenAttributes(
  name=c("A","B"),
  value=list("a",3.14)
)

## force the number to be characters
hiddenAttributes = createCyHiddenAttributes(
  name=c("A","B"),
  value=list("a",3.14),
  dataType=c("character","character")
)

## with a list as input for one value
hiddenAttributes = createCyHiddenAttributes(
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b")
)

## force "B" to be a list as well
hiddenAttributes = createCyHiddenAttributes(
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b"),
  isList=c(TRUE,TRUE)
)

## with a subnetwork
hiddenAttributes = createCyHiddenAttributes(
  name=c("A","A"),
  value=c("a","a with subnetwork"),
  subnetworkId=c(NA,1)
)

## with all parameters
hiddenAttributes = createCyHiddenAttributes(
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)
