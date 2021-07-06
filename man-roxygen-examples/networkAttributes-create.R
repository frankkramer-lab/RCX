## a minimal example
networkAttributes = createNetworkAttributes(
  name="A", 
  value="a"
)

## defining several properties at once
networkAttributes = createNetworkAttributes(
  name=c("A", "B"), 
  value=c("a","b")
)

## with characters and numbers mixed
networkAttributes = createNetworkAttributes(
  name=c("A","B"),
  value=list("a",3.14)
)

## force the number to be characters
networkAttributes = createNetworkAttributes(
  name=c("A","B"),
  value=list("a",3.14),
  dataType=c("character","character")
)

## with a list as input for one value
networkAttributes = createNetworkAttributes(
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b")
)

## force "B" to be a list as well
networkAttributes = createNetworkAttributes(
  name=c("A","B"),
  value=list(c("a1","a2"),
             "b"),
  isList=c(TRUE,TRUE)
)

## with a subnetwork
networkAttributes = createNetworkAttributes(
  name=c("A","A"),
  value=c("a","a with subnetwork"),
  subnetworkId=c(NA,1)
)

## with all parameters
networkAttributes = createNetworkAttributes(
  name=c("A","A","B","B"),
  value=list(c("a1","a2"),
             "a with subnetwork",
             "b",
             "b with subnetwork"),
  isList=c(TRUE,FALSE,TRUE,FALSE),
  subnetworkId=c(NA,1,NA,1)
)
