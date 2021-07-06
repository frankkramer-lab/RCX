## a minimal example
cyNetworkRelations = createCyNetworkRelations(
  child = 1
)

## with all parameters
cyNetworkRelations = createCyNetworkRelations(
  child = c(1,2),
  parent = c(NA,1),
  name = c("Network A",
           "View A"),
  isView = c(FALSE, TRUE)
)

