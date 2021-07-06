## a minimal example
cySubNetworks = createCySubNetworks(
  nodes = "all",
  edges = "all"
)

## defining several subnetworks at once
cySubNetworks = createCySubNetworks(
  nodes = list("all",
               c(1,2,3)),
  edges = list("all",
               c(0,2))
)

## with all parameters
cySubNetworks = createCySubNetworks(
  id = c(0,1),
  nodes = list("all",
               c(1,2,3)),
  edges = list("all",
               c(0,2))                    
)

