## a minimal example
cyGroups = createCyGroups(
  name = "Group One",
  nodes = list(c(1,2,3)),
  internalEdges = list(c(0,1))
)

## defining several groups at once
cyGroups = createCyGroups(
  name = c("Group One", "Group Two"),
  nodes = list(c(1,2,3), 0),
  internalEdges = list(c(0,1),NA)
)

## with all parameters
cyGroups = createCyGroups(
  id = c(0,1),
  name = c("Group One", "Group Two"),
  nodes = list(c(1,2,3), 0),
  internalEdges = list(c(0,1),NA),
  externalEdges = list(NA,c(1,3)),
  collapsed = c(TRUE,NA)                     
)

