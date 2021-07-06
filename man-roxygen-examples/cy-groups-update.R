## For CyGroupsAspects: 
## prepare some aspects:
cyGroups1 = createCyGroups(
  name = c("Group One", "Group Two"),
  nodes = list(c(1,2,3), 0),
  internalEdges = list(c(0,1),NA),
  externalEdges = list(NA,c(2,3)),
  collapsed = c(TRUE,NA)                     
)

cyGroups2 = createCyGroups(
  name = "Group Three",
  nodes = list(c(4,5)),
  externalEdges = list(c(4,5))                     
)

## group ids will be kept
cyGroups3 = updateCyGroups(cyGroups1, cyGroups2)

## old group ids will be omitted
cyGroups3 = updateCyGroups(cyGroups1, cyGroups2,
                           keepOldIds=FALSE)

## Raise an error if duplicate keys are present
try(updateCyGroups(cyGroups1, cyGroups2,
                   stopOnDuplicates=TRUE))
## =>ERROR: 
## Elements of "id" (in updateCyGroups) must not contain duplicates!

## For RCX
## prepare RCX object:
nodes = createNodes(name = c("a","b","c","d","e","f"))
edges = createEdges(source=c(1,2,0,0,0,2), 
                    target=c(2,3,1,2,5,4))
rcx = createRCX(nodes, edges)

## add the group
rcx = updateCyGroups(rcx, cyGroups1)

## add an additional group
rcx = updateCyGroups(rcx, cyGroups2)

## create a group with a not existing node...
cyGroups3 = createCyGroups(
  name = "Group Three",
  nodes = list(9)                    
)

## ...and try to add them
try(updateCyGroups(rcx, cyGroups3))
## =>ERROR: 
## Provided IDs of "additionalGroups$nodes" (in updateCyGroups) 
## don't exist in "rcx$nodes$id"

## create a group with a not existing edge...
cyGroups4 = createCyGroups(
  name = "Group Four",
  nodes = list(c(1,2)),
  internalEdges = list(c(9))
)

## ...and try to add them
try(updateCyGroups(rcx, cyGroups4))
## =>ERROR: 
## Provided IDs of "additionalGroups$internalEdges" (in updateCyGroups) 
## don't exist in "rcx$edges$id"
