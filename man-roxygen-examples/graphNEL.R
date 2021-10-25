## graphNEL can handle multi-edges, but only if the graph is directed and the 
## source and target start and end not between the same nodes.
## Unfortunaltelly this is the case in our sample network.
## A quick fix is simply switching the direction of source and target 
## for the multi-edges:
dubEdges = duplicated(rcx$edges[c("source","target")])

s = rcx$edges$source
rcx$edges$source[dubEdges] = rcx$edges$target[dubEdges]
rcx$edges$target[dubEdges] = s[dubEdges]

## convert the network to graphNEL
gNel = toGraphNEL(rcx, directed = TRUE)

## convert it back
rcxFromGraphNel = fromGraphNEL(gNel)
