## create some simple edges
edges1 = createEdges(source=1, target=2)

## create edges with more information
edges2 = createEdges(id=c(3,2,4),
                    source=c(0,0,1), 
                    target=c(1,2,2),
                    interaction=c("activates","inhibits", NA))
