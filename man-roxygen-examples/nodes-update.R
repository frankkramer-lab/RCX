## create some nodes
nodes1 = createNodes(name = c("EGFR","AKT1","WNT"))
nodes2 = createNodes(name=c("CDK1", "CDK2", "CDK3"),
                     represents=c("HGNC:CDK1", 
                                  "Uniprot:P24941", 
                                  "Ensembl:ENSG00000250506"))

## simply add the nodes and keep old ids
nodes3 = updateNodes(nodes1, nodes2)

## add the nodes
nodes4 = updateNodes(nodes1, nodes2, keepOldIds=FALSE)

## force an error because of duplicated ids
try(updateNodes(nodes1, nodes2, stopOnDuplicates=TRUE))
## =>Error:
## Elements of "id" (in updateNodes) must not contain duplicates!

## create an RCX object with nodes
rcx = createRCX(nodes1)

## add additional nodes
rcx = updateNodes(rcx, nodes2, keepOldIds=FALSE)

## force an error becauses of duplicated ids
try(updateNodes(rcx, nodes2, stopOnDuplicates=TRUE))
## =>Error:
## Elements of "id" (in updateNodes) must not contain duplicates!
