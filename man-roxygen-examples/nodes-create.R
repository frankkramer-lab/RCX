## a minimal example
nodes = createNodes()

## ids will be generated
nodes = createNodes(name = c("a","b","c"))

## with all parameters
nodes = createNodes(id=c(1, 2, 3), 
                    name=c("CDK1", "CDK2", "CDK3"),
                    represents=c("HGNC:CDK1", 
                                 "Uniprot:P24941", 
                                 "Ensembl:ENSG00000250506"))
