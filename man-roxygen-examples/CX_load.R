## Read from a CX file
## reading the provided example network of the package
cxFile <- system.file(
 "extdata", 
 "Imatinib-Inhibition-of-BCR-ABL-66a902f5-2022-11e9-bb6a-0ac135e8bacf.cx", 
 package = "RCX"
)

rcx = readCX(cxFile)

