################################################################################
## Authors:
##   Florian Auer [Florian.Auer@informatik.uni-augsburg.de]
##
## Description:
##    Tests for RCX data model:
##    Check from and to JSON 
##    Check create and update
##    Check MetaData update
##
################################################################################

library(RCX)


testNetworks = c(
 "Direct-p53-effectors-67c3b75d-6191-11e5-8ac5-06603eb7f303.cx",
 "Imatinib-Inhibition-of-BCR-ABL-66a902f5-2022-11e9-bb6a-0ac135e8bacf.cx",
 "WP3633-d1663a2f-56bc-11eb-9e72-0ac135e8bacf.cx",
 "RCX_Data_Structure.cx"
 )

for(net in testNetworks){
 context(paste0("Network: ",net))

  test_that(paste0("Test ",net), {
   cxFile <- system.file(
    "extdata", 
    net, 
    package = "RCX"
   )
   
   rcx = readCX(cxFile)
   expect_is(rcx, 'RCX')
   expect_named(rcx)
   expect_is(rcx$nodes, 'NodesAspect')
   expect_is(rcx$edges, 'EdgesAspect')
   expect_is(rcx$metaData, 'MetaDataAspect')
   
   cxFile = tempfile(fileext = ".cx")
   writeCX(rcx, cxFile)
   
   rcx2 = readCX(cxFile)
   expect_is(rcx2, 'RCX')
   expect_named(rcx2)
   expect_is(rcx2$nodes, 'NodesAspect')
   expect_is(rcx2$edges, 'EdgesAspect')
   expect_is(rcx2$metaData, 'MetaDataAspect')
   
   expect_equal(
     sort(names(rcx)), 
     sort(names(rcx2))
   )
   
   expect_equal(
     rcx$metaData,
     rcx2$metaData
   )
   
   expect_equal(
    rcx$nodes,
    rcx2$nodes
   )
   
   expect_equal(
    rcx$edges,
    rcx2$edges
   )
   
   expect_equal(
    rcx$cyVisualProperties,
    rcx2$cyVisualProperties
   )
   
   # expect_equal(
   #  lapply(rcx, colnames),
   #  lapply(rcx2, colnames)
   # )
   
  })
 
}

