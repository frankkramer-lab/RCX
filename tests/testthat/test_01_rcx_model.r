################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##   Copied from NDExR package on 3 August 2017 by Auer
##
## Description:
##    Tests for RCX data model:
##    Check from and to JSON (rcx_fromJSON, rcx_toJSON)
##    Check new RCX objects (rcx_new)
##    Check MetaData update (rcx_updateMetaData)
##
## Usage:
##  devtools::test(filter='05_*')
################################################################################

# library(RCX)
# context('RCX data model')
# 
# 
# test_that('Create nodes aspect', {
#     nodes1 = createNodesAspect(id=c(0,1,2,3))
# 
#     nodes2a = createNodesAspect(id=c(0,1,2,3), name=c("a","b","c","d"))
#     nodes2b = createNodesAspect(name=c("a","b","c","d"))
# 
#     nodes3a = createNodesAspect(id=c(0,1,2,3), represent=c("aa","bb","cc","dd"))
#     nodes3b = createNodesAspect(represent=c("aa","bb","cc","dd"))
# 
#     nodes4a = createNodesAspect(id=c(0,1,2,3), name=c("a","b","c","d"), represent=c("aa","bb","cc","dd"))
#     nodes4b = createNodesAspect(name=c("a","b","c","d"), represent=c("aa","bb","cc","dd"))
# 
#     expect_is(nodes1, 'NodesAspect')
#     expect_is(nodes2a, 'NodesAspect')
#     expect_is(nodes2b, 'NodesAspect')
#     expect_is(nodes3a, 'NodesAspect')
#     expect_is(nodes3b, 'NodesAspect')
#     expect_is(nodes4a, 'NodesAspect')
#     expect_is(nodes4b, 'NodesAspect')
# 
#     expect_equal(nodes2a, nodes2b)
#     expect_equal(nodes3a, nodes3b)
#     expect_equal(nodes4a, nodes4b)
# 
#     expect_error(createNodesAspect())
#     expect_error(createNodesAspect(id=c(0,1,1,2)))
#     expect_error(createNodesAspect(id=c(0,1,2), name=c("a","b","c","d")))
#     expect_error(createNodesAspect(id=c(0,1,2,3), name=c("a","b","c")))
#     expect_error(createNodesAspect(id=c(0,1,2), name=c("a","b","c","d"), represent=c("aa","bb","cc","dd")))
#     expect_error(createNodesAspect(id=c(0,1,2,3), name=c("a","b","c"), represent=c("aa","bb","cc","dd")))
#     expect_error(createNodesAspect(id=c(0,1,2,3), name=c("a","b","c","d"), represent=c("aa","bb","cc")))
# })
# 
# 
# test_that('Create edges aspect', {
#     edges1 = createEdgesAspect(source=c(1,2,2,4), target=c(4,1,3,2))
#     edges2 = createEdgesAspect(id=c(0,1,2,3), source=c(1,2,2,4), target=c(4,1,3,2))
#     edges3 = createEdgesAspect(id=c(0,1,2,3), source=c(1,2,2,4), target=c(4,1,3,2), interaction=c("bind","bind", NA, "activates"))
#     edges4 = createEdgesAspect(source=c(), target=c())
# 
#     expect_is(edges1, "EdgesAspect")
#     expect_is(edges2, "EdgesAspect")
#     expect_is(edges3, "EdgesAspect")
#     expect_is(edges4, "EdgesAspect")
# 
#     expect_equal(edges1, edges2)
# 
#     expect_error(createEdgesAspect())
#     expect_error(createEdgesAspect(source=c(1,2,2,4)))
#     expect_error(createEdgesAspect(target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(source=c(1,2,2), target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(source=c(1,2,2,4), target=c(4,1,3)))
#     expect_error(createEdgesAspect(id=c(0,1,2), source=c(1,2,2,4), target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(id=c(0,1,2,2), source=c(1,2,2,4), target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(source=c(1,2,2,4), target=c(4,1,3,2), interaction=c("bind","bind", NA)))
#     expect_error(createEdgesAspect(id=c(0,1,2,3), source=c(1,2,2,4), target=c(4,1,3,2), interaction=c("bind","bind", NA)))
# })
# 
# 
# test_that('Create nodeAttributes aspect', {
#     propertyOf = c(1,3,4,4)
#     name = c("alias","alias","alias","relatedTo")
#     value1 = list("bla",NA,c("bla","blubb"),3)
#     subnetworkId
# 
#     expect_is(edges1, "EdgesAspect")
#     expect_is(edges2, "EdgesAspect")
#     expect_is(edges3, "EdgesAspect")
#     expect_is(edges4, "EdgesAspect")
# 
#     expect_equal(edges1, edges2)
# 
#     expect_error(createEdgesAspect())
#     expect_error(createEdgesAspect(source=c(1,2,2,4)))
#     expect_error(createEdgesAspect(target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(source=c(1,2,2), target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(source=c(1,2,2,4), target=c(4,1,3)))
#     expect_error(createEdgesAspect(id=c(0,1,2), source=c(1,2,2,4), target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(id=c(0,1,2,2), source=c(1,2,2,4), target=c(4,1,3,2)))
#     expect_error(createEdgesAspect(source=c(1,2,2,4), target=c(4,1,3,2), interaction=c("bind","bind", NA)))
#     expect_error(createEdgesAspect(id=c(0,1,2,3), source=c(1,2,2,4), target=c(4,1,3,2), interaction=c("bind","bind", NA)))
# })
