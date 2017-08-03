Introduction
============

Networks are a powerful and flexible methodology for expressing
biological knowledge for computation and communication. Albeit its
benefits, the sharing of networks, the collaboration on network
curation, keeping track of changes between different network versions,
and detecting different versions itself, still is a major problem in
network biology.

The Network Data Exchange, or NDEx, is an open-source software framework
to manipulate, store and exchange networks of various types and formats
(Pratt et al., 2015, Cell Systems 1, 302-305, October 28, 2015 ©2015
Elsevier Inc.
[ScienceDirect](http://www.sciencedirect.com/science/article/pii/S2405471215001477)).
NDEx can be used to upload, share and publicly distribute networks,
while providing an output in formats, that can be used by plenty of
other applications.

This package provides an interface to query the public NDEx server, as
well as private installations, in order to upload, download or modify
biological networks. This document aims to help the user to install and
benefit from the wide range of funtionality of this implementation. The
package also provides classes to implement the Cytoscape
Cyberinfrastructure (CX) Format and to extend the \[iGraph Package\]
(<http://igraph.org/r/>).

The package is compatible with both NDEx versions 1.3 and 2.0.

Installation
============
    
## Installation via GitHub

using [*devtools*](http://cran.r-project.org/web/packages/devtools/index.html) R package

    require(devtools)
    install_github("frankkramer-lab/ndexr")
    library(ndexr)

RCX
===

For the exchange of network data, NDEx uses the Cytoscape
Cyberinfrastructure Network Interchange Format, or just CX format (See
[*http://www.home.ndexbio.org/data-model/*](http://www.home.ndexbio.org/data-model/)).
CX is an Aspect-Oriented Network Interchange Format encoded in JSON,
which is used as basis for the R implementation of the CX format, namely
RCX.

The RCX object is currently implemented within this package as a list of
data.frames, containing meta-data and all aspects of the network. The
structure of an RCX object, as shown via `str(rcx)` could be a list like
this:

    str(rcx, max.level = 2)

    ## List of 13
    ##  $ metaData          :'data.frame':  11 obs. of  7 variables:
    ##   ..$ elementCount    : int [1:11] 1 1 395 667 328 299 3434 160 99 3 ...
    ##   ..$ lastUpdate      : num [1:11] 1.49e+12 1.49e+12 NA NA NA ...
    ##   ..$ name            : chr [1:11] "ndexStatus" "provenanceHistory" "nodes" "edges" ...
    ##   ..$ properties      :List of 11
    ##   ..$ version         : chr [1:11] "1.0" "1.0" NA NA ...
    ##   ..$ consistencyGroup: int [1:11] NA 1 NA NA NA NA NA NA NA NA ...
    ##   ..$ idCounter       : int [1:11] NA NA 1689 1689 1689 1689 NA NA NA NA ...
    ##  $ numberVerification:'data.frame':  1 obs. of  1 variable:
    ##   ..$ longNumber: num 2.81e+14
    ##  $ ndexStatus        :'data.frame':  1 obs. of  10 variables:
    ##   ..$ externalId      : chr "7aed4dd0-14e4-11e6-a1f8-06603eb7f303"
    ##   ..$ creationTime    : num 1.46e+12
    ##   ..$ modificationTime: num 1.49e+12
    ##   ..$ visibility      : chr "PUBLIC"
    ##   ..$ published       : logi FALSE
    ##   ..$ nodeCount       : int 395
    ##   ..$ edgeCount       : int 667
    ##   ..$ owner           : chr "rasmachine"
    ##   ..$ ndexServerURI   : chr "http://public.ndexbio.org"
    ##   ..$ readOnly        : logi FALSE
    ##  $ nodes             :'data.frame':  395 obs. of  2 variables:
    ##   ..$ @id: int [1:395] 0 1 5 10 11 20 27 28 32 33 ...
    ##   ..$ n  : chr [1:395] "ABCC8" "ATP" "CHI" "ADCY1" ...
    ##  $ edges             :'data.frame':  667 obs. of  4 variables:
    ##   ..$ @id: int [1:667] 2 6 12 21 29 34 38 43 53 58 ...
    ##   ..$ s  : int [1:667] 0 0 10 20 27 32 1 11 52 52 ...
    ##   ..$ t  : int [1:667] 1 5 11 11 28 33 10 42 10 57 ...
    ##   ..$ i  : chr [1:667] "Activation" "Activation" "Activation" "Activation" ...
    ##  $ supports          :'data.frame':  328 obs. of  4 variables:
    ##   ..$ text      : chr [1:328] "This validation of the model gives us confidence that robust predictions can be made for the role of gap junction coupling upon"| __truncated__ "Although treatment of CHI is primarily based on the use of diazoxide, CHI caused by recessive inactivating mutations in the ABC"| __truncated__ "Therefore, we report this case of CHI caused by a novel mutation of ABCC8 in a half-Korean newborn infant with diazoxide-unresp"| __truncated__ "Intracellular cAMP is synthesized by AC and can be degraded by phosphodiesterases (PDEs)." ...
    ##   ..$ citation  : logi [1:328] NA NA NA NA NA NA ...
    ##   ..$ @id       : int [1:328] 4 8 9 15 16 17 18 19 24 25 ...
    ##   ..$ attributes:List of 328
    ##   .. .. [list output truncated]
    ##  $ citations         :'data.frame':  299 obs. of  7 variables:
    ##   ..$ @id           : int [1:299] 3 7 13 14 22 23 30 35 39 44 ...
    ##   ..$ dc:title      : logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ dc:contributor: logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ dc:identifier : chr [1:299] "pmid:27681078" "pmid:28018462" "pmid:28261091" "pmid:27138453" ...
    ##   ..$ dc:type       : logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ dc:description: logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ attributes    :List of 299
    ##   .. .. [list output truncated]
    ##  $ edgeAttributes    :'data.frame':  3434 obs. of  3 variables:
    ##   ..$ po: int [1:3434] 2 2 2 2 2 2 6 6 6 6 ...
    ##   ..$ n : chr [1:3434] "INDRA statement" "type" "polarity" "Belief score" ...
    ##   ..$ v : chr [1:3434] "Activation(ABCC8(), ATP())" "Activation" "positive" "0.88" ...
    ##  $ edgeCitations     :'data.frame':  160 obs. of  2 variables:
    ##   ..$ po       :List of 160
    ##   .. .. [list output truncated]
    ##   ..$ citations:List of 160
    ##   .. .. [list output truncated]
    ##  $ edgeSupports      :'data.frame':  99 obs. of  2 variables:
    ##   ..$ po      :List of 99
    ##   ..$ supports:List of 99
    ##  $ networkAttributes :'data.frame':  3 obs. of  2 variables:
    ##   ..$ n: chr [1:3] "name" "description" "version"
    ##   ..$ v: chr [1:3] "The Diabetes Machine" "<div>The Diabetes Machine reads new publications on the molecular mechanisms behind drugs affecting insulin secretion in pancre"| __truncated__ "3.120"
    ##  $ nodeAttributes    :'data.frame':  1165 obs. of  3 variables:
    ##   ..$ po: int [1:1165] 0 0 0 1 1 1 1 5 5 10 ...
    ##   ..$ n : chr [1:1165] "type" "UniProt" "HGNC" "type" ...
    ##   ..$ v : chr [1:1165] "protein" "http://identifiers.org/uniprot/Q09428" "http://identifiers.org/hgnc/HGNC:59" "chemical" ...
    ##  $ status            :'data.frame':  1 obs. of  2 variables:
    ##   ..$ error  : chr ""
    ##   ..$ success: logi TRUE
    ##  - attr(*, "class")= chr [1:2] "RCX" "list"

The data.frames representing nodes and edges could look like this:

    rcx[["nodes"]][1:5,]

    ##   @id     n
    ## 1   0 ABCC8
    ## 2   1   ATP
    ## 3   5   CHI
    ## 4  10 ADCY1
    ## 5  11  CAMP

    rcx[["edges"]][1:5,]

    ##   @id  s  t          i
    ## 1   2  0  1 Activation
    ## 2   6  0  5 Activation
    ## 3  12 10 11 Activation
    ## 4  21 20 11 Activation
    ## 5  29 27 28 Activation

Usually, and RCX object is automatically created by using the functions
of this package for downloading network data from a NDEx server. But it
might be useful to convert an RCX object from/to JSON manually, for
example for down-/uploading a CX file from/to a NDEx server via the web
interface. For handling the network information within R, besides RCX
objects, one can use RCXgraph objects. A lossless conversion between the
two files can be done using the following functions:

    ## convert RCX to JSON
    json <- rcx_toJSON(rcx)

    ## ...and back
    rcx <- rcx_fromJSON(json)

    ## convert RCX to RCXgraph
    rcxgraph <- rcx_toRCXgraph(rcx)

    ## ...and back
    rcx <- rcxgraph_toRCX(rcxgraph)

It is possible to create blank RCX objects from scratch:

    newRcx <- rcx_new()

After a RCX object is downloaded from an NDEx server, it will contain
some aspects that are not present in a newly generated network, i.e.
“ndexStatus”, “provenanceHistory” and “status”. Removing those aspects
might be useful in some cases.

    asNewRcx <- rcx_asNewNetwork(rcx)

After a RCX object underwent some changes (adding/removing
nodes/edges/aspects/meta- data, etc.), it is advisable to check a RCX
object for its integrity and update its meta-data.

    rcx <- rcx_updateMetaData(rcx)

The meta-data is not only updated by the function, but also created, if
not existent in the first place. It is necessary to mention, that the
function tries to check the format and content of most of the aspects
and properties, but due to the dynamic structure of RCX and CX, it is
advised to have a look at the CX data model specification for a deeper
insight about the core structure, dependencies and limitations.
