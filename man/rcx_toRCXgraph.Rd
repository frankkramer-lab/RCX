% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndex_RCXgraph.r
\name{rcx_toRCXgraph}
\alias{rcx_toRCXgraph}
\title{Create RCXgraph object from RCX object}
\usage{
rcx_toRCXgraph(rcx, verbose = FALSE)
}
\arguments{
\item{rcx}{RCX object}

\item{verbose}{logical; whether to print out extended feedback}
}
\value{
returns object of class RCXgraph if successfull, NULL otherwise
}
\description{
Create RCXgraph object from RCX object
}
\note{
Wrapper function for \code{\link{rcxgraph_fromRCX}}
}
\examples{
## Create an RCX object
rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
## Convert to RCXgraph
RCXgraph = rcx_toRCXgraph(rcx)
}
