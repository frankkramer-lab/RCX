#' @details # Nodes:
#' Nodes are represented by <%= .CLS$nodes %> objects. This aspect consists of at least one node, 
#' while a node at least consists of one id. This minimal 
#' 
#' @section Property overview:
#' 
#' **Note:** At least one node has to be present to be valid!
#' 
#' \tabular{rrrr}{
#' **property** \tab **options** \tab **values** \tab **description** \cr
#' id           \tab *Unique*    \tab integer    \tab *(R)CX internal node id, starts at 0* \cr
#' name         \tab *Optional*  \tab string     \tab *node name, eg. "EGFR", "node 1"* \cr
#' represents   \tab *Optional*  \tab string     \tab *represents, eg. "HGNC:AKT1"* \cr
#' }
