## a minimal example
tableColumn = createCyTableColumn(
  appliesTo="nodes",
  name="weight"
)

## defining several properties at once
tableColumn = createCyTableColumn(
  appliesTo=c("nodes","edges"),
  name=c("weight","weight")
)

## with all parameters
tableColumn = createCyTableColumn(
  appliesTo=c("nodes","edges","networks"),
  name=c("weight","weight","collapsed"),
  dataType=c("numeric","numeric","logical"),
  isList=c(FALSE,FALSE,TRUE),
  subnetworkId=c(NA,NA,1)
)
