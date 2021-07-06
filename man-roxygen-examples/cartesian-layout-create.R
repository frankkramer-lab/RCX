## a minimal example
cartesianLayout = createCartesianLayout(
  node=0,
  x=5.5, 
  y=200.3
)

## defining several coordinates at once
cartesianLayout = createCartesianLayout(
  node=c(0, 1),
  x=c(5.5, 110.1), 
  y=c(200.3, 210.2)
)

## with all parameters
cartesianLayout = createCartesianLayout(
  node=c(0, 1, 0),
  x=c(5.5, 110.1, 7.2), 
  y=c(200.3, 210.2, 13.9),
  z=c(-1, 3.1, NA),
  view=c(NA, NA, 1476)
)
