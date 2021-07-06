## Read from a CX file
## reading one of the provided examples of the package
cx = file.path(system.file("data", package="RCX"), "Wnt signaling network.cx")
rcx = read.rcx(cx)
