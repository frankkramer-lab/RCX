## use a file in a temporary directory
someFile <- file.path(tempdir(),"network.cx")
## Write RCX to a file
write.rcx(rcx, someFile)
