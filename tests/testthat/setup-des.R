# Randomised Complete Block Design
library(agricolae)
trt <- LETTERS[1:11]
rep <- 4
outdesign <- design.rcbd(trt = trt, r = rep, seed = 42)
outdesign_crd <- design.crd(trt = trt, r = rep, seed = 42)
