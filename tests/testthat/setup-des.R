# Randomised Complete Block Design
library(agricolae)
trt <- LETTERS[1:11]
rep <- 4
outdesign <- design.rcbd(trt = trt, r = rep, seed = 42)
outdesign_crd <- design.crd(trt = trt, r = rep, seed = 42)
outdesign_crd_2 <- design.crd(trt = trt, r = rep)
outdesign_fac <- design.ab(c(3, 2), r = rep, design = "rcbd")

expect_file <- function(fn, args, file, missing = F) {
  x <- do.call(fn, args)
  if (!missing) {
    expect_true(all(file.exists(file)))
  }
  else {
    expect_false(all(file.exists(file)))
  }
}
