# Test model object is correct type
# Test pred.obj is provided with asreml objects?
# Test ordering works as expected

# model.obj
# pred.obj
# classify
# sig
# int.type
# trans
# offset
# decimals
# order
# save
# savename
# pred



dat.aov <- aov(Petal.Width ~ Species, data = iris)
mct.out(dat.aov, classify = "Species")


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
