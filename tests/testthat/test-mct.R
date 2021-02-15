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

# skip if not local/asreml installed
# model.asr <- asreml(log(yield) ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen", sed = TRUE)
#
# mct.out(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen", trans = "log", offset = 0, decimals = 2)



# dat.aov <- aov(Petal.Width ~ Species, data = iris)
# mct.out(dat.aov, classify = "Species")


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
