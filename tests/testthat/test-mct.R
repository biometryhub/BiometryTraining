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




test_that("mct produces output", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    mct.out(dat.aov, classify = "Species")
    expect_identical(output$predicted_values$predicted.value, c(0.25, 1.33, 2.03))

    vdiffr::expect_doppelganger("mct output", output$predicted_plot, )
})


# skip if not local/asreml installed
# model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen:Variety", sed = TRUE)
#
# mct.out(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen:Variety", label_height = 0.1)
#
# model.asr <- asreml(log(yield) ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen", sed = TRUE)
#
# mct.out(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen", trans = "log", offset = 0, label_height = 0.1)




