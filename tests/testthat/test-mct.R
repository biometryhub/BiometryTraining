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
    output <- mct.out(dat.aov, classify = "Species")
    expect_identical(output$predicted_values$predicted.value, c(0.25, 1.33, 2.03))
    skip_if(interactive())
    vdiffr::expect_doppelganger("mct output", output$predicted_plot)
})

test_that("dashes are handled", {
    iris2 <- iris
    iris2$Species <- as.factor(gsub("to", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)
    output2 <- suppressWarnings(mct.out(dat.aov2, classify = "Species"))
    expect_warning(mct.out(dat.aov2, classify = "Species"),
                   "The treatment level se-sa contained '-', which has been replaced in the final output with '_'")
    expect_identical(output2$predicted_values$predicted.value, c(0.25, 1.33, 2.03))

    skip_if(interactive())
    vdiffr::expect_doppelganger("mct dashes output", output2$predicted_plot)
})

test_that("mct removes aliased treatments in aov", {
    iris1 <- iris
    iris1$Petal.Length[1:50] <- NA
    dat.aov1 <- aov(Petal.Length ~ Species, data = iris1)
    output1 <- mct.out(dat.aov1, classify = "Species")
    expect_identical(output1$predicted_values$predicted.value, c(4.26, 5.55))
    skip_if(interactive())
    vdiffr::expect_doppelganger("aov aliased output", output1$predicted_plot)
})


test_that("mct handles aliased results in asreml with a warning", {
    skip_if_not_installed("asreml")
    library(asreml)
    load("../asreml_oats.Rdata")
    pred.asr$pvals$predicted.value[12] <- NA
    pred.asr$sed[12, ] <- NA
    pred.asr$sed[, 12] <- NA
    expect_warning(mct.out(model.asr, pred.asr, classify = "Nitrogen:Variety"), NULL)
    pred.asr$pvals$predicted.value[11] <- NA
    pred.asr$sed[11, ] <- NA
    pred.asr$sed[, 11] <- NA
    expect_warning(mct.out(model.asr, pred.asr, classify = "Nitrogen:Variety"), NULL)
    pred2.asr$pvals$predicted.value[4] <- NA
    pred2.asr$sed[4, ] <- NA
    pred2.asr$sed[, 4] <- NA
    expect_warning(mct.out(model2.asr, pred2.asr, classify = "Nitrogen"), NULL)
    pred2.asr$pvals$predicted.value[3] <- NA
    pred2.asr$sed[3, ] <- NA
    pred2.asr$sed[, 3] <- NA
    expect_warning(mct.out(model2.asr, pred2.asr, classify = "Nitrogen"), NULL)
})

test_that("Significance values that are too high give a warning", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(mct.out(dat.aov, classify = "Species", sig = 0.95),
                   "Significance level given by sig is high. Perhaps you meant 0.05?")
})

test_that("Use of pred argument gives warning", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_warning(mct.out(dat.aov, pred = "Species"),
                   "Argument pred has been deprecated and will be removed in a future version. Please use classify instead.")
})

test_that("Missing pred.obj object causes error", {
    skip_if_not_installed("asreml")
    library(asreml)
    load("../asreml_oats.Rdata")
    expect_error(mct.out(model.asr, pred = "Nitrogen"),
                   "You must provide a prediction object in pred.obj")
})


# skip if not local/asreml installed
# model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen:Variety", sed = TRUE)

# mct.out(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen:Variety", label_height = 0.1)

# model.asr <- asreml(log(yield) ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ units,
#                     data = asreml::oats)
#
# pred.asr <- predict(model.asr, classify = "Nitrogen", sed = TRUE)
#
# mct.out(model.obj = model.asr, pred.obj = pred.asr, classify = "Nitrogen", trans = "log", offset = 0, label_height = 0.1)

