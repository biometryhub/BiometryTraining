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

test_that("transformations are handled", {
    dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
    output.log <- mct.out(dat.aov.log, classify = "Species", trans = "log", offset = 0)
    dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
    output.sqrt <- mct.out(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0)
    dat.aov.logit <- aov(car::logit(1/Petal.Width) ~ Species, data = iris)
    output.logit <- mct.out(dat.aov.logit, classify = "Species", trans = "logit", offset = 0)
    dat.aov.inverse <- aov((1/Petal.Width) ~ Species, data = iris)
    output.inverse <- mct.out(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0)

    expect_identical(output.log$predicted_values$predicted.value, c(-1.48, 0.27, 0.70))
    expect_identical(output.sqrt$predicted_values$predicted.value, c(0.49, 1.15, 1.42))
    expect_identical(output.logit$predicted_values$predicted.value, c(-5.30, -4.87, -3.07))
    expect_identical(output.inverse$predicted_values$predicted.value, c(0.50, 0.77, 4.79))

    skip_if(interactive())
    vdiffr::expect_doppelganger("mct log output", output.log$predicted_plot)
    vdiffr::expect_doppelganger("mct sqrt output", output.sqrt$predicted_plot)
    vdiffr::expect_doppelganger("mct logit output", output.logit$predicted_plot)
    vdiffr::expect_doppelganger("mct inverse output", output.inverse$predicted_plot)
})

test_that("transformations with no offset produces an error", {
    dat.aov <- aov(log(Petal.Width) ~ Species, data = iris)
    expect_error(mct.out(dat.aov, classify = "Species", trans = "log"))
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
    expect_error(suppressWarnings(mct.out(model.asr, pred = "Nitrogen")),
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

