# Test model object is correct type
# Test pred.obj is provided with asreml objects?
# Test ordering works as expected

# model.obj
# offset
# decimals

logit <- function (p, percents = range.p[2] > 1, adjust)
{
    range.p <- range(p, na.rm = TRUE)
    if (percents) {
        if (range.p[1] < 0 || range.p[1] > 100)
            stop("p must be in the range 0 to 100")
        p <- p/100
        range.p <- range.p/100
    }
    else if (range.p[1] < 0 || range.p[1] > 1)
        stop("p must be in the range 0 to 1")
    a <- if (missing(adjust)) {
        if (isTRUE(all.equal(range.p[1], 0)) || isTRUE(all.equal(range.p[2],
                                                                 1)))
            0.025
        else 0
    }
    else adjust
    if (missing(adjust) && a != 0)
        warning(paste("proportions remapped to (", a, ", ",
                      1 - a, ")", sep = ""))
    a <- 1 - 2 * a
    log((0.5 + a * (p - 0.5))/(1 - (0.5 + a * (p - 0.5))))
}

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
    output.log2 <- mct.out(dat.aov.log, classify = "Species", trans = "log", offset = 0, int.type = "1se")
    output.log3 <- mct.out(dat.aov.log, classify = "Species", trans = "log", offset = 0, int.type = "2se")
    dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
    output.sqrt <- mct.out(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0)
    output.sqrt2 <- mct.out(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0, int.type = "1se")
    output.sqrt3 <- mct.out(dat.aov.sqrt, classify = "Species", trans = "sqrt", offset = 0, int.type = "2se")
    dat.aov.logit <- aov(logit(1/Petal.Width) ~ Species, data = iris)
    output.logit <- mct.out(dat.aov.logit, classify = "Species", trans = "logit", offset = 0)
    output.logit2 <- mct.out(dat.aov.logit, classify = "Species", trans = "logit", offset = 0, int.type = "1se")
    output.logit3 <- mct.out(dat.aov.logit, classify = "Species", trans = "logit", offset = 0, int.type = "2se")
    dat.aov.inverse <- aov((1/Petal.Width) ~ Species, data = iris)
    output.inverse <- mct.out(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0)
    output.inverse2 <- mct.out(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0, int.type = "1se")
    output.inverse3 <- mct.out(dat.aov.inverse, classify = "Species", trans = "inverse", offset = 0, int.type = "2se")

    expect_identical(output.log$predicted_values$predicted.value, c(-1.48, 0.27, 0.70))
    expect_identical(output.log2$predicted_values$predicted.value, c(-1.48, 0.27, 0.70))
    expect_identical(output.log3$predicted_values$predicted.value, c(-1.48, 0.27, 0.70))
    expect_identical(output.sqrt$predicted_values$predicted.value, c(0.49, 1.15, 1.42))
    expect_identical(output.sqrt2$predicted_values$predicted.value, c(0.49, 1.15, 1.42))
    expect_identical(output.sqrt3$predicted_values$predicted.value, c(0.49, 1.15, 1.42))
    expect_identical(output.logit$predicted_values$predicted.value, c(-3.07, -4.87, -5.30))
    expect_identical(output.logit2$predicted_values$predicted.value, c(-3.07, -4.87, -5.30))
    expect_identical(output.logit3$predicted_values$predicted.value, c(-3.07, -4.87, -5.30))
    expect_identical(output.inverse$predicted_values$predicted.value, c(4.79, 0.77, 0.50))
    expect_identical(output.inverse2$predicted_values$predicted.value, c(4.79, 0.77, 0.50))
    expect_identical(output.inverse3$predicted_values$predicted.value, c(4.79, 0.77, 0.50))

    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct log output", output.log$predicted_plot)
    vdiffr::expect_doppelganger("mct sqrt output", output.sqrt$predicted_plot)
    vdiffr::expect_doppelganger("mct logit output", output.logit$predicted_plot)
    vdiffr::expect_doppelganger("mct inverse output", output.inverse$predicted_plot)
})

test_that("transformations with no offset produces an error", {
    dat.aov <- aov(log(Petal.Width) ~ Species, data = iris)
    expect_error(mct.out(dat.aov, classify = "Species", trans = "log"))
})

test_that("ordering output works", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output1 <- mct.out(dat.aov, classify = "Species", order = "asc")
    output2 <- mct.out(dat.aov, classify = "Species", order = "desc")
    expect_identical(output1$predicted_values$predicted.value, c(0.25, 1.33, 2.03))
    expect_identical(output2$predicted_values$predicted.value, c(2.03, 1.33, 0.25))

    vdiffr::expect_doppelganger("mct ascending order", output1$predicted_plot)
    vdiffr::expect_doppelganger("mct descending output", output2$predicted_plot)
})

test_that("different interval types work", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output1 <- mct.out(dat.aov, classify = "Species", int.type = "1se")
    output2 <- mct.out(dat.aov, classify = "Species", int.type = "2se")
    expect_identical(output1$predicted_values$low, c(0.22, 1.30, 2.00))
    expect_identical(output1$predicted_values$up, c(0.27, 1.35, 2.05))
    expect_identical(output2$predicted_values$low, c(0.19, 1.27, 1.97))
    expect_identical(output2$predicted_values$up, c(0.30, 1.38, 2.08))

    vdiffr::expect_doppelganger("mct output 1se", output1$predicted_plot)
    vdiffr::expect_doppelganger("mct output 2se", output2$predicted_plot)
})

test_that("save produces output", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    withr::local_file("pred_vals.csv")
    output <- mct.out(dat.aov, classify = "Species", save = TRUE, savename = "pred_vals")
    output$predicted_values[c("Species", "groups")] <- lapply(output$predicted_values[c("Species", "groups")], as.character)
    expect_identical(output$predicted_values, read.csv("pred_vals.csv"))
})

test_that("Interaction terms work", {
    skip_if_not_installed("asreml")
    quiet(library(asreml))
    load("../asreml_oats.Rdata")
    output <- mct.out(model.asr, pred.asr, classify = "Nitrogen:Variety")
    expect_identical(output$predicted_values$predicted.value,
                     c(80.00, 86.67, 71.50, 98.50, 108.50, 89.67, 114.67, 117.17, 110.83, 124.83, 126.83, 118.50))

    # skip_if(interactive())
    vdiffr::expect_doppelganger("Interactions work", output$predicted_plot)
})

test_that("invalid order input produces an error", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    expect_error(mct.out(dat.aov, classify = "Species", order = "xyz"),
                 "order must be one of 'ascending', 'increasing', 'descending', 'decreasing' or 'default'")
    expect_error(
        expect_warning(mct.out(dat.aov, classify = "Species", order = 1:2),
                       "argument 'pattern' has length > 1 and only the first element will be used"),
        "order must be one of 'ascending', 'increasing', 'descending', 'decreasing' or 'default'")
})

test_that("dashes are handled", {
    iris2 <- iris
    # Replace 'gin' in setosa with '-'
    iris2$Species <- as.factor(gsub("to", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)
    output2 <- suppressWarnings(mct.out(dat.aov2, classify = "Species"))
    expect_warning(mct.out(dat.aov2, classify = "Species"),
                   "The treatment level se-sa contained '-', which has been replaced in the final output with '_'")
    expect_identical(output2$predicted_values$predicted.value, c(0.25, 1.33, 2.03))

    # Replace 'gin' in virginica with '-' as well
    iris2$Species <- as.factor(gsub("gin", "-", iris2$Species))
    dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)

    expect_warning(mct.out(dat.aov2, classify = "Species"),
                   "The treatment levels se-sa, vir-ica contained '-', which has been replaced in the final output with '_'")
    expect_identical(output2$predicted_values$predicted.value, c(0.25, 1.33, 2.03))

    # skip_if(interactive())
    vdiffr::expect_doppelganger("mct dashes output", output2$predicted_plot)
})

test_that("mct removes aliased treatments in aov", {
    iris1 <- iris
    iris1$Petal.Length[1:50] <- NA
    dat.aov1 <- aov(Petal.Length ~ Species, data = iris1)
    output1 <- mct.out(dat.aov1, classify = "Species")
    expect_identical(output1$predicted_values$predicted.value, c(4.26, 5.55))
    # skip_if(interactive())
    vdiffr::expect_doppelganger("aov aliased output", output1$predicted_plot)
})


test_that("mct handles aliased results in asreml with a warning", {
    skip_if_not_installed("asreml")
    quiet(library(asreml))
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
    quiet(library(asreml))
    load("../asreml_oats.Rdata")
    expect_error(suppressWarnings(mct.out(model.asr, classify = "Nitrogen")),
                 "You must provide a prediction object in pred.obj")
})

test_that("Forgetting sed = T in pred.obj object causes error", {
    skip_if_not_installed("asreml")
    quiet(library(asreml))
    dat.asr <- quiet(asreml(Petal.Width ~ Species, data = iris, trace = F))
    pred.out <- predict.asreml(dat.asr, classify = "Species")
    expect_error(mct.out(dat.asr, pred.out, classify = "Species"),
                 "Prediction object \\(pred.obj\\) must be created with argument sed = TRUE\\.")
})

test_that("lme4 model works", {
    skip_if_not_installed("lme4")
    quiet(library(lme4))
    load("../oats_data.Rdata")
    dat.lmer <- lmer(yield ~ nitro*gen + (1|block), data = dat)
    output <- mct.out(dat.lmer, classify = "nitro")
    expect_identical(output$predicted_values$predicted.value, c(79.39, 98.89, 114.22, 123.39))
    # skip_if(interactive())
    vdiffr::expect_doppelganger("lme4 output", output$predicted_plot)
})

# test_that("sommer model works", {
#     skip_if_not_installed("sommer")
#     quiet(library(sommer))
#     data("DT_yatesoats")
#     dat.sommer <- mmer(Y ~ N*V, random = ~B + B/MP, data = DT_yatesoats, verbose = F)
#     output <- mct.out(dat.sommer, classify = "N")
#     expect_identical(output$predicted_values$predicted.value, c(0.25, 1.33, 2.03))
#     skip_if(interactive())
#     vdiffr::expect_doppelganger("mct output", output$predicted_plot)
# })

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

