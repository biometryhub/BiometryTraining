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


# save_png <- function(code, width = 400, height = 400) {
#     path <- tempfile(fileext = ".png")
#     png(path, width = width, height = height)
#     on.exit(dev.off())
#     # code
#
#     path
# }

test_that("mct produces output", {
    dat.aov <- aov(Petal.Width ~ Species, data = iris)
    output <- mct.out(dat.aov, classify = "Species")
    expect_identical(output$predicted_values$predicted.value, c(0.25, 1.33, 2.03))
    # expect_snapshot_file(path = save_png(plot(output$predicted_plot)), "plot.png")
    skip_if(interactive())
    vdiffr::expect_doppelganger("mct output", output$predicted_plot)
})



# expect_snapshot_plot <- function(name, code) {
#     # Other packages might affect results
#     # skip_if_not_installed("ggplot2", "2.0.0")
#     # Or maybe the output is different on some operation systems
#     # skip_on_os("windows")
#     # You'll need to carefully think about and experiment with these skips
#
#     path <- save_png(code)
#     expect_snapshot_file(path, paste0(name, ".png"))
# }




# test_that("snapshot", {
#     expect_snapshot_plot("plot", plot(output$predicted_plot))
# })


# iris2 <- iris
# iris2$Species <- as.factor(gsub(pattern = "to", replacement = "-", x = iris2$Species))
# dat.aov <- aov(Petal.Width ~ Species, data = iris2)
# mct.out(dat.aov, classify = "Species")


test_that("mct handles aliased results properly", {
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





