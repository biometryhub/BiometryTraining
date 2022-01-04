model.asr <- readRDS(test_path("data", "model_asr.rds"))

test_that("vario_df produces a dataframe", {
    vg <- vario_df(model.asr)
    expect_equal(nrow(vg), 72)
    expect_equal(round(vg[1:6, "gamma"], 3), c(0.000, 50.171, 54.679, 91.463, 101.107, 96.750))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")
})

test_that("variogram produces a plot", {
    v1 <- variogram(model.asr)
    expect_type(v1, "list")
    expect_s3_class(v1, "ggplot")
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    vdiffr::expect_doppelganger(title = "Variogram produced", v1)
})

test_that("vario produces an error for other models and data types", {
    model.lm <- lm(Petal.Length~Petal.Width, data = iris)
    expect_error(variogram(model.lm), "model.obj must be an asreml model object")
    expect_error(variogram(1:3), "model.obj must be an asreml model object")
})


# Comp to asreml
# library(asreml)
# oats <- asreml::oats
# dat <- dat[order(dat$Row, dat$Column),]
# model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#                     random = ~ Blocks + Blocks:Wplots,
#                     residual = ~ ar1(Row):ar1(Column),
#                     data = dat)




# Direct comparison
# View(cbind(vario, asreml::varioGram(model.asr)))

# v1 <- vario(model.asr)
# v2 <- asreml::varioGram(model.asr)
# cbind(v1, v2)
#
# library(dplyr)
# new_data <- design(type = "split", 1:5, sub_treatments = 1:20, 4, 20, 20, brows = 5, bcols = 20, seed = 42, quiet = T)
# new_data <- new_data$design
#
# new_data <- new_data[order(new_data$row, new_data$col),]
# new_data$yield <- rnorm(400)
#
# new_data <- new_data %>% mutate(across(1:7, factor))
#
# model.asr <- asreml(yield ~ treatments * sub_treatments,
#                     random = ~ block + block:plots,
#                     residual = ~ ar1(row):ar1(col),
#                     data = new_data)
#
# v1 <- vario(model.asr)
# v2 <- asreml::varioGram(model.asr)

