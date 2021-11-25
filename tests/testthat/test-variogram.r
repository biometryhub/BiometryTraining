test_that("vario produces a variogram", {
    load("../asreml_oats.Rdata")
    v1 <- vario(model.asr)
    vdiffr::expect_doppelganger(title = "Variogram produced", v1)
})

test_that("vario produces an error for other models and data types", {
    model.lm <- lm(Petal.Length~Petal.Width, data = iris)
    expect_error(vario(model.lm), "model.obj must be an asreml model object")
    expect_error(vario(1:3), "model.obj must be an asreml model object")
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

