test_that("Residual plots work for aov", {
  dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

  expect_error(resplt(1:10), "mod.obj must be an aov, lm, lmerMod, lmerModLmerTest, asreml or mmer object")
  p1 <- resplt(dat.aov)
  p2 <- resplt(dat.aov, shapiro = FALSE)

  vdiffr::expect_doppelganger(title = "Resplot for aov", p1)
  vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})

test_that("Residual plots work for asreml", {
  skip_if_not_installed("asreml")

  dat.asr <- quiet(asreml::asreml(Petal.Length ~ Petal.Width, data = iris, trace = F))
  p1_single <- resplot(dat.asr, shapiro = F)

  load("../complexmodel.Rdata")
  p1_multi <- suppressWarnings(resplt(final.m.asr))#[[1]]
  # p2_multi <- resplot(final.m.asr)[[2]]
  # p3_multi <- resplot(final.m.asr)[[3]]
  # ggsave("test-plot.png", p1)

  # expect_plots_identical("test-plot.png", "../figs/resplt/resplt-aov.png")
  vdiffr::expect_doppelganger(title = "Resplot for asreml single", p1_single)
  vdiffr::expect_doppelganger(title = "Resplot for asreml pt 1", p1_multi[[1]])
  vdiffr::expect_doppelganger(title = "Resplot for asreml pt 2", p1_multi[[2]])
  vdiffr::expect_doppelganger(title = "Resplot for asreml pt 3", p1_multi[[3]])


  # vdiffr::manage_cases()
})


test_that("Residual plots work for lme4", {
  skip_if_not_installed("lme4")
  dat.lme4 <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)

  # expect_error(resplt(1:10), "mod.obj must be an aov, lm, lmerMod, lmerModLmerTest, asreml or mmer object")
  p1 <- resplot(dat.lme4)
  # p2 <- resplt(dat.lme4, shapiro = FALSE)

  vdiffr::expect_doppelganger(title = "Resplot for lme4", p1)
  # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})


test_that("Residual plots work for nlme", {
  skip_if_not_installed("nlme")
  dat.nlme <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
                    data = Loblolly,
                    fixed = Asym + R0 + lrc ~ 1,
                    random = Asym ~ 1,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

  p1 <- resplt(dat.nlme)

  vdiffr::expect_doppelganger(title = "Resplot for nlme", dat.nlme)
  # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})

test_that("Residual plots work for sommer", {
  skip_if_not_installed("sommer")
  dat.som <- sommer::mmer(yield ~ Nitrogen + Variety + Nitrogen:Variety,
                    random = ~ Blocks + Blocks:Wplots,
                    rcov = ~ units,
                    data = asreml::oats)

  p1 <- resplt(dat.som)
  # p2 <- resplt(dat.aov, shapiro = FALSE)

  vdiffr::expect_doppelganger(title = "Resplot for sommer", p1)
  # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})
