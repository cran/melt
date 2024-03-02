## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", dpi = 300, fig.width = 7, fig.height = 5,
  fig.align = "center", out.width = "100%"
)

## ----echo=FALSE---------------------------------------------------------------
library(melt, warn.conflicts = FALSE)

## ----message=FALSE------------------------------------------------------------
library(MASS)
library(dplyr)
data("synth.tr", package = "MASS")
data <- dplyr::select(synth.tr, c(xs, ys))

## -----------------------------------------------------------------------------
fit_mean <- el_mean(data, par = c(0, 0.5))

## -----------------------------------------------------------------------------
fit_mean

## -----------------------------------------------------------------------------
ctrl <- el_control(maxit_l = 50, th = 10000)
fit2_mean <- el_mean(data, par = c(1, 0.5), control = ctrl)
logL(fit2_mean)
logLR(fit2_mean)
getOptim(fit2_mean)

## -----------------------------------------------------------------------------
mu <- 0
sigma <- 1
set.seed(123526)
x <- rnorm(100)
g <- matrix(c(x - mu, (x - mu)^2 - sigma^2), ncol = 2)
fit_eval <- el_eval(g)
fit_eval$pval

## -----------------------------------------------------------------------------
data("UScrime", package = "MASS")
ctrl <- el_control(maxit = 1000, nthreads = 2)
(fit_lm <- el_lm(y ~ Pop + Ineq, data = UScrime, control = ctrl))

## -----------------------------------------------------------------------------
summary(fit_lm)

## -----------------------------------------------------------------------------
confint(fit_lm)

## -----------------------------------------------------------------------------
elt_mean <- elt(fit_mean, rhs = c(0, 0.5))
all.equal(getOptim(elt_mean), getOptim(fit_mean))
elt_lm <- elt(fit_lm, lhs = c("Pop", "Ineq"))
all.equal(getOptim(elt_lm), getOptim(fit_lm))

## -----------------------------------------------------------------------------
ctrl <- el_control(
  maxit = 10000, tol = 1e-04, nthreads = 4, b = 100000, step = 1e-05
)
(elt_mean_f <- elt(fit_mean,
  rhs = c(0, 0.5), calibrate = "F", control = ctrl
))
(elt_mean_boot <- elt(fit_mean,
  rhs = c(0, 0.5), calibrate = "boot", control = ctrl
))

## -----------------------------------------------------------------------------
data("thiamethoxam")
fit_glm <- el_glm(visit ~ trt + var + fruit + defoliation,
  family = quasipoisson(link = "log"), data = thiamethoxam,
  control = ctrl
)
print(summary(fit_glm), width.cutoff = 50)

## -----------------------------------------------------------------------------
elt_glm <- elt(fit_glm, lhs = c("trtSpray", "trtFurrow", "trtSeed"))
summary(elt_glm)

## -----------------------------------------------------------------------------
elmt_glm <- elmt(fit_glm, lhs = list("trtSpray", "trtFurrow", "trtSeed"))
summary(elmt_glm)

## -----------------------------------------------------------------------------
confint(elmt_glm)

