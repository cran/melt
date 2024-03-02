## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", dpi = 300)

## ----echo=FALSE---------------------------------------------------------------
library(melt, warn.conflicts = FALSE)

## ----eval=TRUE----------------------------------------------------------------
data("faithful")
str(faithful)
summary(faithful)

## -----------------------------------------------------------------------------
fit <- el_mean(faithful, par = c(3.5, 70))
class(fit)
showClass("EL")

## -----------------------------------------------------------------------------
fit

## -----------------------------------------------------------------------------
w <- faithful$waiting
(wfit <- el_mean(faithful, par = c(3.5, 70), weights = w))

