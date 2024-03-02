## ----preliminaries, cache=FALSE, echo=FALSE, include=FALSE, message=FALSE-----
pkgVersion <- packageVersion("melt")
knitr::opts_chunk$set(message = FALSE)

## ----data---------------------------------------------------------------------
library("melt")
library("MASS")
library("dplyr")
library("ggplot2")
data("synth.tr", package = "MASS")
data <- dplyr::select(synth.tr, c(xs, ys))

## ----hull-plot, echo=FALSE, fig.width=7, fig.height=5-------------------------
ggplot(data, aes(xs, ys)) +
  geom_point() +
  geom_polygon(
    data = slice(data, chull(xs, ys)), alpha = 0.3, colour = "black"
  ) +
  xlim(-1.5, 1.1) +
  ylim(-0.4, 1.3) +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, linewidth = 1),
    panel.grid = element_blank()
  )

## ----fit-code-----------------------------------------------------------------
fit_mean <- el_mean(data, par = c(0, 0.5))

## ----fit-print----------------------------------------------------------------
fit_mean

## ----fit2---------------------------------------------------------------------
ctrl <- el_control(maxit_l = 50, th = 10000)
fit2_mean <- el_mean(data, par = c(1, 0.5), control = ctrl)
logL(fit2_mean)
logLR(fit2_mean)
getOptim(fit2_mean)

## ----surface-plot, echo=FALSE, fig.width=10, fig.height=7.5-------------------
xs <- seq(-1.5, 1.1, length.out = 60)
ys <- seq(-0.4, 1.3, length.out = 40)
ctrl <- el_control(th = 400)
z <- matrix(NA_real_, nrow = length(xs), ncol = length(ys))
for (i in seq_len(length(xs))) {
  for (j in seq_len(length(ys))) {
    z[i, j] <- logLR(el_mean(data, par = c(xs[i], ys[j]), control = ctrl))
  }
}
par(mar = c(1, 0, 0, 0))
persp(xs, ys, z,
  xlab = "xs", ylab = "ys", zlab = "logLR", theta = 315, phi = 25, d = 5,
  ticktype = "detailed", cex.axis = 1.2, cex.lab = 1.2, lwd = 2
)

## ----fit-eval-----------------------------------------------------------------
mu <- 0
sigma <- 1
set.seed(123526)
x <- rnorm(100)
g <- matrix(c(x - mu, (x - mu)^2 - sigma^2), ncol = 2)
fit_eval <- el_eval(g)
fit_eval$pval

## ----el-lm-fit----------------------------------------------------------------
data("UScrime", package = "MASS")
ctrl <- el_control(maxit = 1000, nthreads = 2)
(fit_lm <- el_lm(y ~ Pop + Ineq, data = UScrime, control = ctrl))

## ----el-lm-summary------------------------------------------------------------
summary(fit_lm)

## ----el-lm-confint------------------------------------------------------------
confint(fit_lm)

## ----confreg, eval=FALSE------------------------------------------------------
#  cr <- confreg(fit_lm, parm = c("Pop", "Ineq"), npoints = 200)
#  plot(cr, cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2, tck = -0.01)

## ----confreg-plot, echo=FALSE, fig.width=10, fig.height=7.5-------------------
cr <- confreg(fit_lm, parm = c("Pop", "Ineq"), npoints = 200)
plot(cr, cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2, tck = -0.01)
axis(1, lwd.ticks = 2, labels = FALSE, tck = -0.01)
axis(2, lwd.ticks = 2, labels = FALSE, tck = -0.01)
box(lwd = 2)

## ----eld, eval=FALSE----------------------------------------------------------
#  eld <- eld(fit_lm)
#  summary(eld)
#  plot(eld,
#    cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2, pch = 19, tck = -0.01
#  )

## ----eld2, echo=FALSE---------------------------------------------------------
eld <- eld(fit_lm)
summary(eld)

## ----cooks-distance-----------------------------------------------------------
fit2_lm <- lm(y ~ Pop + Ineq, data = UScrime)
cd <- cooks.distance(fit2_lm)
all.equal(which.max(eld), which.max(cd), check.attributes = FALSE)

## ----eld-plot, echo=FALSE, fig.width=10, fig.height=7.5-----------------------
plot(eld,
  cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 2, pch = 19, tck = -0.01
)
axis(1, lwd.ticks = 2, labels = FALSE, tck = -0.01)
axis(2, lwd.ticks = 2, labels = FALSE, tck = -0.01)
box(lwd = 2)

## ----elt----------------------------------------------------------------------
elt_mean <- elt(fit_mean, rhs = c(0, 0.5))
all.equal(getOptim(elt_mean), getOptim(fit_mean))
elt_lm <- elt(fit_lm, lhs = c("Pop", "Ineq"))
all.equal(getOptim(elt_lm), getOptim(fit_lm))

## ----critVal------------------------------------------------------------------
critVal(elt_mean)

## ----elt-f-boot---------------------------------------------------------------
ctrl <- el_control(
  maxit = 10000, tol = 1e-04, nthreads = 4, b = 100000, step = 1e-05
)
(elt_mean_f <- elt(fit_mean,
  rhs = c(0, 0.5), calibrate = "F", control = ctrl
))
(elt_mean_boot <- elt(fit_mean,
  rhs = c(0, 0.5), calibrate = "boot", control = ctrl
))

## ----glm----------------------------------------------------------------------
library("car")
data("Mroz", package = "carData")
fit_glm <- el_glm(lfp ~ .,
  family = binomial(link = "logit"), data = Mroz, control = ctrl
)
fit2_glm <- glm(lfp ~ ., family = binomial(link = "logit"), data = Mroz)

## ----glm-confint--------------------------------------------------------------
matrix(c(confint(fit_glm), confint(fit2_glm)),
  ncol = 4, dimnames = list(
    c(names(coef(fit2_glm))),
    c("EL_lower", "EL_upper", "MASS_2.5%", "MASS_97.5%")
  )
)

## ----glm-summary--------------------------------------------------------------
coef(summary(fit_glm))

## ----comparison---------------------------------------------------------------
lhs <- c(0, 0, 0, 0, 1, -1, 0, 0)
elt_glm <- elt(fit_glm, lhs = lhs)
lht_glm <- lht(fit2_glm, hypothesis.matrix = lhs, test = "Chisq")
lhs2 <- rbind(
  c(0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0)
)
rhs2 <- c(-1.5, 0)
elt2_glm <- elt(fit_glm, rhs = rhs2, lhs = lhs2)
lht2_glm <- lht(fit2_glm,
  hypothesis.matrix = lhs2, rhs = rhs2, test = "Chisq"
)

## ----comparison2--------------------------------------------------------------
matrix(
  c(
    chisq(elt_glm), pVal(elt_glm),
    lht_glm$Chisq[2], lht_glm$`Pr(>Chisq)`[2]
  ),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("EL", "Wald"), c("Chisq", "Pr(>Chisq)"))
)
matrix(
  c(
    chisq(elt2_glm), pVal(elt2_glm),
    lht2_glm$Chisq[2], lht2_glm$`Pr(>Chisq)`[2]
  ),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("EL", "Wald"), c("Chisq", "Pr(>Chisq)"))
)

## ----thiamethoxam-------------------------------------------------------------
data("thiamethoxam")
summary(thiamethoxam)

## ----ridgeline-plot, echo=FALSE, fig.width=7, fig.height=5--------------------
library("ggridges")
ggplot(thiamethoxam, aes(
  x = visit, y = trt, fill = var, linetype = var, color = var
)) +
  geom_density_ridges2(
    alpha = 0.5, scale = 0.9, bandwidth = 1.5, rel_min_height = 0.01,
    jittered_points = TRUE, point_shape = "|", point_size = 3,
    position = position_points_jitter(width = 0.05, height = 0)
  ) +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10, color = "black"),
    legend.background = element_rect(fill = alpha("white", 0)),
    legend.margin = margin(t = 0),
    legend.key = element_rect(fill = alpha("white", 1)),
  ) +
  labs(
    x = "Number of bee visits", y = "Treatment",
    colour = "", linetype = "", fill = ""
  ) +
  scale_linetype_manual(
    breaks = c("GZ", "SPM"),
    values = c("solid", "dashed")
  ) +
  scale_colour_manual(
    breaks = c("GZ", "SPM"),
    values = c("red", "blue")
  ) +
  scale_fill_manual(
    breaks = c("GZ", "SPM"),
    values = c("red", "blue")
  )

## ----quasi-poisson------------------------------------------------------------
fit3_glm <- el_glm(visit ~ trt + var + fruit + defoliation,
  family = quasipoisson(link = "log"), data = thiamethoxam,
  control = ctrl
)
print(summary(fit3_glm), width.cutoff = 50)

## ----test-all-zero------------------------------------------------------------
elt2_glm <- elt(fit3_glm, lhs = c("trtSpray", "trtFurrow", "trtSeed"))
summary(elt2_glm)

## ----comparisons-with-control-------------------------------------------------
elmt_glm <- elmt(fit3_glm, lhs = list("trtSpray", "trtFurrow", "trtSeed"))
summary(elmt_glm)

## ----multcomp-----------------------------------------------------------------
library("multcomp")
fit4_glm <- glm(visit ~ trt + var + fruit + defoliation,
  family = quasipoisson(link = "log"), data = thiamethoxam,
)
fit4_glm$call <- NULL
glht_glm <- glht(fit4_glm,
  linfct = mcp(trt = c("Spray = 0", "Furrow = 0", "Seed = 0"))
)
summary(glht_glm)

## ----sci----------------------------------------------------------------------
confint(elmt_glm)
glht_sci <- confint(glht_glm)$confint
attributes(glht_sci)[c("calpha", "conf.level")] <- NULL
glht_sci

