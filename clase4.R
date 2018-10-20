library("splines")
library("demography")
AUSdata <- load("AUSdata.RData")

Ext <- AUSdata$pop$male
mxt <- AUSdata$rate$male
Dxt <- round(Ext * mxt)
Ex <- rowSums(Ext[, c("2009", "2010", "2011")])
Dx <- rowSums(Dxt[, c("2009", "2010", "2011")])
mx <- Dx/Ex



age <- AUSdata$age

plot(age, Dx, type = "l", xlab = "age", ylab = "Number of deaths",
     main = "Australian Men: 2009-2011")

plot(age, Ex, type = "l", xlab = "age", ylab = "Exposed to risk",
    main = "Australian Men: 2009-2011")

plot(log(mx), type = 'l' , xlab = "age", ylab = "Central mortality rate (log scale)",
     
     main = "Australian Men: 2009-2011")

x <- 2:105
mx <- mx[as.character(x)]
Ex <- Ex[as.character(x)]
Dx <- Dx[as.character(x)]

gompertz <- nls(mx ~ exp(b0 + b1 * x), start = list(b0 = 1, b1 = 0), weights = Ex / mx)

gompertz

mx_gompertz <- fitted(gompertz)
plot(x, log(mx), pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men: Gompertz law", ylim = range(-11,0))
lines(x, log(mx_gompertz), col = 'blue')

makeham <- nls(mx ~ A + exp(b0 + b1 * x), start = list(A = 0, b0 = coef(gompertz)[1],
                                                       b1 = coef(gompertz)[2]), weights = Ex / mx)
makeham


mx_makeham <- fitted(makeham)
plot(x, log(mx), pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men: Makeham law", ylim = range(-11,0))
lines(x, log(mx_makeham), col = 'blue')


library(splines)
knots <- c(7, 12, 16, 18, 20, 32, 53, 54, 61, 66, 77, 90, 95)
cubic_basis <- ns(x, knots = knots)
matplot(x, cubic_basis, type = "l", xlab = "age (x)", ylab = "phi_i(x)",
        main = "Cubic B-spline basis")

cubSpline <- lm(mx ~ cubic_basis, weights = Ex / mx )
cubSpline

mx_cubSpline <- fitted(cubSpline)
plot(x, log(mx), pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men: Natural cubic spline", ylim = range(-11,0))
lines(x, log(mx_cubSpline), col = 'blue')


smSpline <- smooth.spline(x, mx, spar = 0.4)
smSpline

mx_smSpline <- fitted(smSpline)
plot(x, log(mx), pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men: Smoothing spline", ylim = range(-11,0))
lines(x, log(mx_smSpline), col = 'blue')

smSpline_0 <- smooth.spline(x, mx, spar = 0.4)
smSpline_0

mx_smSpline_0 <- fitted(smSpline_0)
plot(x, log(mx), pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men: Smoothing spline", ylim = range(-11,0))
lines(x, log(mx_smSpline_0), col = 'blue')

smSpline_2 <- smooth.spline(x, mx, spar = 2)
smSpline_2

mx_smSpline_2 <- fitted(smSpline_2)
plot(x, mx, pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men: Smoothing spline")
lines(x, mx_smSpline_2, col = 'blue')


plot(x, log(mx), pch = 20, xlab = "age", ylab = "Central mortality rate (log scale)",
     main = "Australian Men", ylim = range(-11,0))
lines(x, log(mx_gompertz), col = 2)
lines(x, log(mx_makeham), col = 3)
lines(x, log(mx_cubSpline), col = 4)
lines(x, log(mx_smSpline), col = 5)
legend("topleft", legend = c("Gompert", "Makeham", "Natural Cubic Spline",
                             "Smoothing Spline"), col = 2:5, lty = 1)


zx_makeham <- (Dx - Ex * mx_makeham) / sqrt(Ex * mx_makeham)

zx_gompertz <- (Dx - Ex * mx_gompertz) / sqrt(Ex * mx_gompertz)

zx_cubSpline <- (Dx - Ex * mx_cubSpline) / sqrt(Ex * mx_cubSpline)

zx_smSpline <- (Dx - Ex * mx_smSpline) / sqrt(Ex * mx_smSpline)


chi2Test <- function(O, E, npar, alpha = 0.05){
  
  chi2 <- sum((O - E)^2 / E) #Test statistic
  
  df <- length(O) - npar
  
  chi2_alpha <- qchisq(1 - alpha, df) #Critical value
  
  p.value <- 1 - pchisq(chi2, df) #p.value
  
  list(statistic = chi2, c.value = chi2_alpha, df = df, p.value = p.value)
  
}

chi2Test(Dx, Ex * mx_gompertz,2)
chi2Test(Dx, Ex * mx_makeham, 3)
chi2Test(Dx, Ex * mx_cubSpline, cubSpline$rank)
chi2Test(Dx, Ex * mx_smSpline, smSpline$df)

nages <- length(x)
signTest_gompertz <- binom.test(sum(zx_gompertz > 0), nages)
signTest_makeham <- binom.test(sum(zx_makeham > 0), nages)
signTest_cubSpline <- binom.test(sum(zx_cubSpline > 0), nages)
signTest_smSpline <- binom.test(sum(zx_smSpline > 0), nages)


