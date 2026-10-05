library(mvtnorm)
library(foreach)
library(doParallel)
w <- detectCores()
registerDoParallel(w)

# Function for simulation ----------------------------------------
coef_sim <- function(corr.x = 0.5, beta1 = 1.5, beta2 = 1.5, n = 50, err = 4,
              nsim = 100) {
  
    coef.mat <- foreach(isim = 1:nsim, .packages = "mvtnorm", 
                        .combine = rbind) %dopar% {
                          
                          sigma <- diag(rep(1, 2))
                          sigma[sigma != 1] <- corr.x
                          
                          x <- rmvnorm(n, mean = rep(0, 2), sigma = sigma)
                          x1 <- x[, 1]
                          x2 <- x[, 2]
                          Y_star <- beta1 * x1 + beta2 * x2
                          Y <- rnorm(n) * err + Y_star
                          Y_test <- rnorm(n) * err + Y_star
                          datas <- data.frame(Y = Y, x1 = x1, x2 = x2)
                          
                          formula <- as.formula("Y~x1+x2")
                          fit.lm <- lm(data = datas, formula = formula)
                          fit.uni <- lm(data = datas, formula = Y ~ x1)
                          keepline <- c(coef(fit.lm),
                                        summary(fit.lm)$coefficients[3, 4],
                                        coef(fit.uni),
                                        se2.full = vcov(fit.lm)[3, 3],
                                        se2.red = vcov(fit.uni)[2, 2])
                          keepline
                        }
    return(coef.mat)
  }

# Function for plotting coefficients ----------------------------

coef_plot <- function(coef.mat, beta1, ...) {
  plot(coef.mat[, 3:2],pch = 1 + 18 * (coef.mat[, 4] < 0.157), 
       xlab = expression(beta[2]), ylab = expression(beta[1]), ...)
  legend("topright", pch = c(19, 1), 
         legend = do.call("expression",
                          list(bquote("P-value" < 0.157), 
                               bquote("P-value" >= 0.157))))
  lin <- lm(coef.mat[, 2] ~ coef.mat[, 3])
  abline(coef(lin), lty = 2)
  abline(beta1, 0, lty = 3)
}

# Plot of the the szenarios beta1=1.5 and beta2=1.5 or 0.3 -------
# Figure 1 in the manuscript -------------------------------------
set.seed(85641364)
szenario1 <- coef_sim()
szenario2 <- coef_sim(beta2 = 0.3)
xlim <- c(min(szenario1[,3], szenario2[,3]), 
          max(szenario1[,3], szenario2[,3]))
ylim <- c(min(szenario1[,2], szenario2[,2]), 
          max(szenario1[,2], szenario2[,2]))

coef_plot(szenario1, beta1=1.5, xlim=xlim, ylim=ylim)
coef_plot(szenario2, beta1=1.5, xlim=xlim, ylim=ylim)



