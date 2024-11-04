################################################################################
## Simulations confidence intervals                                           ##
################################################################################

# sample size
sample_size <- 17

# variance of the errors
sigma_2 <- 3

# population parameter values (beta_0, beta_1) 
beta <- c(1, 2)

# covariate
x <- cbind(rep(x = 1, times = sample_size),
           round(x = 10 * runif(n = sample_size, min = 0, max = 1), digits = 0))

# number of simulated experiments
simulations <- 100

b0_ci_95 <- matrix(data = NA, nrow = simulations, ncol = 2)
b1_ci_95 <- matrix(data = NA, nrow = simulations, ncol = 2)

for (i in 1:simulations){
  y <- rnorm(n = sample_size, mean = x %*% beta, sd = sqrt(sigma_2))
  
  inverse_xtx <- solve(t(x) %*% x)
  
  hat_beta <- inverse_xtx %*% (t(x) %*% y)
  
  ss_error <- sum((y - x %*% hat_beta)^2) / (sample_size - 2)
  
  variance_matrix <- ss_error * inverse_xtx
  
  b0_ci_95[i, ] <- hat_beta[1] + c(-1, 1) *
    qt(p = 0.975, df = sample_size - 2) *
    sqrt(variance_matrix[1, 1])
  
  b1_ci_95[i, ] <- hat_beta[2] + c(-1, 1) *
    qt(p = 0.975, df = sample_size - 2) *
    sqrt(variance_matrix[2, 2])
}

# plot of the confidence intervals calculated for b0
plot(x = 0, y = 0, ann = FALSE, axes = FALSE, las = 1, 
     xlim = c(1, simulations), ylim = c(min(b0_ci_95), max(b0_ci_95)),
     type = "n")

abline(h = beta[1], lwd = 2, lty = 3)

segments(x0 = seq(from = 1, to = simulations),
         x1 = seq(from = 1, to = simulations),
         y0 = b0_ci_95[, 1], y1 = b0_ci_95[, 2], 
         col = ifelse(test = beta[1] >= b0_ci_95[, 1] & 
                             beta[1] <= b0_ci_95[, 2],
                      yes = "#005f86", no = "#f8971f"),
         lwd = 1.8)
box(bty = "l")
axis(1)
axis(2, las = 1)
mtext(text = expression(beta[0]), side = 2, line = 2.5, cex = 1.5, las = 2)
mtext(text = "Simulation number", side = 1, line = 2.5, cex = 1.5)

# plot of the confidence intervals calculated for b1
plot(x = 0, y = 0, ann = FALSE, axes = FALSE, las = 1, 
     xlim = c(1, simulations), ylim = c(min(b1_ci_95), max(b1_ci_95)),
     type = "n")

abline(h = beta[2], lwd = 2, lty = 3)

segments(x0 = seq(from = 1, to = simulations),
         x1 = seq(from = 1, to = simulations),
         y0 = b1_ci_95[, 1], y1 = b1_ci_95[, 2], 
         col = ifelse(test = beta[2] >= b1_ci_95[, 1] & 
                             beta[2] <= b1_ci_95[, 2],
                      yes = "#005f86", no = "#f8971f"),
         lwd = 1.8)

box(bty = "l")
axis(1)
axis(2, las = 1)
mtext(text = expression(beta[1]), side = 2, line = 2.5, cex = 1.5, las = 2)
mtext(text = "Simulation number", side = 1, line = 2.5, cex = 1.5)

