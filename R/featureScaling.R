# Feature scaling
normalize <- function(x, avg, sdev) {
  (x - avg) / sdev
}

un_normalize <- function(x, avg, sdev) {
  (x * sdev) + avg
}

logTransformation <- function(data, base, c) {
  data.frame(lapply(data, FUN = function(x) {
    log(x + c, base)
  }))
}

reverseLogTransformation <- function(data, base, c) {
  data.frame(lapply(list(data = data), FUN = function(x) {
    base ^ x - c
  }))
}


library(e1071)
skewScore <- function(c, x) (skewness(log(x + c)))^2

# Skew score plotting by c [log(x + c)]
plotSkewnessByLogConstant <- function(data, from, to, len) {
  c_vals <- seq(from, to, l = len)
  skew <- calculateSkewnessByLogConstant(data, c_vals)
  
  plot(c_vals, skew, type = "l", ylab = expression(skewness(c)), xlab = expression(c))
  abline(h = 0, lty = 3)
}

calculateSkewnessByLogConstant <- function(data, c_vals) {
  skew <- rep(0, length(c_vals))
  
  for (i in 1:length(c_vals)) {
      skew[i] <- skewness(log(data + c_vals[i]))
  }
  
  skew
}

plotSkewnessByLambda <- function(data, lambda_vals) {
  skew <- calculateSkewnessByLambda(data, lambda_vals)
  
  plot(lambda_vals, skew, type = "l", ylab = expression(skewness(c)), xlab = expression(c))
  abline(h = 0, lty = 3)
}

calculateSkewnessByLambda <- function(data, lambda_vals) {
  skew <- rep(0, length(lambda_vals))
  
  for (i in 1:length(lambda_vals)) {
    skew[i] <- skewness(data^ lambda_vals[i])
  }
  
  skew
}


# Boxcox transformation
invBcPower <- function(data, lambda) {
  ((data * lambda) + 1)^(1/lambda)
}

