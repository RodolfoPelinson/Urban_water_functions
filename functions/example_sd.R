example_sd <- function(fit,x,
                       cons_par = c(intercept = 3),
                       linear_pos_par = c(intercept = 3, slope = 0.05),
                       linear_neg_par = c(intercept = 3, slope = -0.05),
                       quadratic_par = c(intercept = 3, slope = 0.05, slope2 = -0.0005),
                       
                       
                       
                       ...){
  
  y <- resid(fit)
  
  max_y = max(y)*5
  min_y = min(y)*5
  
  par(mfrow = c(2,2), mar = c(2,2,4,.1))
  
  new_x <- seq(0,100,0.1)
  
  ########################################
  
  coefs_sd <- cons_par
  
  predicted_sd <- rep(coefs_sd[1], length(new_x))
  
  predicted_sd_upper <- 0 + predicted_sd
  predicted_sd_lower <- 0 - predicted_sd
  
  plot(y ~ x, main = "Constant", ylim = c(min_y, max_y))
  
  polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
  points(y ~ x, pch = 16)
  box()
  ##########################################
  
  
  coefs_sd <- linear_pos_par
  
  predicted_sd <-  exp(coefs_sd[1] +  coefs_sd[2]*new_x)
  
  predicted_sd_upper <- 0 + predicted_sd
  predicted_sd_lower <- 0 - predicted_sd
  
  plot(y ~ x, main = "Linear_Positive", ylim = c(min_y, max_y))
  
  polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
  points(y ~ x, pch = 16)
  box()
  ###########################################
  
  
  coefs_sd <- linear_neg_par
  
  predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x)
  
  predicted_sd_upper <- 0 + predicted_sd
  predicted_sd_lower <- 0 - predicted_sd
  
  plot(y ~ x, main = "Linear_Negative", ylim = c(min_y, max_y))
  
  polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
  points(y ~ x, pch = 16)
  box()
  
  ###########################################
  
  
  
  coefs_sd <- quadratic_par
  
  predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)
  
  predicted_sd_upper <- 0 + predicted_sd
  predicted_sd_lower <- 0 - predicted_sd
  
  plot(y ~ x, main = "Quadratic", ylim = c(min_y, max_y))
  
  polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
  points(y ~ x, pch = 16)
  box()
  
  
  
  }

