
example_neg_exp <- function(y, x, use_defaut_start = FALSE,
                              start_exp_neg = NULL, main = NULL, ...){
  
  #par(mfrow = c(1,1), mar = c(2,2,4,.1))
  
  ##########################
  
  if(use_defaut_start == TRUE ){
 
    
    start_exp_neg <- list(a = mean(y[x>99], na.rm = TRUE),
                          b = mean(y[x<5], na.rm = TRUE) - mean(y[x>95], na.rm = TRUE),
                          c = -0.02)
  

  }
  
  
  
  
  
  ###########################
  
  
  
  #Exponential Decelerating
  plot(y ~ x, main = main)
  curve(expotential(a = start_exp_neg$a, b = start_exp_neg$b, c = start_exp_neg$c, x), add = TRUE, lwd = 2)
  
  
}

