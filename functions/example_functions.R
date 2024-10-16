



example_functions <- function(y, x, use_defaut_start = FALSE,
                              start_linear = NULL, 
                              start_exp_pos = NULL,
                              start_exp_neg = NULL,
                              start_hof = NULL,
                              #start_sech_sym = NULL,
                              start_sech_asy = NULL, ...){
  
  par(mfrow = c(3,2), mar = c(2,2,4,.1))
  
  ##########################
  
if(use_defaut_start == TRUE ){
  start_linear <- list(a = mean(y[x==0], na.rm = TRUE),
                       b = ( mean(y[x > 95], na.rm = TRUE) - mean(y[x < 5], na.rm = TRUE) )/100 )
  
  
  start_exp_pos <- list(a = mean(y[x==0], na.rm = TRUE),
                        b = mean(y[x<25], na.rm = TRUE),
                        #b = ( mean(y[x == max(x)]) - mean(y[x == min(x)]) )/100,
                        c = 0.02)
  
  start_exp_neg <- list(a = mean(y[x>99], na.rm = TRUE),
                        b = mean(y[x<5], na.rm = TRUE) - mean(y[x>95], na.rm = TRUE),
                        c = -0.02)
  
  start_hof <- list(a = mean(y[x==0], na.rm = TRUE),
                    H = mean(y[x>95], na.rm = TRUE),
                    m = x[ y == y[order(y)][length(y)/2]][1] ,
                    w = ( ( (mean(y[x > 95], na.rm = TRUE) - mean(y[x < 5], na.rm = TRUE) )  /100 ) )/100 )
  
  start_sech_asy <- list(a = mean(y[x < 5], na.rm = TRUE),
                         H = mean(y[x > 95], na.rm = TRUE),
                         m = x[y == max(y)] ,
                         s = 10,
                         r = 0)
  
  if(start_sech_asy$m <= 5){start_sech_asy$m <- 5}
  if(start_sech_asy$m >= 95){start_sech_asy$m <- 95}
  
  if(start_hof$m >= 95){start_sech_asy$m <- 95}
  if(start_hof$m >= 95){start_sech_asy$m <- 95}
}
  
  
  
  
  
  ###########################
  
  
  
  
  #No Effect
  #plot(y ~ x, main = "No Effect")
  #abline(h = mean(y), lwd = 2)  
  #par(mfrow = c(1,1))
  
  #Linear
  plot(y ~ x, main = "Linear")
  curve(linear(a = start_linear$a, b = start_linear$b, x), add = TRUE, lwd = 2)
  
  #Exponential acelerating
  plot(y ~ x, main = "Positive Exponential")
  curve(expotential(a = start_exp_pos$a, b = start_exp_pos$b, c = start_exp_pos$c, x), add = TRUE, lwd = 2)
  
  #Exponential Decelerating
  plot(y ~ x, main = "Negative Exponential")
  curve(expotential(a = start_exp_neg$a, b = start_exp_neg$b, c = start_exp_neg$c, x), add = TRUE, lwd = 2)
  
  #Sigmoid
  plot(y ~ x, main = "HOF (Sigmoid)")
  curve(hofII(a = start_hof$a, H = start_hof$H, m = start_hof$m, w = start_hof$w, x), add = TRUE, lwd = 2)
  
  #Symmetric Sech (Bell)
  #plot(y ~ x, main = "Symmetric Sech (Bell)")
  #curve(sech_symetrical_p_1_r0(a = start_sech_sym$a, H = start_sech_sym$H, m = start_sech_sym$m, s = start_sech_sym$s, x), add = TRUE, lwd = 2)
  
  #Asymmetric Sech (Asymmetric Bell)
  plot(y ~ x, main = "Asymmetric Sech (Bell)")
  curve(sech_p_1(a = start_sech_asy$a, H = start_sech_asy$H, m = start_sech_asy$m, s = start_sech_asy$s, r = start_sech_asy$r, x), add = TRUE, lwd = 2)
  
  
}





