fit_sd <- function(y,x2,
                   max_intercept_quad = Inf, max_slope_quad = Inf, max_slope_sd2_quad = Inf,
                   min_intercept_quad = -Inf, min_slope_quad = -Inf, min_slope_sd2_quad = -Inf,
                   max_intercept_lin_pos = Inf, max_slope_lin_pos = Inf,
                   min_intercept_lin_pos = -Inf,
                   max_intercept_lin_neg = Inf,
                   min_intercept_lin_neg = -Inf, min_slope_lin_neg = -Inf,
                   
                   ...){ #adicionar: optimizer = "nlminb"
  
  library(bbmle)
  
  ###Desvio constante
  nll_const_desv = function( intercept_sd ,x = y, x1 = x2){
    sd <- intercept_sd
    -sum(dnorm(x, mean=0, sd=sd, log=TRUE))
  }
  
  ###Desvio linear
  nll_linear_desv = function( intercept_sd, slope_sd, x = y, x1 = x2){
    sd <- exp(intercept_sd + slope_sd*x1)
    -sum(dnorm(x, mean=0, sd=sd, log=TRUE))
  }
  
  ###Desvio quadrático
  nll_quad_desv = function(intercept_sd, slope_sd, slope_sd2, x = y, x1 = x2){
    sd <- exp(intercept_sd + slope_sd*x1 + slope_sd2*(x1^2))
    -sum(dnorm(x, mean=0, sd=sd, log=TRUE))
  }
  
  
  #Trabalhar melhor nos valores iniciais
  
  mod_const_desv <- mle2(nll_const_desv,start=list(intercept_sd = sd(y)))
  
  mod_linear_desv_pos <- mle2(nll_linear_desv,start=list(intercept_sd = sd(y), slope_sd = 0),  optimizer = "nlminb",
                              lower = c(intercept_sd = min_intercept_lin_pos, slope_sd = 0.0000000000001),
                              upper = c(intercept_sd = max_intercept_lin_pos, slope_sd = max_slope_lin_pos))
  
  mod_linear_desv_neg <- mle2(nll_linear_desv,start=list(intercept_sd = sd(y), slope_sd = 0),  optimizer = "nlminb",
                              lower = c(intercept_sd = min_intercept_lin_neg, slope_sd = min_slope_lin_neg),
                              upper = c(intercept_sd = max_intercept_lin_neg, slope_sd = 0.0000000000001))
  
  mod_quad_desv <- mle2(nll_quad_desv,start=list(intercept_sd = coef(mod_linear_desv_pos)[1],  slope_sd = 0, slope_sd2 = 0),  optimizer = "nlminb",
                        lower = c(intercept_sd = min_intercept_quad, slope_sd = min_slope_quad, slope_sd2 = min_slope_sd2_quad),
                        upper = c(intercept_sd = max_intercept_quad, slope_sd = max_slope_quad, slope_sd2 = max_slope_sd2_quad))
  
  models <- list(mod_const_desv, mod_linear_desv_pos, mod_linear_desv_neg, mod_quad_desv)
  
  name <- c("Constant",
            "Linear_Positive",
            "Linear_Negative",
            "Quadratic")
  
  names(models) <- name
  
  aictab <- data.frame(AICctab(mod_const_desv, mod_linear_desv_pos, mod_linear_desv_neg,mod_quad_desv, base = TRUE, sort = FALSE, mnames = name, nobs = length(y)))
  
  best_model <- models[which(aictab$dAICc==0)]
  
  
  return(list(best_model = best_model, all_models = models, aictab = aictab))
  
}



