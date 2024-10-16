fit_functions <- function(y, x, use_defaut_start = FALSE, confint = TRUE,
                          start_linear = NULL, 
                          start_exp_pos = NULL,
                          start_exp_neg = NULL,
                          start_hof = NULL,
                          #start_sech_sym = NULL,
                          start_sech_asy = NULL, 
         algorithm = rep("port",5), 
         hof_lower = NULL,
         hof_upper = NULL,
         sech_upper = NULL,
         sech_lower = NULL, ...){
  

newdata <- data.frame(x = seq(0,100,0.1))


if(use_defaut_start == TRUE ){
  start_linear <- list(a = mean(y[x==0]),
                       b = ( mean(y[x == max(x)]) - mean(y[x == min(x)]) )/100 )
  
  
  start_exp_pos <- list(a = mean(y[x==0]),
                        b = mean(y[x<25]),
                        #b = ( mean(y[x == max(x)]) - mean(y[x == min(x)]) )/100,
                        c = 0.02)
  
  start_exp_neg <- list(a = mean(y[x>99]),
                        b = mean(y[x==0]) - mean(y[x>99]),
                        c = -0.02)
  
  start_hof <- list(a = mean(y[x==0]),
                    H = mean(y[x>99]),
                    m = x[ y == y[order(y)][length(y)/2]] ,
                    w = ( ( (mean(y[x == max(x)]) - mean(y[x == min(x)]) )  /100 ) )/100 )
  
  start_sech_asy <- list(a = mean(y[x==0]),
                         H = mean(y[x>99]),
                         m = x[y == max(y)] ,
                         s = 10,
                         r = 0)
  
  if(start_sech_asy$m < 5){start_sech_asy$m <- 5}
  if(start_sech_asy$m > 95){start_sech_asy$m <- 95}
  
  if(start_hof$m > 95){start_hof$m <- 95}
  if(start_hof$m < 5){start_hof$m <- 5}
}

  
##########################################################################################  
  
#No Effect Function
mod_no_effect <- lm(y ~ 1)

start_no_effect <- list(a = coef(mod_no_effect))


mod_no_effect <- nls(y ~ a,
                  #algorithm = algorithm[1],
                  start = start_no_effect)
                  #control = list(warnOnly = TRUE))#, ...)
  newdata
predicted_no_effect <- rep(predict(mod_no_effect, newdata = newdata),nrow(newdata))

  
##########################################################################################  
  
#Linear Function
if(is.null(start_linear)){
      start_linear <- list(a = 900, b = -8.75)
}
    
mod_linear <- nls(y ~ linear(a, b, x = x),
                  algorithm = algorithm[1],
                  start = start_linear,
                  control = list(maxiter = 10000000, warnOnly = TRUE))#, ...)

predicted_linear <- predict(mod_linear, newdata = newdata)




##########################################################################################


#Positive Exponential
if(is.null(start_exp_pos)){
  start_exp_pos <- list(a = 20, b = 900, c = 0.1)
}

mod_exp_pos <- nls(y ~ expotential(a, b, c, x = x),
                    algorithm = algorithm[2],
                    start = start_exp_pos,
                    lower = c(a = -Inf, b = -Inf, c = 0),
                    upper = c(a = Inf, b = Inf, c = Inf),
                    control = list(warnOnly = TRUE))#, ...)

predicted_exp_pos <- predict(mod_exp_pos, newdata = newdata)




##########################################################################################
    

#Negative Exponential
if(is.null(start_exp_neg)){
  start_exp_neg <- list(a = 20, b = 900, c = -0.1)
}

mod_exp_neg <- nls(y ~ expotential(a, b, c, x = x),
                   algorithm = algorithm[3],
                   start = start_exp_neg,
                   lower = c(a = -Inf, b = -Inf, c = -Inf),
                   upper = c(a = Inf, b = Inf, c = 0),
                   control = list(warnOnly = TRUE))#, ...)

predicted_exp_neg <- predict(mod_exp_neg, newdata = newdata)




##########################################################################################

if(is.null(hof_lower)){
  hof_lower <- c(a = min(y), H = min(y), m = 5, w = -Inf)
}
if(is.null(hof_upper)){
  hof_upper <- c(a = max(y), H = max(y), m = 95, w = Inf)
}



#Sigmoid Function
if(is.null(start_hof)){
  start_hof <- list(a = 900, H = 10, m = 50, w = 0.5)
}


if(start_hof$a < min(y)){start_hof$a <- min(y+0.0001)}

mod_hof <- nls(y ~ hofII(a, H, m, w, x = x),
               algorithm = algorithm[4],
               start = start_hof,
               lower = hof_lower,
               upper = hof_upper,
               control = list(warnOnly = TRUE))#, ...)

predicted_hof <- predict(mod_hof, newdata = newdata)



##########################################################################################
#Bell Function SYMETRIC
#if(is.null(start_sech_sym)){
#  start_sech_sym <- list(a = 0, H = 300, m = 15, s = 5)
#}

#mod_sech_sym <- nls(y ~ sech_symetrical_p_1_r0(a, H, m, s, x = x),
#                algorithm = algorithm[5],
#                start = start_sech_sym,
#                lower = c(a = -Inf, H = 0, m = -Inf, s = 0),
#                upper = c(a = Inf, H = Inf, m = Inf, s = Inf),
#                control = list(maxiter = 10000000, warnOnly = TRUE), ...)

#predicted_bell_sym <- predict(mod_sech_sym, newdata = newdata)


##########################################################################################
#Asymetric Bell Function
if(is.null(sech_lower)){
  sech_lower <-  c(a = -Inf, H = 0.000001, m = 5, s = 5, r = -0.999999)
}
if(is.null(sech_upper)){
  sech_upper <- c(a = max(y)*2, H = max(y)*2, m = 95, s = Inf, r = 0.999999)
}


if(is.null(start_sech_asy)){
  start_sech_asy <- list(a = 0, H = 300, m = 15, s = 5, r = 0)
}

mod_sech_asy <- nls(y ~ sech_p_1(a, H, m, s, r, x = x),
                    algorithm = algorithm[5],
                    start = start_sech_asy,
                    lower = sech_lower,
                    upper = sech_upper,
                    control = list(warnOnly = TRUE))#, ...)

predicted_bell_asy <- predict(mod_sech_asy, newdata = newdata)


##########################################################################################
  
library(bbmle)
models <- list(mod_no_effect = mod_no_effect,
               mod_linear = mod_linear,
               mod_exp_pos = mod_exp_pos,
               mod_exp_neg = mod_exp_neg,
               mod_hof = mod_hof,
               #mod_sech_sym = mod_sech_sym,
               mod_sech_asy = mod_sech_asy)
aictab <- data.frame(AICctab(mod_no_effect, mod_linear, mod_exp_pos, mod_exp_neg, mod_hof, #mod_sech_sym,
                             mod_sech_asy, weights = TRUE, base = TRUE, sort = FALSE))
                             #mnames = c("No effect",
                              #          "Linear",
                               #         "Positive Exponential",
                                #        "Negative Exponential",
                                  #      "HOF (Sigmoid)",
                                 #      "Symetric Bell-Shaped",
                                  #      "Asymetric Bell-Shaped")))
list_predicted <- list(predicted_no_effect, predicted_linear, predicted_exp_pos, predicted_exp_neg, predicted_hof,# predicted_bell_sym,
                       predicted_bell_asy)

name <- c("No_effect",
          "Linear",
          "Positive_Exponential",
          "Negative_Exponential",
          "HOF_(Sigmoid)",
          #"Symetric_Bell-Shaped",
          "Asymetric_Bell-Shaped")

names(list_predicted) <- name
names(models) <- name
rownames(aictab) <- name

best_predicted <- list_predicted[which(aictab$dAICc==0)]
best_model <- models[which(aictab$dAICc==0)]





############# COEFFICIENTS ##############

coef_mod_no_effect <- coef(mod_no_effect)
coef_mod_linear <- coef(mod_linear)
coef_mod_exp_pos <- coef(mod_exp_pos)
coef_mod_exp_neg <- coef(mod_exp_neg)
coef_mod_hof <- coef(mod_hof)
coef_mod_sech_asy<- coef(mod_sech_asy)

#########################################

############# COEFFICIENTS STANDARDIZED ##############

coefs_mean <- list(mod_no_effect = coef_mod_no_effect,
                   mod_linear = coef_mod_linear,
                   mod_exp_pos = coef_mod_exp_pos,
                   mod_exp_neg = coef_mod_exp_neg,
                   mod_hof = coef_mod_hof,
                   mod_sech_asy = coef_mod_sech_asy)

center <- mean(y)
scale <- sd(y)

coef_stand_mod_no_effect <- ( coef(mod_no_effect) - center) / scale
coef_stand_mod_linear <- c( ( coef(mod_linear)[1]  - center) / scale,
                            coef(mod_linear)[2]/scale)

coef_stand_mod_exp_pos <- c( ( coef(mod_exp_pos)[1]  - center) / scale,
                             coef(mod_exp_pos)[2]/scale,
                             coef(mod_exp_pos)[3])

coef_stand_mod_exp_neg <- c( ( coef(mod_exp_neg)[1]  - center) / scale,
                             coef(mod_exp_neg)[2]/scale,
                             coef(mod_exp_neg)[3])

coef_stand_mod_hof <- c( ( coef(mod_hof)[1]  - center) / scale,
                         coef(mod_hof)[2]/scale,
                         coef(mod_hof)[3],
                         coef(mod_hof)[4])

coef_stand_mod_sech_asy <- c( ( coef(mod_sech_asy)[1]  - center) / scale,
                              coef(mod_sech_asy)[2]/scale,
                              coef(mod_sech_asy)[3],
                              coef(mod_sech_asy)[4],
                              coef(mod_sech_asy)[5])

coefs_stand_mean <- list(mod_no_effect = coef_stand_mod_no_effect,
                         mod_linear = coef_stand_mod_linear,
                         mod_exp_pos = coef_stand_mod_exp_pos,
                         mod_exp_neg = coef_stand_mod_exp_neg,
                         mod_hof = coef_stand_mod_hof,
                         mod_sech_asy = coef_stand_mod_sech_asy)


##############################################################

################# CONFIDENCE INTERVAL ########################
if(isTRUE(confint)){
#  confint_mean <- list(mod_no_effect = confint(mod_no_effect),
#                       
#                       mod_linear = confint(mod_linear),
#                       
#                       mod_exp_pos = confint(mod_exp_pos),
#                       
#                       mod_exp_neg = confint(mod_exp_neg)
#                       
#                       )
                       #mod_hof = confint(mod_hof, parm = parm_hof),
                       #mod_sech_asy = confint(mod_sech_asy), parm = parm_bell)
  
  
  
  
  
  
  
  
  
  confint_mean <- list(mod_no_effect = confint(mod_no_effect),
                       
                       tryCatch(
                         # This is what I want to do...
                         {
                           confint(mod_linear)
                         },
                         # ... but if an error occurs, tell me what happened: 
                         error=function(error_message) {
                           message("confint was not calculated for Linear model")
                           message(error_message)
                           return(NA)
                         }
                       )
                       ,
                       
                       tryCatch(
                         # This is what I want to do...
                         {
                           confint(mod_exp_pos)
                         },
                         # ... but if an error occurs, tell me what happened: 
                         error=function(error_message) {
                           message("confint was not calculated for Positive exponential model")
                           message(error_message)
                           return(mod_exp_pos = NA)
                         }
                       ),
                       
                       tryCatch(
                         # This is what I want to do...
                         {
                           confint(mod_exp_neg)
                         },
                         # ... but if an error occurs, tell me what happened: 
                         error=function(error_message) {
                           message("confint was not calculated for Negaive exponential model")
                           message(error_message)
                           return(mod_exp_neg = NA)
                         }
                       ),
                       
                       tryCatch(
                         # This is what I want to do...
                         {
                           confint(mod_hof)
                         },
                         # ... but if an error occurs, tell me what happened: 
                         error=function(error_message) {
                           message("confint was not calculated for HOF model")
                           message(error_message)
                           return(mod_exp_neg = NA)
                         }
                       ),
                       
                       
                       tryCatch(
                         # This is what I want to do...
                         {
                           confint(mod_sech_asy)
                         },
                         # ... but if an error occurs, tell me what happened: 
                         error=function(error_message) {
                           message("confint was not calculated for Sech Asymetric model")
                           message(error_message)
                           return(mod_sech_asy = NA)
                         }
                       )
                       
                       
  )
  #mod_hof = confint(mod_hof, parm = parm_hof),
  #mod_sech_asy = confint(mod_sech_asy), parm = parm_bell)
  
  
  names(confint_mean) <- c("mod_no_effect", "mod_linear", "mod_exp_pos", "mod_exp_neg", "mod_hof", "mod_sech_asy")
  
  
  

  
  
  
  
  confint_mean_stand <- confint_mean
  
  confint_mean_stand$mod_no_effect <- ( confint_mean$mod_no_effect - center) / scale
  
  confint_mean_stand$mod_linear[1,] <- ( confint_mean$mod_linear[1,] - center) / scale
  confint_mean_stand$mod_linear[2,] <- ( confint_mean$mod_linear[2,]) / scale
  
  
  tryCatch(
    # This is what I want to do...
    {
      confint_mean_stand$mod_exp_pos[1,] <- ( confint_mean$mod_exp_pos[1,] - center) / scale
      confint_mean_stand$mod_exp_pos[2,] <- ( confint_mean$mod_exp_pos[2,]) / scale
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      return(NA)
    }
  )
  
  
  tryCatch(
    # This is what I want to do...
    {
      confint_mean_stand$mod_exp_neg[1,] <- ( confint_mean$mod_exp_neg[1,] - center) / scale
      confint_mean_stand$mod_exp_neg[2,] <- ( confint_mean$mod_exp_neg[2,]) / scale
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {

      return(NA)
    }
  )
  
  
  tryCatch(
    # This is what I want to do...
    {
      confint_mean_stand$mod_hof[1,] <- ( confint_mean$mod_hof[1,] - center) / scale
      confint_mean_stand$mod_hof[2,] <- ( confint_mean$mod_hof[2,]) / scale
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
    return(NA)
    }
  )
  

  tryCatch(
    # This is what I want to do...
    {
      confint_mean_stand$mod_sech_asy[1,] <- ( confint_mean$mod_sech_asy[1,] - center) / scale
      confint_mean_stand$mod_sech_asy[2,] <- ( confint_mean$mod_sech_asy[2,]) / scale
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {

      return(NA)
    }
  )
  

  
  
}else{confint_mean <- NA; confint_mean_stand <- NA}



##############################################################





return(list(aictab = aictab,
            best_model = best_model,
            best_predicted = best_predicted,
            all_predicted = list_predicted,
            all_models = models,
            newdata_x = newdata$x,
            coefs_mean = coefs_mean,
            coefs_stand_mean = coefs_stand_mean,
            confint_mean = confint_mean,
            confint_mean_stand = confint_mean_stand))

}
