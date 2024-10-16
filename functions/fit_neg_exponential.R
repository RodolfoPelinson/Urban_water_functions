fit_neg_exponential <- function(y, x,  start_exp_neg = NULL, algorithm = "port", limit_c_to_zero = TRUE, confint = TRUE,...){


newdata <- data.frame(x = seq(min(x),max(x),length.out = 100 ))


#Negative Exponential
if(is.null(start_exp_neg)){
  start_exp_neg <- list(a = mean(y[x==max(y)]),
                        b = mean(y[x==min(y)]) - mean(y[max(y)]),
                        c = -0.02)
  }

if(isTRUE(limit_c_to_zero)){
  lower <- c(a = -Inf, b = -Inf, c = -Inf)
  upper <- c(a = Inf, b = Inf, c = 0)
}else{
  lower <- c(a = -Inf, b = -Inf, c = -Inf)
  upper <- c(a = Inf, b = Inf, c = Inf)
}



mod_exp_neg <- nls(y ~ expotential(a, b, c, x = x),
                   algorithm = algorithm,
                   start = start_exp_neg,
                   lower = lower,
                   upper = upper,
                   control = list(warnOnly = TRUE, maxiter = 500, minFactor = 1/2048), ...)

predicted_exp_neg <- predict(mod_exp_neg, newdata = newdata)


mod_exp_neg$data

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

library(bbmle)
models <- list(mod_no_effect = mod_no_effect,
               mod_exp_neg = mod_exp_neg)
aictab <- data.frame(AICctab(mod_no_effect, mod_exp_neg, weights = TRUE, base = TRUE, sort = FALSE))

list_predicted <- list(predicted_no_effect, predicted_exp_neg)

name <- c("No_effect",
          "Negative_Exponential")

names(list_predicted) <- name
names(models) <- name
rownames(aictab) <- name

best_predicted <- list_predicted[which(aictab$dAICc==0)]
best_model <- models[which(aictab$dAICc==0)]


###########################################################################
coef_mod_no_effect <- coef(mod_no_effect)
coef_mod_exp_neg <- coef(mod_exp_neg)

###########################################################################
############# COEFFICIENTS ##############


coefs_mean <- list(mod_no_effect = coef_mod_no_effect,
                   mod_exp_neg = coef_mod_exp_neg)

############# COEFFICIENTS STANDARDIZED ##############


center <- mean(y)
scale <- sd(y)

coef_stand_mod_no_effect <- ( coef(mod_no_effect) - center) / scale

coef_stand_mod_exp_neg <- c( ( coef(mod_exp_neg)[1]  - center) / scale,
                             coef(mod_exp_neg)[2]/scale,
                             coef(mod_exp_neg)[3])

coefs_stand_mean <- list(mod_no_effect = coef_stand_mod_no_effect,
                         mod_exp_neg = coef_stand_mod_exp_neg)


################# CONFIDENCE INTERVAL ########################


if(isTRUE(confint)){

  
  
  
  confint_mean <- list(mod_no_effect = confint(mod_no_effect),
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
                       )
  )

  
  
  names(confint_mean) <- c("mod_no_effect", "mod_exp_neg")
  
  
  confint_mean_stand <- confint_mean
  
  confint_mean_stand$mod_no_effect <- ( confint_mean$mod_no_effect - center) / scale
  
  confint_mean_stand$mod_linear[1,] <- ( confint_mean$mod_linear[1,] - center) / scale
  confint_mean_stand$mod_linear[2,] <- ( confint_mean$mod_linear[2,]) / scale
  
  
 
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
