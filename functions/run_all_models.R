run_all_models <- function(mean_models_list, x, y, ...){
  
  mean_models <- mean_models_list$all_models
  

  models_sd_list <- list()
  list_expected_means <- list()
  list_expected_sds <- list()
  all_sd_models <- list()
  log_likehoods <- list()
  
  #Loop que inicia o calculo por modelo de média
  for(i in 1:length(mean_models)){
    list_expected_sds_per_mean <- list()
    
    
    residuals <- resid(mean_models[[i]])
    
    models_sd <- fit_sd(y = residuals, x = x, ...)
    
    models_sd_list[[i]] <- models_sd$all_models
    
    
    
    #Calculo por modelo de variancia
    
    #Constante
    list_expected_sds_per_mean[[1]] <- rep(coef(models_sd_list[[i]][[1]]),length(x))
    #Linear Positivo
    list_expected_sds_per_mean[[2]] <- exp(coef(models_sd_list[[i]][[2]])[1] + coef(models_sd_list[[i]][[2]])[2]*x)
    #Linear Negativo
    list_expected_sds_per_mean[[3]] <- exp(coef(models_sd_list[[i]][[3]])[1] + coef(models_sd_list[[i]][[3]])[2]*x)
    #Quadrativo
    list_expected_sds_per_mean[[4]] <- exp(coef(models_sd_list[[i]][[4]])[1] + coef(models_sd_list[[i]][[4]])[2]*x + coef(models_sd_list[[i]][[4]])[3]*(x^2))
    

    name <- c("Constant",
              "Linear_Positive",
              "Linear_Negative",
              "Quadratic")
    
    all_sd_models[[i]] <- models_sd
    
    
    
    #Valores preditos para variancia e para média
    list_expected_sds[[i]] <- list_expected_sds_per_mean
    list_expected_means[[i]] <- predict(mean_models[[i]])
    
    
    log_likehoods_per_mean <- list()
    
    #Calculo da logverossimilhança negativa por modelo
    for(j in 1:length(models_sd_list[[i]])){
      
      nll <- sum(dnorm(x = y, mean=list_expected_means[[i]], sd=list_expected_sds[[i]][[j]], log=TRUE))
      class(nll) <- "logLik"
      
      atributos_mean <- attributes(logLik(mean_models[[i]]))
      
      atributos_sd <- attributes(logLik(models_sd_list[[i]][[j]]))
      
      attr(nll, "df") <- atributos_mean$df + atributos_sd$df
      attr(nll, "nobs") <- atributos_mean$nobs
      attr(nll, "nall") <- atributos_mean$nall
      
      log_likehoods_per_mean[[j]] <- nll
    }
    
    names(log_likehoods_per_mean) <- c("Constant",
                                       "Linear_Positive",
                                       "Linear_Negative",
                                       "Quadratic")
    
    log_likehoods[[i]] <- log_likehoods_per_mean
    
    
  }
  
names(all_sd_models) <- names(mean_models)
  

  
names(log_likehoods) <- names(mean_models)

log_likeloods_unlisted <- log_likehoods[[1]]
for(i in 2:length(log_likehoods)){
  log_likeloods_unlisted <- c(log_likeloods_unlisted, log_likehoods[[i]])
}

names(log_likeloods_unlisted) <- names(unlist(log_likehoods))

return(list(log_likehoods = log_likeloods_unlisted,
            all_sd_models = all_sd_models,
            predicted_sds = list_expected_sds))
  
}
