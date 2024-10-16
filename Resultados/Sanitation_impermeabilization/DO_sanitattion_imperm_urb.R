do <- water_quality_agua_mean$DO_.mg.L.
urb <- water_quality_agua_mean$X.urb
D_sanitation <- map_biomas_dados$habitantes_sem_saneamento/100
imperm <- map_biomas_dados$ArImp_2021_ha_Vect
library(vegan)
D_sanitation <- decostand(D_sanitation, method = "stand")
imperm <- decostand(imperm, method = "stand")
urb_st <- decostand(urb, method = "stand")


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = urb_st,
                  start_linear = list(a = 3, b = -2), 
                  start_exp_pos = list(a = 4, b = -1, c = 1),
                  start_exp_neg = list(a = 1, b= 1, c = -1.5),
                  start_hof = list(a = 1, H = 6, m = -1, w = -0.2),
                  #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                  start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5))

#dev.off()

fit_do_urb <- fit_functions(y = do, x = urb_st,
                        start_linear = list(a = 5, b = -0.05), 
                        start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                        start_exp_neg = list(a = 1, b= 6, c = -0.1),
                        start_hof = list(a = 1, H = 6, m = 55, w = -0.2),
                        #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                        start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5))
#weights = do_weights)

fit_do_urb$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = urb,
                  start_linear = as.list(coef(fit_do_urb$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_do_urb$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_do_urb$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_do_urb$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_do$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_do_urb$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_do_urb <- run_all_models(fit_do_urb, y = do,x = urb)

list_loglik_urb <- resultado_do_urb$log_likehoods

aictab_urb_urb <- AICctab(list_loglik_urb, base = TRUE)
aictab_urb_urb






######################################################################################################################
######################################################################################################################
######################################################################################################################


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = imperm,
                  start_linear = list(a = 5, b = -0.05), 
                  start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                  start_exp_neg = list(a = 1, b= 6, c = -0.1),
                  start_hof = list(a = 1, H = 6, m = 55, w = -0.2),
                  #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                  start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5))

#dev.off()

fit_do_imperm <- fit_functions(y = do, x = imperm,
                        start_linear = list(a = 5, b = -0.05), 
                        start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                        start_exp_neg = list(a = 1, b= 6, c = -0.1),
                        start_hof = list(a = 1, H = 6, m = 55, w = -0.2),
                        #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                        start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5),
                        hof_lower = c(a = min(do), H = min(do), m = -Inf, w = -Inf),
                        hof_upper = c(a = max(do), H = max(do), m = Inf, w = -Inf))
#weights = do_weights)

fit_do_imperm$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = imperm,
                  start_linear = as.list(coef(fit_do_imperm$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_do_imperm$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_do_imperm$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_do_imperm$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_do$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_do_imperm$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_do_imperm <- run_all_models(fit_do_imperm, y = do,x = imperm)

list_loglik_imperm <- resultado_do_imperm$log_likehoods

aictab_imperm <- AICctab(list_loglik_imperm, base = TRUE)
aictab_imperm
#write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_DO.csv")


#################################################################

######################################################################################################################
######################################################################################################################
######################################################################################################################




#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = D_sanitation,
                  start_linear = list(a = 5, b = -0.05), 
                  start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                  start_exp_neg = list(a = 1, b= 6, c = -0.1),
                  start_hof = list(a = 1, H = 8, m = 20, w = -0.4),
                  #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                  start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5))

#dev.off()

fit_do_sanitation <- fit_functions(y = do, x = D_sanitation,
                        start_linear = list(a = 5, b = -0.05), 
                        start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                        start_exp_neg = list(a = 1, b= 6, c = -0.1),
                        start_hof = list(a = 1, H = 8, m = 20, w = -0.4),
                        #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                        start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5),
                        hof_lower = c(a = min(do), H = min(do), m = -Inf, w = -Inf),
                        hof_upper = c(a = max(do), H = max(do), m = Inf, w = Inf))
#weights = do_weights)

fit_do$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = D_sanitation,
                  start_linear = as.list(coef(fit_do_sanitation$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_do_sanitation$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_do_sanitation$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_do_sanitation$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_do$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_do_sanitation$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_do_D_sanitation <- run_all_models(fit_do_sanitation, y = do,x = D_sanitation)

list_loglik_D_sanitation <- resultado_do_D_sanitation$log_likehoods

aictab_D_sanitation_D_sanitation <- AICctab(list_loglik_D_sanitation, base = TRUE)
aictab_D_sanitation_D_sanitation


##################################################################
aictab_D_sanitation_D_sanitation
aictab_imperm
aictab_urb

lista_preditores <- list(sanitation = resultado_do_D_sanitation$log_likehoods$Negative_Exponential.Linear_Negative,
                         imperm = resultado_do_imperm$log_likehoods$Negative_Exponential.Linear_Negative,
                         urbanization = resultado_do_urb$log_likehoods$Negative_Exponential.Constant)

AICctab(lista_preditores, base = TRUE)


####################################################################

mean_cs <- c(Urbanization = fit_do_urb$coefs_mean$mod_exp_neg[3],
             Impermeabilization = fit_do_imperm$coefs_mean$mod_exp_neg[3],
             Sanitation = fit_do_sanitation$coefs_mean$mod_exp_neg[3])

lower_cs <- c(Urbanization = fit_do_urb$confint_mean$mod_exp_neg[3,1],
              Impermeabilization = fit_do_imperm$confint_mean$mod_exp_neg[3,1],
              Sanitation = fit_do_sanitation$confint_mean$mod_exp_neg[3,1])

upper_cs <- c(Urbanization = fit_do_urb$confint_mean$mod_exp_neg[3,2],
              Impermeabilization = fit_do_imperm$confint_mean$mod_exp_neg[3,2],
              Sanitation = fit_do_sanitation$confint_mean$mod_exp_neg[3,2])

names <- c("Urbanization", "Impermeabilization", "Sanitation")

par(mar = c(4,10,1,1))
My_coefplot(mean_cs, upper_cs, lower_cs, species_labels = names, xlab = "Negative Exponential: c", cex.axis = 1,y_spa = 0, xlim = c(-0.7, 0))


####################################################################



resid_imperm <- resid(lm(imperm ~ urb))
resid_sanitation <- resid(lm(D_sanitation ~ urb))

resid_do <- residuals(fit_do_urb$best_model$Negative_Exponential)

plot(resid_do ~ resid_imperm)
plot(resid_do ~ resid_sanitation)

mod_resid_no_effect <- lm(resid_do ~ 1)
mod_resid_imperm <- lm(resid_do ~ resid_imperm)
mod_resid_sanitation <- lm(resid_do ~ resid_sanitation)
mod_resid_both <- lm(resid_do ~ resid_sanitation + resid_imperm)

AICctab(mod_resid_no_effect, mod_resid_imperm, mod_resid_sanitation, mod_resid_both)

#####################################################################
library(mgcv)

mod_resid_no_effect_gam <- gam(resid_do ~ 1)
mod_resid_imperm_gam <- gam(resid_do ~ s(resid_imperm, k = 10))
mod_resid_sanitation_gam <- gam(resid_do ~ s(resid_sanitation, k = 10))
mod_resid_both_gam <- gam(resid_do ~ s(resid_sanitation, k = 10) + s(resid_imperm, k = 10))

AICctab(mod_resid_no_effect_gam, mod_resid_imperm_gam, mod_resid_sanitation_gam, mod_resid_both_gam)

###################################################################