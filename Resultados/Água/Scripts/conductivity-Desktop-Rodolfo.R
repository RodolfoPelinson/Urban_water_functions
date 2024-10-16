condutividade <- water_quality_agua_mean$Conductivity_.uS.cm.
urb <- water_quality_agua_mean$X.urb
condutividade_weights <- weights_agua$Conductivity_.uS.cm.

length(condutividade)
length(condutividade_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = condutividade, x = urb,
                              start_linear = list(a = 0, b = 7), 
                              start_exp_pos = list(a = 0, b = 50, c = 0.03),
                              start_exp_neg = list(a = 1000, b= -1000, c = -0.01),
                              start_hof = list(a = 50, H = 600, m = 50, w = 0.04),
                              #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                              start_sech_asy = list(a = 0, H = 600, m = 95, s = 50,r = 0))

#dev.off()
  
fit_condutividade <- fit_functions(y = condutividade, x = urb,
                                   start_linear = list(a = 0, b = 7), 
                                   start_exp_pos = list(a = 0, b = 50, c = 0.03),
                                   start_exp_neg = list(a = 1000, b= -1000, c = -0.01),
                                   start_hof = list(a = 50, H = 600, m = 50, w = 0.04),
                                   #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                   start_sech_asy = list(a = 0, H = 600, m = 95, s = 50, r = 0))#,
                                   #weights = condutividade_weights)

fit_condutividade$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = condutividade, x = urb,
                  start_linear = as.list(coef(fit_condutividade$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_condutividade$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_condutividade$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_condutividade$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_condutividade$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_condutividade$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()




#################################################################

resultado_condutividade <- run_all_models(fit_condutividade, y = condutividade,x = urb)

list_loglik <- resultado_condutividade$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_Conductivity.csv")

resultado_condutividade$all_sd_models$Linear$all_models$Quadratic

#################################################################


########### Predicting

coefs_sd <- coef(resultado_condutividade$all_sd_models$Negative_Exponential$best_model[[1]])

new_x <- seq(0,100,0.1)

predicted_mean <- fit_condutividade$best_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


########## Ploting




png("Resultados/Água/Curvas/Conductivity.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(condutividade ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Conductivity (uS/cm)", line = 2, cex.lab = 1.25)

#title(ylab = "Density of houses without", line = 2.5, cex.lab = 1.25)
#title(ylab = "sanitation (Houses/ha)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

points(condutividade ~ urb, pch = 16)

dev.off()





#####################################

condutividade_stand <- vegan::decostand(condutividade, method = "stand")

condutividade_stand* attr(condutividade_stand, "scaled:scale") + attr(condutividade_stand, "scaled:center")

scale <- attr(condutividade_stand, "scaled:scale")
center <- attr(condutividade_stand, "scaled:center")
                                                                      
                                                                      
example_functions(y = condutividade_stand, x = urb,
                  start_linear = list(a = (0-center)/scale, b = (7/scale)), 
                  start_exp_pos = list(a = (0-center)/scale, b = 50/scale, c = 0.03/scale),
                  start_exp_neg = list(a = (1000-center)/scale, b= -1000, c = -0.01/scale),
                  start_hof = list(a = (50-center)/scale, H = (600-center)/scale, m = 50, w = 0.04),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = (0-center)/scale, H = 600/scale, m = 95/scale, s = 50/scale,r = 0/scale))


#dev.off()

fit_condutividade_stand <- fit_functions(y = condutividade_stand, x = urb,
                                   start_linear = list(a = 0, b = 7), 
                                   start_exp_pos = list(a = 0, b = 50, c = 0.03),
                                   start_exp_neg = list(a = 1000, b= -1000, c = -0.01),
                                   start_hof = list(a = 50, H = 600, m = 50, w = 0.04),
                                   #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                   start_sech_asy = list(a = 0, H = 600, m = 95, s = 50, r = 0))#,
#weights = condutividade_weights)


fit_condutividade$coefs_mean$mod_exp_neg
fit_condutividade$coefs_stand_mean$mod_exp_neg

best_model <- fit_condutividade$all_models$Negative_Exponential

confint <- confint(best_model)

(confint[1,] - center)/scale

(confint[2,])/scale

confint[3,]
