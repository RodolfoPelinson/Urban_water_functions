descarga <- water_quality_agua_mean$Discharge_.L.s.
urb <- water_quality_agua_mean$X.urb
descarga_weights <- weights_agua$Discharge_.L.s.

length(descarga)
length(descarga_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = descarga, x = urb,
                  start_linear = list(a = 1, b = 1), 
                  start_exp_pos = list(a = 0, b = 30, c = 0.01),
                  start_exp_neg = list(a = 1000, b= -1000, c = -0.001),
                  start_hof = list(a = 15, H = 100, m = 50, w = 0.04),
                  #start_sech_sym = list(a = 10, H = 200, m = 60, s = 55),
                  start_sech_asy = list(a = 10, H = 200, m = 60, s = 50, r = 0.7))

#dev.off()

fit_descarga <- fit_functions(y = descarga, x = urb,
                                   start_linear = list(a = 1, b = 1), 
                                   start_exp_pos = list(a = 0, b = 30, c = 0.01),
                                   start_exp_neg = list(a = 1000, b= -1000, c = -0.001),
                                   start_hof = list(a = 15, H = 100, m = 50, w = 0.04),
                                   #start_sech_sym = list(a = 10, H = 200, m = 60, s = 55),
                                   start_sech_asy = list(a = 10, H = 200, m = 60, s = 50, r = 0.7))
                                   #weights = descarga_weights)

fit_descarga$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = descarga, x = urb,
                  start_linear = as.list(coef(fit_descarga$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_descarga$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_descarga$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_descarga$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_descarga$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_descarga$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_descarga <- run_all_models(fit_descarga, y = descarga,x = urb)

list_loglik <- resultado_descarga$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_Discharge.csv")

#################################################################


########### Predicting

coefs_sd <- coef(resultado_descarga$all_sd_models$Negative_Exponential$best_model[[1]])

new_x <- seq(0,100,0.1)

predicted_mean <- fit_descarga$best_predicted$Linear
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd




########## Ploting




png("Resultados/Água/Curvas/Discharge.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(descarga ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Discharge (L/s)", line = 2, cex.lab = 1.25)

#title(ylab = "Density of houses without", line = 2.5, cex.lab = 1.25)
#title(ylab = "sanitation (Houses/ha)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

points(descarga ~ urb, pch = 16)
box()

dev.off()



#####################################

