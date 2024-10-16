phycocianin <- water_quality_agua_mean$phycocyanin
urb <- water_quality_agua_mean$X.urb
phycocianin_weights <- weights_agua$phycocyanin

length(phycocianin)
length(phycocianin_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = phycocianin, x = urb,
                  start_linear = list(a = 1, b = 0.6), 
                  start_exp_pos = list(a = 5, b = 3, c = 0.03),
                  start_exp_neg = list(a = 100, b= -95, c = -0.01),
                  start_hof = list(a = 0, H = 60, m = 50, w = 0.04),
                 # start_sech_sym = list(a = 1, H = 60, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 60, m = 95, s = 50, r = 0))

#dev.off()

fit_phycocianin <- fit_functions(y = phycocianin, x = urb,
                                 start_linear = list(a = 1, b = 0.6), 
                                 start_exp_pos = list(a = 5, b = 3, c = 0.03),
                                 start_exp_neg = list(a = 100, b= -95, c = -0.01),
                                 start_hof = list(a = 0, H = 60, m = 50, w = 0.04),
                                 #start_sech_sym = list(a = 1, H = 60, m = 95, s = 50),
                                 start_sech_asy = list(a = 0, H = 60, m = 95, s = 50, r = -0.7))
                                   #weights = phycocianin_weights)

fit_phycocianin$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = phycocianin, x = urb,
                  start_linear = as.list(coef(fit_phycocianin$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_phycocianin$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_phycocianin$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_phycocianin$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_phycocianin$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_phycocianin$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_phycocianin <- run_all_models(fit_phycocianin, y = phycocianin,x = urb)

list_loglik <- resultado_phycocianin$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_Ficocianina.csv")

resultado_phycocianin$all_sd_models$Linear$all_models$Quadratic

#################################################################


########### Predicting

coefs_sd <- coef(resultado_phycocianin$all_sd_models$Linear$best_model[[1]])

new_x <- seq(0,100,0.1)

predicted_mean <- fit_phycocianin$best_predicted$Linear
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


########## Ploting




png("Resultados/Água/Curvas/Ficocianina.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(phycocianin ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Phycocyanin", line = 2, cex.lab = 1.25)

#title(ylab = "Density of houses without", line = 2.5, cex.lab = 1.25)
#title(ylab = "sanitation (Houses/ha)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

points(phycocianin ~ urb, pch = 16)

dev.off()



#####################################







