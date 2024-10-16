pH <- water_quality_agua_mean$pH
urb <- water_quality_agua_mean$X.urb
pH_weights <- weights_agua$pH

length(pH)
length(pH_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = pH, x = urb,
                  start_linear = list(a = 6, b = 0.015), 
                  start_exp_pos = list(a = 5, b = 1, c = 0.01),
                  start_exp_neg = list(a = 80, b= -74, c = -0.0002),
                  start_hof = list(a = 6, H = 1, m = 20, w = 0.1),
                  #start_sech_sym = list(a = 6, H = 1, m = 95, s = 60),
                  start_sech_asy = list(a = 6, H = 1, m = 95, s = 60, r = 0))

#dev.off()

fit_pH <- fit_functions(y = pH, x = urb,
                        start_linear = list(a = 6, b = 0.015), 
                        start_exp_pos = list(a = 5, b = 1, c = 0.01),
                        start_exp_neg = list(a = 80, b= -74, c = -0.0002),
                        start_hof = list(a = 6, H = 1, m = 20, w = 0.1),
                        #start_sech_sym = list(a = 6, H = 1, m = 95, s = 60),
                        start_sech_asy = list(a = 6, H = 1, m = 95, s = 60, r = 0))
                        #weights = pH_weights)

fit_pH$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = pH, x = urb,
                  start_linear = as.list(coef(fit_pH$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_pH$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_pH$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_pH$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_pH$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_pH$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultapH_pH <- run_all_models(fit_pH, y = pH,x = urb)

list_loglik <- resultapH_pH$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_pH.csv")


#################################################################


########### Predicting

coefs_sd <- coef(resultapH_pH$all_sd_models$Negative_Exponential$best_model[[1]])

new_x <- seq(0,100,0.1)

predicted_mean <- fit_pH$best_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] + new_x*coefs_sd[2])

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


########## Ploting #################

png("Resultados/Água/Curvas/pH.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(pH ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "pH", line = 2, cex.lab = 1.25)

#title(ylab = "Density of houses without", line = 2.5, cex.lab = 1.25)
#title(ylab = "sanitation (Houses/ha)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

points(pH ~ urb, pch = 16)

dev.off()

#####################################







