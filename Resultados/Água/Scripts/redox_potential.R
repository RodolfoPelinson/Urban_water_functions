redox <- water_quality_agua_mean$redox_potential_.mV.
urb <- water_quality_agua_mean$X.urb
redox_weights <- weights_agua$turbidity_.NTU.

length(redox)
length(redox_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = redox, x = urb,
                  start_linear = list(a = 40, b = -0.8), 
                  start_exp_pos = list(a = 50, b = -25, c = 0.013),
                  start_exp_neg = list(a = -40, b= 50, c = -0.03),
                  start_hof = list(a = -20, H = 50, m = 20, w = -0.25),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = -20, H = 50, m = 5, s = 50,r = 0))

#dev.off()

fit_redox <- fit_functions(y = redox, x = urb,
                           start_linear = list(a = 40, b = -0.8), 
                           start_exp_pos = list(a = 50, b = -25, c = 0.013),
                           start_exp_neg = list(a = -40, b= 50, c = -0.03),
                           start_hof = list(a = -20, H = 50, m = 20, w = -0.25),
                           #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                           start_sech_asy = list(a = -20, H = 50, m = 5, s = 50,r = 0))
#weights = redox_weights)

fit_redox$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = redox, x = urb,
                  start_linear = as.list(coef(fit_redox$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_redox$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_redox$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_redox$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_redox$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_redox$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_redox <- run_all_models(fit_redox, y = redox,x = urb)

list_loglik <- resultado_redox$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_redox.csv")


#################################################################


########### Predicting

coefs_sd <- coef(resultado_redox$all_sd_models$Negative_Exponential$best_model$Linear_Negative)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_redox$all_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


########## Ploting




png("resultados/Água/Curvas/redox.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(redox ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Redox Potential (mV)", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

points(redox ~ urb, pch = 16)

box()

dev.off()

#####################################
