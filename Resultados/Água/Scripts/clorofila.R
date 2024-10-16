clorofila <- water_quality_agua_mean$chlorophyll_a
urb <- water_quality_agua_mean$X.urb
clorofila_weights <- weights_agua$chlorophyll_a

length(clorofila)
length(clorofila_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = clorofila, x = urb,
                  start_linear = list(a = 1, b = 0.07), 
                  start_exp_pos = list(a = 0, b = 1, c = 0.02),
                  start_exp_neg = list(a = 100, b= -100, c = -0.0008),
                  start_hof = list(a = 0, H = 8, m = 50, w = 0.04),
                  start_sech_sym = list(a = 1, H = 8, m = 95, s = 50),
                  start_sech_asy = list(a = 1, H = 8, m = 95, s = 50, r = 0))

#dev.off()

fit_clorofila <- fit_functions(y = clorofila, x = urb,
                               start_linear = list(a = 1, b = 0.07), 
                               start_exp_pos = list(a = 0, b = 1, c = 0.02),
                               start_exp_neg = list(a = 100, b= -100, c = -0.0008),
                               start_hof = list(a = 0, H = 8, m = 50, w = 0.04),
                               #start_sech_sym = list(a = 1, H = 8, m = 95, s = 50),
                               start_sech_asy = list(a = 1, H = 8, m = 95, s = 50, r = 0))
                                  # weights = clorofila_weights)

fit_clorofila$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = clorofila, x = urb,
                  start_linear = as.list(coef(fit_clorofila$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_clorofila$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_clorofila$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_clorofila$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_clorofila$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_clorofila$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_clorofila <- run_all_models(fit_clorofila, y = clorofila,x = urb)

list_loglik <- resultado_clorofila$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_clorofila.csv")

resultado_clorofila$all_sd_models$Negative_Exponential$all_models$Quadratic

#################################################################


########### Predicting

coefs_sd <- coef(resultado_clorofila$all_sd_models$Linear$best_model[[1]])

new_x <- seq(0,100,0.1)

predicted_mean <- fit_clorofila$best_predicted$Linear
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd



coefs_sd2 <- coef(resultado_clorofila$all_sd_models$Negative_Exponential$all_models$Quadratic)

new_x2 <- seq(0,100,0.1)

predicted_mean2 <- fit_clorofila$all_predicted$Negative_Exponential
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2+ coefs_sd2[3]*new_x2^2)

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2


########## Ploting




png("Resultados/Água/Curvas/Clorofila.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(clorofila ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Chlorophyll-a", line = 2, cex.lab = 1.25)

#title(ylab = "Density of houses without", line = 2.5, cex.lab = 1.25)
#title(ylab = "sanitation (Houses/ha)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")


points(clorofila ~ urb, pch = 16)

dev.off()



#####################################







