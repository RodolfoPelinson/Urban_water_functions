do <- water_quality_agua_mean$DO_.mg.L.
urb <- water_quality_agua_mean$X.urb
do_weights <- weights_agua$DO_.mg.L.

length(do)
length(do_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = urb,
                  start_linear = list(a = 5, b = -0.05), 
                  start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                  start_exp_neg = list(a = 1, b= 6, c = -0.1),
                  start_hof = list(a = 1, H = 6, m = 55, w = -0.2),
                  #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                  start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5))

#dev.off()

fit_do <- fit_functions(y = do, x = urb,
                                   start_linear = list(a = 5, b = -0.05), 
                                   start_exp_pos = list(a = 4, b = -0.1, c = 0.04),
                                   start_exp_neg = list(a = 1, b= 6, c = -0.1),
                                   start_hof = list(a = 1, H = 6, m = 55, w = -0.2),
                                   #start_sech_sym = list(a = 1, H = 6, m = 50, s = 50),
                                   start_sech_asy = list(a = 1, H = 6, m = 5, s = 50, r = 0.5))
                                   #weights = do_weights)

fit_do$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = do, x = urb,
                  start_linear = as.list(coef(fit_do$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_do$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_do$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_do$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_do$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_do$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_do <- run_all_models(fit_do, y = do,x = urb)

list_loglik <- resultado_do$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_DO.csv")


#################################################################


########### Predicting

coefs_sd <- coef(resultado_do$all_sd_models$Negative_Exponential$best_model[[1]])

new_x <- seq(0,100,0.1)

predicted_mean <- fit_do$best_predicted$Negative_Exponential
predicted_sd <- coefs_sd[1]

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


coefs_sd2 <- coef(resultado_do$all_sd_models$Negative_Exponential$all_models$Linear_Negative)

new_x2 <- seq(0,100,0.1)

predicted_mean2 <- fit_do$all_predicted$Negative_Exponential
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2)

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2



coefs_sd3 <- coef(resultado_do$all_sd_models$Negative_Exponential$all_models$Quadratic)

new_x3 <- seq(0,100,0.1)

predicted_mean3 <- fit_do$all_predicted$Negative_Exponential
predicted_sd3 <- exp(coefs_sd3[1] +  coefs_sd3[2]*new_x3+ coefs_sd3[3]*new_x3^2)

predicted_sd_upper3 <- predicted_mean3 + predicted_sd3
predicted_sd_lower3 <- predicted_mean3 - predicted_sd3

########## Ploting




png("Resultados/Água/Curvas/DO.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(do ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Dissolved Oxygen (mg/L)", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")

lines(c(seq(0,100,0.1)), predicted_mean3, lty = 3, lwd = 2)
lines(c(seq(0,100,0.1)), predicted_sd_lower3, lty = 3, lwd = 2, col = "grey70")
lines(c(seq(0,100,0.1)), predicted_sd_upper3, lty = 3, lwd = 2, col = "grey70")

points(do ~ urb, pch = 16)

dev.off()

#####################################







