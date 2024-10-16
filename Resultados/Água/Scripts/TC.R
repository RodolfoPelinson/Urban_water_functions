TC <- water_quality_agua_mean$TC
urb <- water_quality_agua_mean$X.urb
TC_weights <- weights_agua$turbidity_.NTU.

length(TC)
length(TC_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = TC, x = urb,
                  start_linear = list(a = 0, b = 0.8), 
                  start_exp_pos = list(a = 0, b = 10, c = 0.025),
                  start_exp_neg = list(a = 70, b= -70, c = -0.03),
                  start_hof = list(a = 0, H = 70, m = 30, w = 0.15),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 1, H = 70, m = 50, s = 50,r = 0.9))

#dev.off()

fit_TC <- fit_functions(y = TC, x = urb,
                        start_linear = list(a = 0, b = 0.8), 
                        start_exp_pos = list(a = 0, b = 10, c = 0.025),
                        start_exp_neg = list(a = 70, b= -70, c = -0.03),
                        start_hof = list(a = 0, H = 70, m = 30, w = 0.15),
                        #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                        start_sech_asy = list(a = 1, H = 70, m = 50, s = 50,r = 0.9))
#weights = TC_weights)

fit_TC$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = TC, x = urb,
                  start_linear = as.list(coef(fit_TC$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_TC$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_TC$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_TC$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_TC$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_TC$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_TC <- run_all_models(fit_TC, y = TC,x = urb)

list_loglik <- resultado_TC$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_TC.csv")


#################################################################


########### Predicting

coefs_sd <- coef(resultado_TC$all_sd_models$Negative_Exponential$best_model$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_TC$all_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


coefs_sd2 <- coef(resultado_TC$all_sd_models$`HOF_(Sigmoid)`$all_models$Quadratic)

new_x2 <- seq(0,100,0.1)

predicted_mean2 <- fit_TC$all_predicted$`HOF_(Sigmoid)`
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2 + coefs_sd2[3]*new_x2^2)

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2


########## Ploting




png("resultados/Água/Curvas/TC.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(TC ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "TC", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")


points(TC ~ urb, pch = 16)

box()

dev.off()

#####################################
