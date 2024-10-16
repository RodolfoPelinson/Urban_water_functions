Citalopram <- water_quality_farmacos$q_Citalopram
urb <- water_quality_agua_mean$X.urb

length(Citalopram)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Citalopram, x = urb,
                  start_linear = list(a = 2, b = 0.2), 
                  start_exp_pos = list(a = -3, b = 10, c = 0.01),
                  start_exp_neg = list(a = 30, b= -30, c = -0.01),
                  start_hof = list(a = 0, H = 7, m = 55, w = 0.5),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 1, H = 15, m = 70, s = 50,r = 0.1))

#dev.off()

fit_Citalopram <- fit_functions(y = Citalopram, x = urb,
                                start_linear = list(a = 2, b = 0.2), 
                                start_exp_pos = list(a = -3, b = 10, c = 0.01),
                                start_exp_neg = list(a = 30, b= -30, c = -0.01),
                                start_hof = list(a = 0, H = 7, m = 55, w = 0.5),
                                #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                start_sech_asy = list(a = 1, H = 15, m = 70, s = 50,r = 0.1))
#weights = Citalopram_weights)

fit_Citalopram$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Citalopram, x = urb,
                  start_linear = as.list(coef(fit_Citalopram$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_Citalopram$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_Citalopram$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_Citalopram$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_Citalopram$all_models$`SymetrCitalopram_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_Citalopram$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_Citalopram <- run_all_models(fit_Citalopram, y = Citalopram,x = urb)

list_loglik <- resultado_Citalopram$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Fármacos/Seleção_de_Modelos/Result_Citalopram.csv")

#################################################################


########### PredCitalopramting

coefs_sd <- coef(resultado_Citalopram$all_sd_models$`HOF_(Sigmoid)`$all_models$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_Citalopram$all_predicted$`HOF_(Sigmoid)`
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


#coefs_sd2 <- coef(resultado_Citalopram$all_sd_models$Negative_Exponential$all_models$Quadratic)

#new_x2 <- seq(0,100,0.1)

#predicted_mean2 <- fit_Citalopram$all_predicted$Negative_Exponential
#predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2 + coefs_sd2[3]*new_x2^2)

#predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
#predicted_sd_lower2 <- predicted_mean2 - predicted_sd2

########## Ploting




png("resultados/Fármacos/Curvas/Citalopram.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(Citalopram ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Citalopram (ng/L)", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

#lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
#lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
#lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")


points(Citalopram ~ urb, pch = 16)

box()

dev.off()

#####################################
