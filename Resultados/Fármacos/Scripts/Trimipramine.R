Trimipramine <- water_quality_farmacos$q_Trimipramine
urb <- water_quality_agua_mean$X.urb

length(Trimipramine)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Trimipramine, x = urb,
                  start_linear = list(a = 0, b = 0.2), 
                  start_exp_pos = list(a = 0, b = 1, c = 0.02),
                  start_exp_neg = list(a = 10, b= -10, c = -0.1),
                  start_hof = list(a = 0, H = 8, m = 43, w = 10),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 8, m = 45, s = 5,r = 0))

#dev.off()

fit_Trimipramine <- fit_functions(y = Trimipramine, x = urb,
                                  start_linear = list(a = 0, b = 0.2), 
                                  start_exp_pos = list(a = 0, b = 1, c = 0.02),
                                  start_exp_neg = list(a = 10, b= -10, c = -0.1),
                                  start_hof = list(a = 0, H = 8, m = 43, w = 10),
                                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                  start_sech_asy = list(a = 0, H = 8, m = 45, s = 5,r = 0))
#weights = Trimipramine_weights)

fit_Trimipramine$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Trimipramine, x = urb,
                  start_linear = as.list(coef(fit_Trimipramine$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_Trimipramine$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_Trimipramine$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_Trimipramine$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_Trimipramine$all_models$`SymetrTrimipramine_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_Trimipramine$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_Trimipramine <- run_all_models(fit_Trimipramine, y = Trimipramine,x = urb)

list_loglik <- resultado_Trimipramine$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Fármacos/Seleção_de_Modelos/Result_Trimipramine.csv")

#################################################################


########### PredTrimipramineting

coefs_sd <- coef(resultado_Trimipramine$all_sd_models$`HOF_(Sigmoid)`$all_models$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_Trimipramine$all_predicted$HOF_
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x+ coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


#coefs_sd2 <- coef(resultado_Trimipramine$all_sd_models$`Asymetric_Bell-Shaped`$all_models$Linear_Positive)

#new_x2 <- seq(0,100,0.1)

#predicted_mean2 <- fit_Trimipramine$all_predicted$`Asymetric_Bell-Shaped`
#predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2) #+ coefs_sd2[3]*new_x2^2)

#predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
#predicted_sd_lower2 <- predicted_mean2 - predicted_sd2

########## Ploting




png("resultados/Fármacos/Curvas/Trimipramine.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(Trimipramine ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Trimipramine (ng/L)", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

#lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
#lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
#lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")


points(Trimipramine ~ urb, pch = 16)

box()

dev.off()

#####################################
