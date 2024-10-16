sarscov2_n2_mean2 <- sarscov2_n2_mean[-which(sarscov2_n2_mean$N2_copias_L == max(sarscov2_n2_mean$N2_copias_L)),]

sarscov2_n2 <- sarscov2_n2_mean2$N2_copias_L
urb <- as.numeric(sarscov2_n2_mean2$urb)

length(sarscov2_n2)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = sarscov2_n2, x = urb, use_defaut_start = FALSE,
                  start_linear = list(a = 0, b = 1000), 
                  start_exp_pos = list(a = 0, b = 200, c = 0.06),
                  start_exp_neg = list(a = 20000, b= -20000, c = -0.08),
                  start_hof = list(a = 50, H = 50000, m = 50, w = 0.06),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 50000, m = 95, s = 50,r = 0))

#dev.off()

fit_sarscov2_n2 <- fit_functions(y = sarscov2_n2, x = urb, use_defaut_start = FALSE, confint = TRUE,
                                 start_linear = list(a = 0, b = 1000), 
                                 start_exp_pos = list(a = 0, b = 200, c = 0.06),
                                 start_exp_neg = list(a = 20000, b= 200000, c = -0.08),
                                 start_hof = list(a = 50, H = 50000, m = 50, w = 0.06),
                                 #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                 start_sech_asy = list(a = 0, H = 50000, m = 95, s = 50,r = 0))
#weights = sarscov2_n2_weights)


sarscov2_n2_01 <- sarscov2_n2/1000


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = sarscov2_n2_01, x = urb, use_defaut_start = FALSE,
                  start_linear = list(a = 0, b = 1), 
                  start_exp_pos = list(a = 0, b = 1, c = 0.05),
                  start_exp_neg = list(a = 40, b= -40, c = -0.08),
                  start_hof = list(a = 0, H = 200, m = 50, w = 0.06),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 200, m = 95, s = 25,r = 0))

#dev.off()

fit_sarscov2_n2 <- fit_functions(y = sarscov2_n2_01, x = urb, use_defaut_start = FALSE, confint = TRUE,
                                 start_linear = list(a = 0, b = 1), 
                                 start_exp_pos = list(a = 0, b = 0.2, c = 0.03),
                                 start_exp_neg = list(a = 20, b= 20, c = -0.08),
                                 start_hof = list(a = 0, H = 50, m = 50, w = 0.06),
                                 #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                 start_sech_asy = list(a = 0, H = 50, m = 95, s = 50,r = 0))
#weights = sarscov2_n2_weights)


fit_sarscov2_n2$aictab
fit_sarscov2_n2$confint_mean
fit_sarscov2_n2$coefs_mean


#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = sarscov2_n2_01, x = urb,use_defaut_start = FALSE,
                  start_linear = as.list(coef(fit_sarscov2_n2$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_sarscov2_n2$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_sarscov2_n2$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_sarscov2_n2$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_sarscov2_n2$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_sarscov2_n2$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_sarscov2_n2 <- run_all_models(fit_sarscov2_n2, y = sarscov2_n2_01,x = urb)

list_loglik <- resultado_sarscov2_n2$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/SARSCOV2/Seleção_de_Modelos/Result_sarscov2_n2.csv")

resultado_sarscov2_n2$all_sd_models$Linear$all_models$Quadratic

#################################################################


########### Predicting

coefs_sd <- coef(resultado_sarscov2_n2$all_sd_models$`HOF_(Sigmoid)`$all_models$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_sarscov2_n2$all_predicted$`HOF_(Sigmoid)`
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd



coefs_sd2 <- coef(resultado_sarscov2_n2$all_sd_models$`HOF_(Sigmoid)`$all_models$Linear_Positive)

predicted_mean2 <- fit_sarscov2_n2$all_predicted$`HOF_(Sigmoid)`
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x)# + coefs_sd[3]*(new_x^2))

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2



coefs_sd3 <- coef(resultado_sarscov2_n2$all_sd_models$Linear$all_models$Quadratic)

predicted_mean3 <- fit_sarscov2_n2$all_predicted$Linear
predicted_sd3 <- exp(coefs_sd3[1] +  coefs_sd3[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper3 <- predicted_mean3 + predicted_sd3
predicted_sd_lower3 <- predicted_mean3 - predicted_sd3


########## Ploting




png("Resultados/SARSCOV2/Curvas/sarscov2_n2.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(sarscov2_n2_01 ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "SARS-Cov-2 N2 (copies/ml)", line = 2, cex.lab = 1.25)

#title(ylab = "Density of houses without", line = 2.5, cex.lab = 1.25)
#title(ylab = "sanitation (Houses/ha)", line = 1.5, cex.lab = 1.25)

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

points(sarscov2_n2_01 ~ urb, pch = 16)
box()

dev.off()
