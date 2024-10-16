library(bbmle)
#install.packages("bbmle")

X2.4D <- water_quality_contaminantes$X2.4D
urb <- water_quality_contaminantes$X.urb[is.na(X2.4D) == FALSE]
X2.4D <- X2.4D[is.na(X2.4D) == FALSE]

length(X2.4D)

data.frame(urb,X2.4D )


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = X2.4D, x = urb,
                  start_linear = list(a = 100, b = -1), 
                  start_exp_pos = list(a = 100, b = -5, c = 0.03),
                  start_exp_neg = list(a = 0, b= 80, c = -0.05),
                  start_hof = list(a = 0, H = 100, m = 55, w = -0.5),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 1, H = 90, m = 5, s = 5,r = 0.8))

#dev.off()

fit_X2.4D <- fit_functions(y = X2.4D, x = urb,
                           start_linear = list(a = 100, b = -1), 
                           start_exp_pos = list(a = 100, b = -5, c = 0.03),
                           start_exp_neg = list(a = 0, b= 80, c = -0.05),
                           start_hof = list(a = 0, H = 100, m = 55, w = -0.5),
                           #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                           start_sech_asy = list(a = 1, H = 90, m = 5, s = 5,r = 0.8))
#weights = X2.4D_weights)

fit_X2.4D$aictab
fit_X2.4D$confint_mean

fit_X2.4D$coefs_mean


#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = X2.4D, x = urb,
                  start_linear = as.list(coef(fit_X2.4D$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_X2.4D$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_X2.4D$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_X2.4D$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_X2.4D$all_models$`SymetrX2.4D_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_X2.4D$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_X2.4D <- run_all_models(fit_X2.4D, y = X2.4D,x = urb)

list_loglik <- resultado_X2.4D$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Contaminantes/Seleção_de_Modelos/Result_X2.4D.csv")


#################################################################


########### PredX2.4Dting

coefs_sd <- coef(resultado_X2.4D$all_sd_models$Negative_Exponential$all_models$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_X2.4D$all_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


#coefs_sd2 <- coef(resultado_X2.4D$all_sd_models$Linear$all_models$Quadratic)

#new_x2 <- seq(0,100,0.1)

#predicted_mean2 <- fit_X2.4D$all_predicted$Linear
#predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2 + coefs_sd2[3]*new_x2^2)

#predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
#predicted_sd_lower2 <- predicted_mean2 - predicted_sd2

########## Ploting




png("resultados/Contaminantes/Curvas/X2.4D.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(X2.4D ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "2,4-D", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

#lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
#lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
#lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")


points(X2.4D ~ urb, pch = 16)

box()

dev.off()

#####################################
