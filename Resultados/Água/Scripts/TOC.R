TOC <- water_quality_agua_mean$TOC
urb <- water_quality_agua_mean$X.urb
TOC_weights <- weights_agua$turbidity_.NTU.

length(TOC)
length(TOC_weights)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = TOC, x = urb,
                  start_linear = list(a = 0, b = 0.6), 
                  start_exp_pos = list(a = 0, b = 10, c = 0.02),
                  start_exp_neg = list(a = 50, b= -50, c = -0.03),
                  start_hof = list(a = 0, H = 50, m = 20, w = 8),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 1, H = 50, m = 50, s = 50,r = 0.8))

#dev.off()

fit_TOC <- fit_functions(y = TOC, x = urb,
                         start_linear = list(a = 0, b = 0.6), 
                         start_exp_pos = list(a = 0, b = 10, c = 0.02),
                         start_exp_neg = list(a = 50, b= -50, c = -0.03),
                         start_hof = list(a = 0, H = 50, m = 20, w = 8),
                         #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                         start_sech_asy = list(a = 1, H = 50, m = 50, s = 50,r = 0.8))

#weights = TOC_weights)

fit_TOC$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = TOC, x = urb,
                  start_linear = as.list(coef(fit_TOC$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_TOC$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_TOC$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_TOC$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_TOC$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_TOC$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_TOC <- run_all_models(fit_TOC, y = TOC,x = urb)

list_loglik <- resultado_TOC$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_TOC.csv")


#################################################################


########### Predicting

coefs_sd <- coef(resultado_TOC$all_sd_models$Negative_Exponential$best_model$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_TOC$all_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


########## Ploting




png("resultados/Água/Curvas/TOC.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(TOC ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "TOC", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

points(TOC ~ urb, pch = 16)

box()

dev.off()

#####################################




#####################################

TOC_stand <- vegan::decostand(TOC, method = "stand")

TOC_stand* attr(TOC_stand, "scaled:scale") + attr(TOC_stand, "scaled:center")

scale <- attr(TOC_stand, "scaled:scale")
center <- attr(TOC_stand, "scaled:center")


example_functions(y = TOC_stand, x = urb,
                  start_linear = list(a = (0-center)/scale, b = 0.6/scale), 
                  start_exp_pos = list(a = (0-center)/scale, b = 10/scale, c = 0.02),
                  start_exp_neg = list(a = (50-center)/scale, b= -50/scale, c = -0.03),
                  start_hof = list(a = (0-center)/scale, H = 50/scale, m = 20, w = 0.25),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = -1, H = 2, m = 50, s = 50,r = 0))
#dev.off()

fit_TOC_stand <- fit_functions(y = TOC_stand, x = urb,
                               start_linear = list(a = (0-center)/scale, b = 0.6/scale), 
                               start_exp_pos = list(a = (0-center)/scale, b = 10/scale, c = 0.02),
                               start_exp_neg = list(a = (50-center)/scale, b= -50/scale, c = -0.03),
                               start_hof = as.list(coef(fit_TOC$all_models$`HOF_(Sigmoid)`)),
                               #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                               start_sech_asy = list(a = -1, H = 2, m = 50, s = 50,r = 0))
#weights = TOC_weights)

fit_TOC_stand$aictab

fit_TOC$aictab


coef_stand <- coef(fit_TOC_stand$all_models$`Asymetric_Bell-Shaped`)
coef <- coef(fit_TOC$all_models$`Asymetric_Bell-Shaped`)



H <- coef[2]

a <- coef[1]

a_p <-  coef[1] +scale


H_p <- ( ( H + a - center) / scale ) - a_p


coef_stand


#png("TOC_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = TOC_stand, x = urb,
                  start_linear = as.list(coef(fit_TOC_stand$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_TOC_stand$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_TOC_stand$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_TOC_stand$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_TOC_stand$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_TOC_stand$all_models$`Asymetric_Bell-Shaped`)))

example_functions(y = TOC, x = urb,
                  start_linear = as.list(coef(fit_TOC$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_TOC$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_TOC$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_TOC$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_TOC$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_TOC$all_models$`Asymetric_Bell-Shaped`)))



coef(fit_TOC_stand$all_models$`HOF_(Sigmoid)`)
coef(fit_TOC$all_models$`HOF_(Sigmoid)`)



best_model <- fit_TOC_stand$best_model$Negative_Exponential

confint(best_model)

