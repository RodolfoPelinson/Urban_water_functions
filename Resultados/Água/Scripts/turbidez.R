turbidity <- water_quality_agua_mean$turbidity_.NTU.
urb <- water_quality_agua_mean$X.urb
turbidity_weights <- weights_agua$Conductivity_.uS.cm.

length(turbidity)
length(turbidity_weights)

urb <- urb[turbidity <= quantile(water_quality_agua_mean$turbidity_.NTU., probs = c(0.95))*3]
turbidity <- turbidity[turbidity <= quantile(water_quality_agua_mean$turbidity_.NTU., probs = c(0.95))*3]

length(turbidity)
length(urb)


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = turbidity, x = urb,
                  start_linear = list(a = 0, b = 1), 
                  start_exp_pos = list(a = 0, b = 1, c = 0.05),
                  start_exp_neg = list(a = 85, b= -85, c = -0.01),
                  start_hof = list(a = 0, H = 80, m = 50, w = 0.04),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 90, m = 75, s = 20,r = 0.9))

#dev.off()

fit_turbidity <- fit_functions(y = turbidity, x = urb, use_defaut_start = FALSE,
                               start_linear = list(a = 0, b = 1), 
                               start_exp_pos = list(a = 0, b = 1, c = 0.05),
                               start_exp_neg = list(a = 85, b= -85, c = -0.01),
                               start_hof = list(a = 0, H = 80, m = 50, w = 0.04),
                               #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                               start_sech_asy = list(a = 0, H = 90, m = 75, s = 20,r = 0.9))
#weights = turbidity_weights)

mod_exp_neg <- nls(turbidity ~ expotential(a, b, c, x = urb),
                   algorithm = "port",
                   start = list(a = 85, b= -85, c = -0.01),
                   lower = c(a = -Inf, b = -Inf, c = -Inf),
                   upper = c(a = Inf, b = Inf, c = Inf),
                   control = list(warnOnly = TRUE))#, ...)

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = turbidity, x = urb,
                  start_linear = as.list(coef(fit_turbidity$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_turbidity$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_turbidity$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_turbidity$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_turbidity$all_models$`Symetric_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_turbidity$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_turbidity <- run_all_models(fit_turbidity, y = turbidity,x = urb)

list_loglik <- resultado_turbidity$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Água/Seleção_de_Modelos/Result_turbidity.csv")

resultado_turbidity$all_sd_models$Linear$all_models$Quadratic

#################################################################


########### Predicting

coefs_sd <- coef(resultado_turbidity$all_sd_models$Linear$all_models$Constant)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_turbidity$all_predicted$Linear
predicted_sd <- coefs_sd[1]#(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*(new_x^2))

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd



coefs_sd2 <- coef(resultado_turbidity$all_sd_models$Linear$all_models$Linear_Negative)

new_x2 <- seq(0,100,0.1)

predicted_mean2 <- fit_turbidity$all_predicted$Linear
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2)

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2



coefs_sd3 <- coef(resultado_turbidity$all_sd_models$Linear$all_models$Quadratic)

new_x3 <- seq(0,100,0.1)

predicted_mean3 <- fit_turbidity$all_predicted$Linear
predicted_sd3 <- exp(coefs_sd3[1] +  coefs_sd3[2]*new_x3+ coefs_sd3[3]*new_x3^2)

predicted_sd_upper3 <- predicted_mean3 + predicted_sd3
predicted_sd_lower3 <- predicted_mean3 - predicted_sd3


########## Ploting




png("Resultados/Água/Curvas/turbidity.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(turbidity ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Turbidity (NTU)", line = 2, cex.lab = 1.25)

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

points(turbidity ~ urb, pch = 16)

box()

dev.off()



#####################################







