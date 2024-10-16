library(bbmle)
#install.packages("bbmle")

Fipronil_Sulfona <- water_quality_contaminantes$Fipronil_Sulfona
urb <- water_quality_contaminantes$X.urb[is.na(Fipronil_Sulfona) == FALSE]
Fipronil_Sulfona <- Fipronil_Sulfona[is.na(Fipronil_Sulfona) == FALSE]

length(Fipronil_Sulfona)

data.frame(urb,Fipronil_Sulfona )


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Fipronil_Sulfona, x = urb,
                  start_linear = list(a = 2, b = -0.02), 
                  start_exp_pos = list(a = 3, b = -1, c = 0.01),
                  start_exp_neg = list(a = 0, b= 2, c = -0.05),
                  start_hof = list(a = 0, H = 2, m = 50, w = -0.5),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 8, m = 70, s = 5,r = -0.8))

#dev.off()

fit_Fipronil_Sulfona <- fit_functions(y = Fipronil_Sulfona, x = urb,
                                      start_linear = list(a = 2, b = -0.02), 
                                      start_exp_pos = list(a = 3, b = -1, c = 0.01),
                                      start_exp_neg = list(a = 0, b= 2, c = -0.05),
                                      start_hof = list(a = 0, H = 2, m = 50, w = -0.5),
                                      #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                                      start_sech_asy = list(a = 0, H = 8, m = 70, s = 5,r = -0.8))
#weights = Fipronil_Sulfona_weights)

fit_Fipronil_Sulfona$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Fipronil_Sulfona, x = urb,
                  start_linear = as.list(coef(fit_Fipronil_Sulfona$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_Fipronil_Sulfona$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_Fipronil_Sulfona$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_Fipronil_Sulfona$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_Fipronil_Sulfona$all_models$`SymetrFipronil_Sulfona_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_Fipronil_Sulfona$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()



#################################################################

resultado_Fipronil_Sulfona <- run_all_models(fit_Fipronil_Sulfona, y = Fipronil_Sulfona,x = urb)

list_loglik <- resultado_Fipronil_Sulfona$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)

write.csv(aictab, "Resultados/Contaminantes/Seleção_de_Modelos/Result_Fipronil_Sulfona.csv")


#################################################################


########### PredFipronil_Sulfonating

coefs_sd <- coef(resultado_Fipronil_Sulfona$all_sd_models$No_effect$best_model$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_Fipronil_Sulfona$all_predicted$No_effect
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


coefs_sd2 <- coef(resultado_Fipronil_Sulfona$all_sd_models$No_effect$all_models$Linear_Positive)

new_x2 <- seq(0,100,0.1)

predicted_mean2 <- fit_Fipronil_Sulfona$all_predicted$No_effect
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2) #+ coefs_sd2[3]*new_x2^2)

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2

########## Ploting




png("resultados/Contaminantes/Curvas/Fipronil_Sulfona.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(Fipronil_Sulfona ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Fipronil Sulfona", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")


points(Fipronil_Sulfona ~ urb, pch = 16)

box()

dev.off()

#####################################
