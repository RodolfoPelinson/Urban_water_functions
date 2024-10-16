library(bbmle)
#install.packages("bbmle")

Ametryn <- water_quality_contaminantes$Ametryn
urb <- water_quality_contaminantes$X.urb[is.na(Ametryn) == FALSE]
Ametryn <- Ametryn[is.na(Ametryn) == FALSE]

length(Ametryn)

data.frame(urb,Ametryn )


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Ametryn, x = urb,
                  start_linear = list(a = 2, b = 2), 
                  start_exp_pos = list(a = 0, b = 1, c = 0.06),
                  start_exp_neg = list(a = 100, b= -100, c = -0.5),
                  start_hof = list(a = 0, H = 100, m = 50, w = 0.5),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 100, m = 50, s = 5,r = 0.999))

#dev.off()

fit_Ametryn <- fit_functions(y = Ametryn, x = urb,
                             start_linear = list(a = 2, b = 2), 
                             start_exp_pos = list(a = 0, b = 1, c = 0.06),
                             start_exp_neg = list(a = 100, b= -100, c = -0.5),
                             start_hof = list(a = 0, H = 100, m = 50, w = 0.5),
                             #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                             start_sech_asy = list(a = 0, H = 100, m = 50, s = 5,r = 0.999))
#weights = Ametryn_weights)

fit_Ametryn$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Ametryn, x = urb,
                  start_linear = as.list(coef(fit_Ametryn$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_Ametryn$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_Ametryn$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_Ametryn$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_Ametryn$all_models$`SymetrAmetryn_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_Ametryn$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()


#################################################################

#example_sd(fit_Ametryn$best_model[[1]], urb,
#           cons_par = c(intercept = 5),
#           linear_pos_par = c(intercept = 0.3, slope = 0.02),
#linear_neg_par = c(intercept = 3, slope = -0.05),
#           linear_neg_par = c(intercept =  3.5952091, slope = -0.1459124 ),

#quadratic_par = c(intercept = 3, slope = 0.1, slope2 = -0.005))
#           quadratic_par = c(intercept = 0.460390537  , slope = 0.157093404 , slope2 = -0.007562701 ))

#Critério maximo para o intercepto para restringir os valores do desvio padrão para algo razoável
#max_intercep <- 2*sd(resid(fit_Ametryn$best_model[[1]]))

resultado_Ametryn <- run_all_models(fit_Ametryn, y = Ametryn,x = urb) #max_intercept_quad = max_intercep, max_intercept_lin_neg = max_intercep)

list_loglik <- resultado_Ametryn$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)
aictab

write.csv(aictab, "Resultados/Contaminantes/Seleção_de_Modelos/Result_Ametryn.csv")


#################################################################


########### PredAmetrynting

coefs_sd <- coef(resultado_Ametryn$all_sd_models$Negative_Exponential$all_models$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_Ametryn$all_predicted$Negative_Exponential
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


#coefs_sd2 <- coef(resultado_Ametryn$all_sd_models$Positive_Exponential$all_models$Linear_Positive)

#new_x2 <- seq(0,100,0.1)

#predicted_mean2 <- fit_Ametryn$all_predicted$Positive_Exponential
#predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2) #+ coefs_sd2[3]*new_x2^2)

#predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
#predicted_sd_lower2 <- predicted_mean2 - predicted_sd2



#coefs_sd3 <- coef(resultado_Ametryn$all_sd_models$Linear$all_models$Quadratic)

#new_x3 <- seq(0,100,0.1)

#predicted_mean3 <- fit_Ametryn$all_predicted$Linear
#predicted_sd3 <- exp(coefs_sd3[1] +  coefs_sd3[2]*new_x3+ coefs_sd3[3]*new_x3^2)

#predicted_sd_upper3 <- predicted_mean3 + predicted_sd3
#predicted_sd_lower3 <- predicted_mean3 - predicted_sd3


########## Ploting




png("resultados/Contaminantes/Curvas/Ametryn.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(Ametryn ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Ametryn", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

#lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
#lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
#lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")

#lines(c(seq(0,100,0.1)), predicted_mean3, lty = 3, lwd = 2)
#lines(c(seq(0,100,0.1)), predicted_sd_lower3, lty = 3, lwd = 2, col = "grey70")
#lines(c(seq(0,100,0.1)), predicted_sd_upper3, lty = 3, lwd = 2, col = "grey70")

points(Ametryn ~ urb, pch = 16)

box()

dev.off()

#####################################
