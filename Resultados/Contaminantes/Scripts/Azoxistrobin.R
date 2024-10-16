library(bbmle)
#install.packages("bbmle")

Azoxistrobin <- water_quality_contaminantes$Azoxistrobin
urb <- water_quality_contaminantes$X.urb[is.na(Azoxistrobin) == FALSE]
Azoxistrobin <- Azoxistrobin[is.na(Azoxistrobin) == FALSE]

length(Azoxistrobin)

data.frame(urb,Azoxistrobin )


#png("Conductivity_starting_values.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Azoxistrobin, x = urb,
                  start_linear = list(a = 2, b = 2), 
                  start_exp_pos = list(a = 0, b = 1, c = 0.06),
                  start_exp_neg = list(a = 100, b= -100, c = -0.5),
                  start_hof = list(a = 0, H = 100, m = 50, w = 0.5),
                  #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                  start_sech_asy = list(a = 0, H = 100, m = 50, s = 5,r = 0.999))

#dev.off()

fit_Azoxistrobin <- fit_functions(y = Azoxistrobin, x = urb,
                             start_linear = list(a = 2, b = 2), 
                             start_exp_pos = list(a = 0, b = 1, c = 0.06),
                             start_exp_neg = list(a = 100, b= -100, c = -0.5),
                             start_hof = list(a = 0, H = 100, m = 50, w = 0.5),
                             #start_sech_sym = list(a = 0, H = 600, m = 95, s = 50),
                             start_sech_asy = list(a = 0, H = 100, m = 50, s = 5,r = 0.999))
#weights = Azoxistrobin_weights)

fit_Azoxistrobin$aictab

#png("Conductivity_values_optmized.png", width = 10, height = 9, units = "cm", pointsize = 8, res = 600)

example_functions(y = Azoxistrobin, x = urb,
                  start_linear = as.list(coef(fit_Azoxistrobin$all_models$Linear)), 
                  start_exp_pos = as.list(coef(fit_Azoxistrobin$all_models$Positive_Exponential)),
                  start_exp_neg = as.list(coef(fit_Azoxistrobin$all_models$Negative_Exponential)),
                  start_hof = as.list(coef(fit_Azoxistrobin$all_models$`HOF_(Sigmoid)`)),
                  #start_sech_sym = as.list(coef(fit_Azoxistrobin$all_models$`SymetrAzoxistrobin_Bell-Shaped`)),
                  start_sech_asy = as.list(coef(fit_Azoxistrobin$all_models$`Asymetric_Bell-Shaped`)))

#dev.off()


#################################################################

#example_sd(fit_Azoxistrobin$best_model[[1]], urb,
#           cons_par = c(intercept = 5),
#           linear_pos_par = c(intercept = 0.3, slope = 0.02),
#linear_neg_par = c(intercept = 3, slope = -0.05),
#           linear_neg_par = c(intercept =  3.5952091, slope = -0.1459124 ),

#quadratic_par = c(intercept = 3, slope = 0.1, slope2 = -0.005))
#           quadratic_par = c(intercept = 0.460390537  , slope = 0.157093404 , slope2 = -0.007562701 ))

#Critério maximo para o intercepto para restringir os valores do desvio padrão para algo razoável
#max_intercep <- 2*sd(resid(fit_Azoxistrobin$best_model[[1]]))

resultado_Azoxistrobin <- run_all_models(fit_Azoxistrobin, y = Azoxistrobin,x = urb) #max_intercept_quad = max_intercep, max_intercept_lin_neg = max_intercep)

list_loglik <- resultado_Azoxistrobin$log_likehoods

aictab <- AICctab(list_loglik, base = TRUE)
aictab

write.csv(aictab, "Resultados/Contaminantes/Seleção_de_Modelos/Result_Azoxistrobin.csv")


#################################################################


########### PredAzoxistrobinting

coefs_sd <- coef(resultado_Azoxistrobin$all_sd_models$`HOF_(Sigmoid)`$all_models$Quadratic)

new_x <- seq(0,100,0.1)

predicted_mean <- fit_Azoxistrobin$all_predicted$`HOF_(Sigmoid)`
predicted_sd <- exp(coefs_sd[1] +  coefs_sd[2]*new_x + coefs_sd[3]*new_x^2)

predicted_sd_upper <- predicted_mean + predicted_sd
predicted_sd_lower <- predicted_mean - predicted_sd


coefs_sd2 <- coef(resultado_Azoxistrobin$all_sd_models$`HOF_(Sigmoid)`$all_models$Linear_Negative)

new_x2 <- seq(0,100,0.1)

predicted_mean2 <- fit_Azoxistrobin$all_predicted$`HOF_(Sigmoid)`
predicted_sd2 <- exp(coefs_sd2[1] +  coefs_sd2[2]*new_x2) #+ coefs_sd2[3]*new_x2^2)

predicted_sd_upper2 <- predicted_mean2 + predicted_sd2
predicted_sd_lower2 <- predicted_mean2 - predicted_sd2



#coefs_sd3 <- coef(resultado_Azoxistrobin$all_sd_models$Linear$all_models$Quadratic)

#new_x3 <- seq(0,100,0.1)

#predicted_mean3 <- fit_Azoxistrobin$all_predicted$Linear
#predicted_sd3 <- exp(coefs_sd3[1] +  coefs_sd3[2]*new_x3+ coefs_sd3[3]*new_x3^2)

#predicted_sd_upper3 <- predicted_mean3 + predicted_sd3
#predicted_sd_lower3 <- predicted_mean3 - predicted_sd3


########## Ploting




png("resultados/Contaminantes/Curvas/Azoxistrobin.png", width = 10, height = 9, units = "cm", pointsize = 10, res = 600)

par(mar = c(3.5,4,1,0.1), bty = "l", mfrow = c(1,1))

plot(Azoxistrobin ~ urb , type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(0,10))

title(xlab = "Urban cover (%)", line = 2, cex.lab = 1.25)
title(ylab = "Azoxistrobin", line = 2, cex.lab = 1.25)

#title(ylab = "Dissolved Oxygen", line = 2.5, cex.lab = 1.25)
#title(ylab = "(mg/L)", line = 1.5, cex.lab = 1.25)

axis(1, tick = TRUE, line = 0, labels = FALSE); axis(1, tick = FALSE, cex.axis = 1, line = -0.5)
axis(2, tick = TRUE, line = 0, labels = FALSE); axis(2, tick = FALSE, cex.axis = 1, line = -0.5)

polygon(x = c(seq(0,100,0.1),seq(100,0,-0.1)), y = c(predicted_sd_lower,predicted_sd_upper[1001:1] ), col = "grey80", border = FALSE)
lines(c(seq(0,100,0.1)), predicted_mean, lty = 1, lwd = 2)

lines(c(seq(0,100,0.1)), predicted_mean2, lty = 2, lwd = 2)
lines(c(seq(0,100,0.1)), predicted_sd_lower2, lty = 2, lwd = 2, col = "grey70")
lines(c(seq(0,100,0.1)), predicted_sd_upper2, lty = 2, lwd = 2, col = "grey70")

#lines(c(seq(0,100,0.1)), predicted_mean3, lty = 3, lwd = 2)
#lines(c(seq(0,100,0.1)), predicted_sd_lower3, lty = 3, lwd = 2, col = "grey70")
#lines(c(seq(0,100,0.1)), predicted_sd_upper3, lty = 3, lwd = 2, col = "grey70")

points(Azoxistrobin ~ urb, pch = 16)

box()

dev.off()

#####################################
