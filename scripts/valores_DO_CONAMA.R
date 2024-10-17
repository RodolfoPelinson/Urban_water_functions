urb_Values <- c(0, 5, 10, 15, 20)

do_Values <- c(6, 5, 4, 2)

coef <- coef(fit_do$best_model$Negative_Exponential) 

#Fazendo algebra pra isolar o x

y = a + b*exp(c*x)

x = ( ln( (y-a) / b) ) / c 

#######################
#Valores de urbanização para diferentes de O2
( log( (do_Values-coef[1]) / coef[2]) ) / coef[3] 


#Valores de o2 para diferentes de urbanização
coef[1] + coef[2]*exp(coef[3]*urb_Values)
