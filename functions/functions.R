##################################################################################
################################# BELL CURVES ####################################
##################################################################################

# a =  Valor base
# H =  Valor máximo do pico, excluindo "a", ou quando a = 0. (H>0  or  0<H<1  or  0<H<n)
# m =  Ponto no eixo x onde ocorrerá o pico (-Inf < m < Inf)
# s =  Largura da boca do "sino", semelhante ao desvio padrão em uma gaussiana (s > Inf)
# r =  Grau de assimetria (-1 < r < 1)
# p =  largura da parte de cima do "sino". Quão "fino" ele é. (p>0)


full_sech <- function(a, H, m, s, r, p, x){
  
  x_m <- (s/2) * log( (1+r) / (1-r)  )
  
  H_m <- exp( ( (r*p) / s) * x_m ) * (sqrt( 1-(r^2) )^p )
  
  sech <- function(x_2){
    2 / (exp(x_2) + exp(-x_2) )
  }
  
  a + (H/H_m) * exp( ( (r*p)/s ) * (x-(m-x_m)) ) * ( sech( ( (x - (m-x_m))/s ) )^p )
}


plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(full_sech(a = 100, H = 600, m = 40, s = 10, r = 0, p = 1, x), add = TRUE)


sech_symetrical_r0 <- function(a, H, m, s, p, x){
  
  x_m <- (s/2) * log( (1+0) / (1-0)  )
  
  H_m <- exp( ( (0*p) / s) * x_m ) * (sqrt( 1-(0^2) )^p )
  
  sech <- function(x_2){
    2 / (exp(x_2) + exp(-x_2) )
  }
  
  a + (H/H_m) * exp( ( (0*p)/s ) * (x-(m-x_m)) ) * ( sech( ( (x - (m-x_m))/s ) )^p )
}

plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(sech_symetrical_r0(a = 100, H = 600, m = 40, s = 10, p = 1, x), add = TRUE)


#Checagem
sech_symetrical_r0 <- function(a, H, m, s, x){
  
  sech <- function(x_2){
    2 / (exp(x_2) + exp(-x_2) )
  }
  
  a + H*( sech( (x - m)/s  ) )
}

plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(sech_symetrical_r0(a = 100, H = 600, m = 40, s = 10, x), add = TRUE)



sech_p_1 <- function(a, H, m, s, r, x){
  
  x_m <- (s/2) * log( (1+r) / (1-r)  )
  
  H_m <- exp( ( (r*1) / s) * x_m ) * (sqrt( 1-(r^2) )^1 )
  
  sech <- function(x_2){
    2 / (exp(x_2) + exp(-x_2) )
  }
  
  a + (H/H_m) * exp( ( (r*1)/s ) * (x-(m-x_m)) ) * ( sech( ( (x - (m-x_m))/s ) )^1 )
}


plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(sech_p_1(a = 100, H = 600, m = 40, s = 10, r = 0, x), add = TRUE)



sech_symetrical_p_1_r0 <- function(a, H, m, s, x){
  
  x_m <- (s/2) * log( (1+0) / (1-0)  )
  
  H_m <- exp( ( (0*1) / s) * x_m ) * (sqrt( 1-(0^2) )^1 )
  
  sech <- function(x_2){
    2 / (exp(x_2) + exp(-x_2) )
  }
  
  a + (H/H_m) * exp( ( (0*1)/s ) * (x-(m-x_m)) ) * ( sech( ( (x - (m-x_m))/s ) )^1 )
}


plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(sech_symetrical_p_1_r0(a = 100, H = 600, m = 40, s = 10,  x), add = TRUE)



##################################################################################
################################# SIGMOID CURVES #################################
##################################################################################

# a =  Valor base
# H =  Valor máximo da curva excluindo "a", ou quando a = 0. (H>0  or  0<H<1  or  0<H<n)
# m =  Ponto de inflexão da curva (-Inf < m < Inf)
# w =  raxa de mudança de um estágio para o outro (-Inf < w < Inf) (Talvez seja melhor entre 1 e -1)

hofII <- function(a, H, m, w, x){
  a + H*( 1 / (1 + exp( -w*(x-m) ) ) )
}


plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(hofII(a = 100, H = 600, m = 40, w  =  1, x), add = TRUE)

##################################################################################
################################# EXPONENCIAL ####################################
##################################################################################


expotential <- function(a, b, c, x){
  a + b*exp(c*x)
}

plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(expotential(a = 0, b = 1, c = 0.07, x), add = TRUE)
curve(expotential(a = 1000, b = -1, c = 0.08, x), add = TRUE)


curve(expotential(a = 0, b = 1000, c = -0.07, x), add = TRUE)
curve(expotential(a = 1000, b = -1000, c = -0.07, x), add = TRUE)



##################################################################################
################################# EXPONENCIAL ####################################
##################################################################################


linear <- function(a, b, x){
  a + b*x
}

plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(linear(a = 0, b = 10, x), add = TRUE)




##################################################################################
################################# SIGMOID CURVES #################################
##################################################################################

# a =  Valor base
# H =  Valor máximo da curva excluindo "a", ou quando a = 0. (H>0  or  0<H<1  or  0<H<n)
# m =  Ponto de inflexão da curva (-Inf < m < Inf)
# w =  raxa de mudança de um estágio para o outro (-Inf < w < Inf) (Talvez seja melhor entre 1 e -1)

hofV <- function(a, H, m, w1, w2, k, x){
  
  Hof <- ( 1 / (1 + exp( -w1*( x-(m - k) ) ) ) ) * ( 1 / (1 + exp( w2*( x-(m + k) ) ) ) )
  
  ## --- Find maximum H
  ## Lower limit of mode location
  x0 <- (m-k) - 2/(w1 + 0.01)
  ## Upper limit of mode location
  x1 <- (m+k) + 2/(w2 + 0.01)
  ## Range of x values
  xx <- c(seq(x0,x1, length.out=length(x)), m)
  ## Create component curves
  zz1 <- w1*(xx - (m - k))
  zz2 <- w2*(xx - (m + k))
  ## Calculate HOF
  Hof2 <- ( 1/(1 + exp(-zz1)) ) * ( 1/(1 + exp(zz2)) )
  ## H mode
  H_mode=max(Hof2)
  
  ## Calculate mean of HOF function
  a + (H/H_mode) * Hof
  
}


plot(NA, xlim = c(0,100), ylim = c(0,1000))
curve(hofV(a = 0, H = 400, m = 40, w1  =  0.1, w2 = 0.1, k = 50, x), add = TRUE)



##################################################################


quadratica <- function(a, b, c, x){
  exp(a + b*x + c*x^2)
}

plot(NA, xlim = c(-100,100), ylim = c(0,2000))
curve(quadratica(a = 6.5, b = -0.01, c = -0.0013, x), add = TRUE)

curve(quadratica(a = 6.5, b = -0.00001, c = 0.0003, x), add = TRUE)



