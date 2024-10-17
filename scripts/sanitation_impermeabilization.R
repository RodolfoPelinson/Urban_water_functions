D_sanitation <- map_biomas_dados$habitantes_sem_saneamento/100
imperm <- map_biomas_dados$ArImp_2021_ha_Vect
library(vegan)
sanitation <- c(decostand(D_sanitation, method = "stand"))
imperm <- c(decostand(imperm, method = "stand"))

Ys <- list(
  do = do,
  pH = pH,
  redox = redox,
  turbidity = turbidity,
  Temperatura = Temperatura,
  TOC = TOC,
  TC = TC,
  SPC = SPC,
  TN = TN,
  IC = IC,
  condutividade = condutividade,
  descarga = descarga,
  clorofila = clorofila,
  phycocianin = phycocianin,
  Carbendazin = Carbendazin,
  Paracetamol = Paracetamol,
  Caffeine = Caffeine,
  Diuron = Diuron,
  Atrazin = Atrazin,
  SNRI = SNRI,
  n_pharma = n_pharma,
  all = all,
  Tricyclic = Tricyclic,
  SSRI = SSRI,
  Aminoketone = Aminoketone,
  sarscov2_n2 = sarscov2_n2/1000,
  sarscov2_n1 = sarscov2_n1/1000
)

#starts = list(
#  start_do = list(a = 1, b= 1, c = -1.5),
#  start_pH = list(a = 1, b= 1, c = -1.5),
#  start_redox_potential = list(a = 1, b= 1, c = -1.5),
#  start_turbidity = list(a = 1, b= 1, c = -1.5),
#  start_temperature = list(a = 1, b= 1, c = -1.5),
#  start_TOC = list(a = 1, b= 1, c = -1.5),
#  start_TC = list(a = 1, b= 1, c = -1.5),
#  start_SPC = list(a = 1, b= 1, c = -1.5),
#  start_TN = list(a = 1, b= 1, c = -1.5),
#  start_IC = list(a = 1, b= 1, c = -1.5),
#  start_conductivity = list(a = 1, b= 1, c = -1.5),
#  start_discharge = list(a = 1, b= 1, c = -1.5),
#  start_chlorophyll = list(a = 1, b= 1, c = -1.5),
#  start_phycocianin = list(a = 1, b= 1, c = -1.5),
#  start_carbendazin = list(a = 1, b= 1, c = -1.5),
#  start_paracetamol = list(a = 1, b= 1, c = -1.5),
#  start_caffeine = list(a = 1, b= 1, c = -1.5),
#  start_diuron = list(a = 1, b= 1, c = -1.5),
#  start_atrazin = list(a = 1, b= 1, c = -1.5),
#  start_SNRI = list(a = 1, b= 1, c = -1.5),
#  start_Npharmaceuticals = list(a = 1, b= 1, c = -1.5),
#  start_all_antidepressants = list(a = 1, b= 1, c = -1.5),
#  start_tricyclic = list(a = 1, b= 1, c = -1.5),
#  start_SSRI = list(a = 1, b= 1, c = -1.5),
#  start_aminoketone = list(a = 1, b= 1, c = -1.5),
#  start_sarscov_n2 = list(a = 1, b= 1, c = -1.5),
#  start_sarscov_n1 = list(a = 1, b= 1, c = -1.5)
#)

lengths <- rep(NA, length(starts))
for(i in 1:length(starts)){
  lengths[[i]] <- length(Ys[[i]])
}

imperm_list <- list()
for(i in 1:length(starts)){
  imperm_list[[i]] <- imperm
}

sanitation_list <- list()
for(i in 1:length(starts)){
  sanitation_list[[i]] <- sanitation
}

turbidity2 <-water_quality_agua_mean$turbidity_.NTU.
imperm_list[[which(names(Ys) == "turbidity")]] <- imperm[turbidity2 <= quantile(water_quality_agua_mean$turbidity_.NTU., probs = c(0.95))*3]
sanitation_list[[which(names(Ys) == "turbidity")]] <- sanitation[turbidity2 <= quantile(water_quality_agua_mean$turbidity_.NTU., probs = c(0.95))*3]

Carbendazin2 <- water_quality_contaminantes$Carbendazin
imperm_list[[which(names(Ys) == "Carbendazin")]] <- imperm[is.na(Carbendazin2) == FALSE]
sanitation_list[[which(names(Ys) == "Carbendazin")]] <- sanitation[is.na(Carbendazin2) == FALSE]

Paracetamol2 <- water_quality_contaminantes$Paracetamol
imperm_list[[which(names(Ys) == "Paracetamol")]] <- imperm[is.na(Paracetamol2) == FALSE]
sanitation_list[[which(names(Ys) == "Paracetamol")]] <- sanitation[is.na(Paracetamol2) == FALSE]

Caffeine2 <- water_quality_contaminantes$Caffeine
imperm_list[[which(names(Ys) == "Caffeine")]] <- imperm[is.na(Caffeine2) == FALSE]
sanitation_list[[which(names(Ys) == "Caffeine")]] <- sanitation[is.na(Caffeine2) == FALSE]

Diuron2 <- water_quality_contaminantes$Diuron
imperm_list[[which(names(Ys) == "Diuron")]] <- imperm[is.na(Diuron2) == FALSE]
sanitation_list[[which(names(Ys) == "Diuron")]] <- sanitation[is.na(Diuron2) == FALSE]

Atrazin2 <- water_quality_contaminantes$Atrazin
imperm_list[[which(names(Ys) == "Atrazin")]] <- imperm[is.na(Atrazin2) == FALSE]
sanitation_list[[which(names(Ys) == "Atrazin")]] <- sanitation[is.na(Atrazin2) == FALSE]


sarscov2_n12 <- sarscov2_n1_mean2$N1_copias_L/1000
sarscov2_n22 <- sarscov2_n2_mean2$N2_copias_L/1000
imperm_sarscov <-  map_biomas_dados$ArImp_2021_ha_Vect[match(sarscov2_n1_mean2$Group.1, map_biomas_dados$ID_geral)]
imperm_list[[which(names(Ys) == "sarscov2_n2")]] <- c(decostand(imperm_sarscov[is.na(sarscov2_n22) == FALSE], method = "stand"))
imperm_list[[which(names(Ys) == "sarscov2_n1")]] <- c(decostand(imperm_sarscov[is.na(sarscov2_n12) == FALSE], method = "stand"))

sanitation_sarscov <-  map_biomas_dados$habitantes_sem_saneamento[match(sarscov2_n1_mean2$Group.1, map_biomas_dados$ID_geral)]/100
sanitation_list[[which(names(Ys) == "sarscov2_n2")]] <- c(decostand(sanitation_sarscov[is.na(sarscov2_n22) == FALSE], method = "stand"))
sanitation_list[[which(names(Ys) == "sarscov2_n1")]] <- c(decostand(sanitation_sarscov[is.na(sarscov2_n12) == FALSE], method = "stand"))

names(imperm_list) <- names(Ys)
names(sanitation_list) <- names(Ys)

  
lengths_y <- rep(NA, length(starts))
for(i in 1:length(starts)){
  lengths_y[[i]] <- length(Ys[[i]])
}

lengths_imperm <- rep(NA, length(starts))
for(i in 1:length(starts)){
  lengths_imperm[[i]] <- length(imperm_list[[i]])
}



c_imperm <- list()
c_sanitation <- list()

r2_imperm <- list()
r2_sanitation <- list()


###############################################################
###############################################################
#########################    do   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$do, x = imperm_list$do,
                  start_exp_neg = list(a = 1, b= 1, c = -2))

fit_do_imperm <- fit_neg_exponential(y = Ys$do, x = imperm_list$do,
                                     start_exp_neg = list(a = 1, b= 1, c = -2))

example_neg_exp(y = Ys$do, x = imperm_list$do,
                start_exp_neg = as.list(coef(fit_do_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$do, x = sanitation_list$do,
                start_exp_neg = list(a = 1, b= 1, c = -2))

fit_do_sanitation <- fit_neg_exponential(y = Ys$do, x = sanitation_list$do,
                                     start_exp_neg = list(a = 1, b= 1, c = -2))

example_neg_exp(y = Ys$do, x = sanitation_list$do,
                start_exp_neg = as.list(coef(fit_do_sanitation$all_models$Negative_Exponential)))


lista_preditores_do <- list(sanitation = fit_do_sanitation$best_model$Negative_Exponential,
                         imperm = fit_do_imperm$best_model$Negative_Exponential)

AICctab(lista_preditores_do, base = TRUE)

deviance(fit_do_imperm$best_model$Negative_Exponential)
deviance(fit_do_sanitation$best_model$Negative_Exponential)

logLik(fit_do_imperm$best_model$Negative_Exponential)
logLik(fit_do_sanitation$best_model$Negative_Exponential)

library(MuMIn)


c_imperm$do <- fit_do_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$do <- fit_do_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$do <- c(r.squaredLR(fit_do_imperm$best_model$Negative_Exponential, null = fit_do_imperm$all_models$No_effect))
r2_sanitation$do <- c(r.squaredLR(fit_do_sanitation$best_model$Negative_Exponential, null = fit_do_sanitation$all_models$No_effect))



###############################################################
###############################################################
#########################    pH   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$pH, x = imperm_list$pH,
                start_exp_neg = list(a = 7.3, b= -0.4, c = -1))

fit_pH_imperm <- fit_neg_exponential(y = Ys$pH, x = imperm_list$pH,
                                     start_exp_neg = list(a = 7.3, b= -0.4, c = -1))

example_neg_exp(y = Ys$pH, x = imperm_list$pH,
                start_exp_neg = as.list(coef(fit_pH_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$pH, x = sanitation_list$pH,
                start_exp_neg = list(a = 7.3, b= -0.4, c = -1))

fit_pH_sanitation <- fit_neg_exponential(y = Ys$pH, x = sanitation_list$pH,
                                         start_exp_neg = list(a = 7.3, b= -0.4, c = -1))

example_neg_exp(y = Ys$pH, x = sanitation_list$pH,
                start_exp_neg = as.list(coef(fit_pH_sanitation$all_models$Negative_Exponential)))


lista_preditores_pH <- list(sanitation = fit_pH_sanitation$best_model$Negative_Exponential,
                            imperm = fit_pH_imperm$best_model$Negative_Exponential)

AICctab(lista_preditores_pH, base = TRUE)

c_imperm$pH <- fit_pH_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$pH <- fit_pH_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$pH <- c(r.squaredLR(fit_pH_imperm$best_model$Negative_Exponential, null = fit_pH_imperm$all_models$No_effect))
r2_sanitation$pH <- c(r.squaredLR(fit_pH_sanitation$best_model$Negative_Exponential, null = fit_pH_sanitation$all_models$No_effect))



###############################################################
###############################################################
#########################    redox   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$redox, x = imperm_list$redox,
                start_exp_neg = list(a = -35, b= 2.5, c = -3.5))

fit_redox_imperm <- fit_neg_exponential(y = Ys$redox, x = imperm_list$redox,
                                        start_exp_neg = list(a = -35, b= 2.5, c = -3.5))

example_neg_exp(y = Ys$redox, x = imperm_list$redox,
                start_exp_neg = as.list(coef(fit_redox_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$redox, x = sanitation_list$redox,
                start_exp_neg = list(a = -35, b= 2.5, c = -3.5))

fit_redox_sanitation <- fit_neg_exponential(y = Ys$redox, x = sanitation_list$redox,
                                            start_exp_neg = list(a = -35, b= 2.5, c = -3.5))

example_neg_exp(y = Ys$redox, x = sanitation_list$redox,
                start_exp_neg = as.list(coef(fit_redox_sanitation$all_models$Negative_Exponential)))


lista_preditores_redox <- list(sanitation = fit_redox_sanitation$best_model$Negative_Exponential,
                               imperm = fit_redox_imperm$best_model$Negative_Exponential)

AICctab(lista_preditores_redox, base = TRUE)

c_imperm$redox <- fit_redox_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$redox <- fit_redox_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$redox <- c(r.squaredLR(fit_redox_imperm$best_model$Negative_Exponential, null = fit_redox_imperm$all_models$No_effect))
r2_sanitation$redox <- c(r.squaredLR(fit_redox_sanitation$best_model$Negative_Exponential, null = fit_redox_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    turbidity   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$turbidity, x = imperm_list$turbidity,
                start_exp_neg = list(a = 400, b= -350, c = -0.1))

fit_turbidity_imperm <- fit_neg_exponential(y = Ys$turbidity, x = imperm_list$turbidity,
                                            start_exp_neg = list(a = 400, b= -350, c = -0.1))

example_neg_exp(y = Ys$turbidity, x = imperm_list$turbidity,
                start_exp_neg = as.list(coef(fit_turbidity_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$turbidity, x = sanitation_list$turbidity,
                start_exp_neg = list(a = 400, b= -350, c = -0.1))

fit_turbidity_sanitation <- fit_neg_exponential(y = Ys$turbidity, x = sanitation_list$turbidity,
                                                start_exp_neg = list(a = 400, b= -350, c = -0.1))

example_neg_exp(y = Ys$turbidity, x = sanitation_list$turbidity,
                start_exp_neg = as.list(coef(fit_turbidity_sanitation$all_models$Negative_Exponential)))


lista_preditores_turbidity <- list(sanitation = fit_turbidity_sanitation$best_model$Negative_Exponential,
                               imperm = fit_turbidity_imperm$best_model$Negative_Exponential,
                               null = fit_turbidity_sanitation$all_models$No_effect)

AICctab(lista_preditores_turbidity, base = TRUE)

c_imperm$turbidity <- fit_turbidity_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$turbidity <- fit_turbidity_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$turbidity <- c(r.squaredLR(fit_turbidity_imperm$best_model$Negative_Exponential, null = fit_turbidity_imperm$all_models$No_effect))
r2_sanitation$turbidity <- c(r.squaredLR(fit_turbidity_sanitation$best_model$Negative_Exponential, null = fit_turbidity_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    Temperatura   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Temperatura, x = imperm_list$Temperatura,
                  start_exp_neg = list(a = 20, b= -1, c = -2))

fit_Temperatura_imperm <- fit_neg_exponential(y = Ys$Temperatura, x = imperm_list$Temperatura,
                                              start_exp_neg = list(a = 20, b= -1, c = -2))

example_neg_exp(y = Ys$Temperatura, x = imperm_list$Temperatura,
                start_exp_neg = as.list(coef(fit_Temperatura_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Temperatura, x = sanitation_list$Temperatura,
                start_exp_neg = list(a = 20, b= -1, c = -2))

fit_Temperatura_sanitation <- fit_neg_exponential(y = Ys$Temperatura, x = sanitation_list$Temperatura,
                                                  start_exp_neg = list(a = 20, b= -1, c = -2))

example_neg_exp(y = Ys$Temperatura, x = sanitation_list$Temperatura,
                start_exp_neg = as.list(coef(fit_Temperatura_sanitation$all_models$Negative_Exponential)))


lista_preditores_Temperatura <- list(sanitation = fit_Temperatura_sanitation$best_model$Negative_Exponential,
                                   imperm = fit_Temperatura_imperm$best_model$Negative_Exponential,
                                   null = fit_Temperatura_sanitation$all_models$No_effect)

AICctab(lista_preditores_Temperatura, base = TRUE)

c_imperm$Temperatura <- fit_Temperatura_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Temperatura <- fit_Temperatura_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Temperatura <- c(r.squaredLR(fit_Temperatura_imperm$best_model$Negative_Exponential, null = fit_Temperatura_imperm$all_models$No_effect))
r2_sanitation$Temperatura <- c(r.squaredLR(fit_Temperatura_sanitation$best_model$Negative_Exponential, null = fit_Temperatura_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    TOC   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$TOC, x = imperm_list$TOC,
                start_exp_neg = list(a = 200, b= -180, c = -0.1))

fit_TOC_imperm <- fit_neg_exponential(y = Ys$TOC, x = imperm_list$TOC,
                                      start_exp_neg = list(a = 200, b= -180, c = -0.1))

example_neg_exp(y = Ys$TOC, x = imperm_list$TOC,
                start_exp_neg = as.list(coef(fit_TOC_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$TOC, x = sanitation_list$TOC,
                start_exp_neg = list(a = 200, b= -180, c = -0.1))

fit_TOC_sanitation <- fit_neg_exponential(y = Ys$TOC, x = sanitation_list$TOC,
                                          start_exp_neg = list(a = 200, b= -180, c = -0.1))

example_neg_exp(y = Ys$TOC, x = sanitation_list$TOC,
                start_exp_neg = as.list(coef(fit_TOC_sanitation$all_models$Negative_Exponential)))


lista_preditores_TOC <- list(sanitation = fit_TOC_sanitation$best_model$Negative_Exponential,
                                     imperm = fit_TOC_imperm$best_model$Negative_Exponential,
                                     null = fit_TOC_sanitation$all_models$No_effect)

AICctab(lista_preditores_TOC, base = TRUE)

c_imperm$TOC <- fit_TOC_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$TOC <- fit_TOC_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$TOC <- c(r.squaredLR(fit_TOC_imperm$best_model$Negative_Exponential, null = fit_TOC_imperm$all_models$No_effect))
r2_sanitation$TOC <- c(r.squaredLR(fit_TOC_sanitation$best_model$Negative_Exponential, null = fit_TOC_sanitation$all_models$No_effect))






###############################################################
###############################################################
#########################    TC   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$TC, x = imperm_list$TC,
                start_exp_neg = list(a = 200, b= -180, c = -0.1))

fit_TC_imperm <- fit_neg_exponential(y = Ys$TC, x = imperm_list$TC,
                                      start_exp_neg = list(a = 200, b= -180, c = -0.1))

example_neg_exp(y = Ys$TC, x = imperm_list$TC,
                start_exp_neg = as.list(coef(fit_TC_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$TC, x = sanitation_list$TC,
                start_exp_neg = list(a = 200, b= -180, c = -0.1))

fit_TC_sanitation <- fit_neg_exponential(y = Ys$TC, x = sanitation_list$TC,
                                          start_exp_neg = list(a = 200, b= -180, c = -0.1))

example_neg_exp(y = Ys$TC, x = sanitation_list$TC,
                start_exp_neg = as.list(coef(fit_TC_sanitation$all_models$Negative_Exponential)))


lista_preditores_TC <- list(sanitation = fit_TC_sanitation$best_model$Negative_Exponential,
                             imperm = fit_TC_imperm$best_model$Negative_Exponential,
                             null = fit_TC_sanitation$all_models$No_effect)

AICctab(lista_preditores_TC, base = TRUE)

c_imperm$TC <- fit_TC_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$TC <- fit_TC_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$TC <- c(r.squaredLR(fit_TC_imperm$best_model$Negative_Exponential, null = fit_TC_imperm$all_models$No_effect))
r2_sanitation$TC <- c(r.squaredLR(fit_TC_sanitation$best_model$Negative_Exponential, null = fit_TC_sanitation$all_models$No_effect))




###############################################################
###############################################################
#########################    SPC   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$SPC, x = imperm_list$SPC,
                start_exp_neg = list(a = 600, b= -200, c = -1))

fit_SPC_imperm <- fit_neg_exponential(y = Ys$SPC, x = imperm_list$SPC,
                                      start_exp_neg = list(a = 600, b= -200, c = -1))

example_neg_exp(y = Ys$SPC, x = imperm_list$SPC,
                start_exp_neg = as.list(coef(fit_SPC_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$SPC, x = sanitation_list$SPC,
                start_exp_neg = list(a = 600, b= -200, c = -1))

fit_SPC_sanitation <- fit_neg_exponential(y = Ys$SPC, x = sanitation_list$SPC,
                                          start_exp_neg = list(a = 600, b= -200, c = -1))

example_neg_exp(y = Ys$SPC, x = sanitation_list$SPC,
                start_exp_neg = as.list(coef(fit_SPC_sanitation$all_models$Negative_Exponential)))


lista_preditores_SPC <- list(sanitation = fit_SPC_sanitation$best_model$Negative_Exponential,
                            imperm = fit_SPC_imperm$best_model$Negative_Exponential,
                            null = fit_SPC_sanitation$all_models$No_effect)

AICctab(lista_preditores_SPC, base = TRUE)

c_imperm$SPC <- fit_SPC_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$SPC <- fit_SPC_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$SPC <- c(r.squaredLR(fit_SPC_imperm$best_model$Negative_Exponential, null = fit_SPC_imperm$all_models$No_effect))
r2_sanitation$SPC <- c(r.squaredLR(fit_SPC_sanitation$best_model$Negative_Exponential, null = fit_SPC_sanitation$all_models$No_effect))








###############################################################
###############################################################
#########################    TN   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$TN, x = imperm_list$TN,
                start_exp_neg = list(a = 30, b= -10, c = -1))

fit_TN_imperm <- fit_neg_exponential(y = Ys$TN, x = imperm_list$TN,
                                     start_exp_neg = list(a = 30, b= -10, c = -1))

example_neg_exp(y = Ys$TN, x = imperm_list$TN,
                start_exp_neg = as.list(coef(fit_TN_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$TN, x = sanitation_list$TN,
                start_exp_neg = list(a = 30, b= -10, c = -1))

fit_TN_sanitation <- fit_neg_exponential(y = Ys$TN, x = sanitation_list$TN,
                                         start_exp_neg = list(a = 30, b= -10, c = -1))

example_neg_exp(y = Ys$TN, x = sanitation_list$TN,
                start_exp_neg = as.list(coef(fit_TN_sanitation$all_models$Negative_Exponential)))


lista_preditores_TN <- list(sanitation = fit_TN_sanitation$best_model$Negative_Exponential,
                             imperm = fit_TN_imperm$best_model$Negative_Exponential,
                             null = fit_TN_sanitation$all_models$No_effect)

AICctab(lista_preditores_TN, base = TRUE)

c_imperm$TN <- fit_TN_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$TN <- fit_TN_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$TN <- c(r.squaredLR(fit_TN_imperm$best_model$Negative_Exponential, null = fit_TN_imperm$all_models$No_effect))
r2_sanitation$TN <- c(r.squaredLR(fit_TN_sanitation$best_model$Negative_Exponential, null = fit_TN_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    IC   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$IC, x = imperm_list$IC,
                start_exp_neg = list(a = 30, b= -10, c = -1))

fit_IC_imperm <- fit_neg_exponential(y = Ys$IC, x = imperm_list$IC,
                                     start_exp_neg = list(a = 30, b= -10, c = -1))

example_neg_exp(y = Ys$IC, x = imperm_list$IC,
                start_exp_neg = as.list(coef(fit_IC_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$IC, x = sanitation_list$IC,
                start_exp_neg = list(a = 30, b= -10, c = -1))

fit_IC_sanitation <- fit_neg_exponential(y = Ys$IC, x = sanitation_list$IC,
                                         start_exp_neg = list(a = 30, b= -10, c = -1))

example_neg_exp(y = Ys$IC, x = sanitation_list$IC,
                start_exp_neg = as.list(coef(fit_IC_sanitation$all_models$Negative_Exponential)))


lista_preditores_IC <- list(sanitation = fit_IC_sanitation$best_model$Negative_Exponential,
                            imperm = fit_IC_imperm$best_model$Negative_Exponential,
                            null = fit_IC_sanitation$all_models$No_effect)

AICctab(lista_preditores_IC, base = TRUE)

c_imperm$IC <- fit_IC_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$IC <- fit_IC_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$IC <- c(r.squaredLR(fit_IC_imperm$best_model$Negative_Exponential, null = fit_IC_imperm$all_models$No_effect))
r2_sanitation$IC <- c(r.squaredLR(fit_IC_sanitation$best_model$Negative_Exponential, null = fit_IC_sanitation$all_models$No_effect))




###############################################################
###############################################################
#########################    condutividade   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$condutividade, x = imperm_list$condutividade,
                start_exp_neg = list(a = 600, b= -180, c = -1))

fit_condutividade_imperm <- fit_neg_exponential(y = Ys$condutividade, x = imperm_list$condutividade,
                                                start_exp_neg = list(a = 600, b= -180, c = -1))

example_neg_exp(y = Ys$condutividade, x = imperm_list$condutividade,
                start_exp_neg = as.list(coef(fit_condutividade_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$condutividade, x = sanitation_list$condutividade,
                start_exp_neg = list(a = 600, b= -180, c = -1))

fit_condutividade_sanitation <- fit_neg_exponential(y = Ys$condutividade, x = sanitation_list$condutividade,
                                                    start_exp_neg = list(a = 600, b= -180, c = -1))

example_neg_exp(y = Ys$condutividade, x = sanitation_list$condutividade,
                start_exp_neg = as.list(coef(fit_condutividade_sanitation$all_models$Negative_Exponential)))


lista_preditores_condutividade <- list(sanitation = fit_condutividade_sanitation$best_model$Negative_Exponential,
                            imperm = fit_condutividade_imperm$best_model$Negative_Exponential,
                            null = fit_condutividade_sanitation$all_models$No_effect)

AICctab(lista_preditores_condutividade, base = TRUE)

c_imperm$condutividade <- fit_condutividade_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$condutividade <- fit_condutividade_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$condutividade <- c(r.squaredLR(fit_condutividade_imperm$best_model$Negative_Exponential, null = fit_condutividade_imperm$all_models$No_effect))
r2_sanitation$condutividade <- c(r.squaredLR(fit_condutividade_sanitation$best_model$Negative_Exponential, null = fit_condutividade_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    descarga   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$descarga, x = imperm_list$descarga,
                start_exp_neg = list(a = 50, b= -20, c = -1))

fit_descarga_imperm <- fit_neg_exponential(y = Ys$descarga, x = imperm_list$descarga,
                                           start_exp_neg = list(a = 50, b= -20, c = -1))

example_neg_exp(y = Ys$descarga, x = imperm_list$descarga,
                start_exp_neg = as.list(coef(fit_descarga_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$descarga, x = sanitation_list$descarga,
                start_exp_neg = list(a = 50, b= -20, c = -1))

fit_descarga_sanitation <- fit_neg_exponential(y = Ys$descarga, x = sanitation_list$descarga,
                                               start_exp_neg = list(a = 50, b= -20, c = -1))

example_neg_exp(y = Ys$descarga, x = sanitation_list$descarga,
                start_exp_neg = as.list(coef(fit_descarga_sanitation$all_models$Negative_Exponential)))


lista_preditores_descarga <- list(sanitation = fit_descarga_sanitation$best_model$Negative_Exponential,
                                       imperm = fit_descarga_imperm$best_model$Negative_Exponential,
                                       null = fit_descarga_sanitation$all_models$No_effect)

AICctab(lista_preditores_descarga, base = TRUE)

c_imperm$descarga <- fit_descarga_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$descarga <- fit_descarga_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$descarga <- c(r.squaredLR(fit_descarga_imperm$best_model$Negative_Exponential, null = fit_descarga_imperm$all_models$No_effect))
r2_sanitation$descarga <- c(r.squaredLR(fit_descarga_sanitation$best_model$Negative_Exponential, null = fit_descarga_sanitation$all_models$No_effect))






###############################################################
###############################################################
#########################    clorofila   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$clorofila, x = imperm_list$clorofila,
                start_exp_neg = list(a = 23, b= -20, c = -0.1))

fit_clorofila_imperm <- fit_neg_exponential(y = Ys$clorofila, x = imperm_list$clorofila,
                                            start_exp_neg = list(a = 23, b= -20, c = -0.1))

example_neg_exp(y = Ys$clorofila, x = imperm_list$clorofila,
                start_exp_neg = as.list(coef(fit_clorofila_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$clorofila, x = sanitation_list$clorofila,
                start_exp_neg = list(a = 23, b= -20, c = -0.1))

fit_clorofila_sanitation <- fit_neg_exponential(y = Ys$clorofila, x = sanitation_list$clorofila,
                                                start_exp_neg = list(a = 23, b= -20, c = -0.1))

example_neg_exp(y = Ys$clorofila, x = sanitation_list$clorofila,
                start_exp_neg = as.list(coef(fit_clorofila_sanitation$all_models$Negative_Exponential)))


lista_preditores_clorofila <- list(sanitation = fit_clorofila_sanitation$best_model$Negative_Exponential,
                                  imperm = fit_clorofila_imperm$best_model$Negative_Exponential,
                                  null = fit_clorofila_sanitation$all_models$No_effect)

AICctab(lista_preditores_clorofila, base = TRUE)

c_imperm$clorofila <- fit_clorofila_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$clorofila <- fit_clorofila_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$clorofila <- c(r.squaredLR(fit_clorofila_imperm$best_model$Negative_Exponential, null = fit_clorofila_imperm$all_models$No_effect))
r2_sanitation$clorofila <- c(r.squaredLR(fit_clorofila_sanitation$best_model$Negative_Exponential, null = fit_clorofila_sanitation$all_models$No_effect))







###############################################################
###############################################################
#########################    phycocianin   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$phycocianin, x = imperm_list$phycocianin,
                start_exp_neg = list(a = 100, b= -80, c = -0.1))

fit_phycocianin_imperm <- fit_neg_exponential(y = Ys$phycocianin, x = imperm_list$phycocianin,
                                              start_exp_neg = list(a = 100, b= -80, c = -0.1))

example_neg_exp(y = Ys$phycocianin, x = imperm_list$phycocianin,
                start_exp_neg = as.list(coef(fit_phycocianin_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$phycocianin, x = sanitation_list$phycocianin,
                start_exp_neg = list(a = 100, b= -80, c = -0.1))

fit_phycocianin_sanitation <- fit_neg_exponential(y = Ys$phycocianin, x = sanitation_list$phycocianin,
                                                  start_exp_neg = list(a = 100, b= -80, c = -0.1))

example_neg_exp(y = Ys$phycocianin, x = sanitation_list$phycocianin,
                start_exp_neg = as.list(coef(fit_phycocianin_sanitation$all_models$Negative_Exponential)))


lista_preditores_phycocianin <- list(sanitation = fit_phycocianin_sanitation$best_model$Negative_Exponential,
                                   imperm = fit_phycocianin_imperm$best_model$Negative_Exponential,
                                   null = fit_phycocianin_sanitation$all_models$No_effect)

AICctab(lista_preditores_phycocianin, base = TRUE)

c_imperm$phycocianin <- fit_phycocianin_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$phycocianin <- fit_phycocianin_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$phycocianin <- c(r.squaredLR(fit_phycocianin_imperm$best_model$Negative_Exponential, null = fit_phycocianin_imperm$all_models$No_effect))
r2_sanitation$phycocianin <- c(r.squaredLR(fit_phycocianin_sanitation$best_model$Negative_Exponential, null = fit_phycocianin_sanitation$all_models$No_effect))



###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################


###############################################################
###############################################################
#########################    Carbendazin   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Carbendazin, x = imperm_list$Carbendazin,
                start_exp_neg = list(a = 100, b= -80, c = -0.1))

fit_Carbendazin_imperm <- fit_neg_exponential(y = Ys$Carbendazin, x = imperm_list$Carbendazin,
                                              start_exp_neg = list(a = 100, b= -80, c = -0.1))

example_neg_exp(y = Ys$Carbendazin, x = imperm_list$Carbendazin,
                start_exp_neg = as.list(coef(fit_Carbendazin_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Carbendazin, x = sanitation_list$Carbendazin,
                start_exp_neg = list(a = 100, b= -80, c = -0.1))

fit_Carbendazin_sanitation <- fit_neg_exponential(y = Ys$Carbendazin, x = sanitation_list$Carbendazin,
                                                  start_exp_neg = list(a = 100, b= -80, c = -0.1))

example_neg_exp(y = Ys$Carbendazin, x = sanitation_list$Carbendazin,
                start_exp_neg = as.list(coef(fit_Carbendazin_sanitation$all_models$Negative_Exponential)))


lista_preditores_Carbendazin <- list(sanitation = fit_Carbendazin_sanitation$best_model$Negative_Exponential,
                                     imperm = fit_Carbendazin_imperm$best_model$Negative_Exponential,
                                     null = fit_Carbendazin_sanitation$all_models$No_effect)

AICctab(lista_preditores_Carbendazin, base = TRUE)

c_imperm$Carbendazin <- fit_Carbendazin_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Carbendazin <- fit_Carbendazin_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Carbendazin <- c(r.squaredLR(fit_Carbendazin_imperm$best_model$Negative_Exponential, null = fit_Carbendazin_imperm$all_models$No_effect))
r2_sanitation$Carbendazin <- c(r.squaredLR(fit_Carbendazin_sanitation$best_model$Negative_Exponential, null = fit_Carbendazin_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    Paracetamol   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Paracetamol, x = imperm_list$Paracetamol,
                start_exp_neg = list(a = 200, b= -80, c = -1))

fit_Paracetamol_imperm <- fit_neg_exponential(y = Ys$Paracetamol, x = imperm_list$Paracetamol,
                                              start_exp_neg = list(a = 200, b= -80, c = -1))

example_neg_exp(y = Ys$Paracetamol, x = imperm_list$Paracetamol,
                start_exp_neg = as.list(coef(fit_Paracetamol_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Paracetamol, x = sanitation_list$Paracetamol,
                start_exp_neg = list(a = 200, b= -80, c = -1))

fit_Paracetamol_sanitation <- fit_neg_exponential(y = Ys$Paracetamol, x = sanitation_list$Paracetamol,
                                                  start_exp_neg = list(a = 200, b= -80, c = -1))

example_neg_exp(y = Ys$Paracetamol, x = sanitation_list$Paracetamol,
                start_exp_neg = as.list(coef(fit_Paracetamol_sanitation$all_models$Negative_Exponential)))


lista_preditores_Paracetamol <- list(sanitation = fit_Paracetamol_sanitation$best_model$Negative_Exponential,
                                     imperm = fit_Paracetamol_imperm$best_model$Negative_Exponential,
                                     null = fit_Paracetamol_sanitation$all_models$No_effect)

AICctab(lista_preditores_Paracetamol, base = TRUE)

c_imperm$Paracetamol <- fit_Paracetamol_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Paracetamol <- fit_Paracetamol_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Paracetamol <- c(r.squaredLR(fit_Paracetamol_imperm$best_model$Negative_Exponential, null = fit_Paracetamol_imperm$all_models$No_effect))
r2_sanitation$Paracetamol <- c(r.squaredLR(fit_Paracetamol_sanitation$best_model$Negative_Exponential, null = fit_Paracetamol_sanitation$all_models$No_effect))






###############################################################
###############################################################
#########################    Caffeine   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Caffeine, x = imperm_list$Caffeine,
                start_exp_neg = list(a = 200, b= -80, c = -1))

fit_Caffeine_imperm <- fit_neg_exponential(y = Ys$Caffeine, x = imperm_list$Caffeine,
                                              start_exp_neg = list(a = 200, b= -80, c = -1))

example_neg_exp(y = Ys$Caffeine, x = imperm_list$Caffeine,
                start_exp_neg = as.list(coef(fit_Caffeine_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Caffeine, x = sanitation_list$Caffeine,
                start_exp_neg = list(a = 200, b= -80, c = -1))

fit_Caffeine_sanitation <- fit_neg_exponential(y = Ys$Caffeine, x = sanitation_list$Caffeine,
                                                  start_exp_neg = list(a = 200, b= -80, c = -1))

example_neg_exp(y = Ys$Caffeine, x = sanitation_list$Caffeine,
                start_exp_neg = as.list(coef(fit_Caffeine_sanitation$all_models$Negative_Exponential)))


lista_preditores_Caffeine <- list(sanitation = fit_Caffeine_sanitation$best_model$Negative_Exponential,
                                     imperm = fit_Caffeine_imperm$best_model$Negative_Exponential,
                                     null = fit_Caffeine_sanitation$all_models$No_effect)

AICctab(lista_preditores_Caffeine, base = TRUE)

c_imperm$Caffeine <- fit_Caffeine_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Caffeine <- fit_Caffeine_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Caffeine <- c(r.squaredLR(fit_Caffeine_imperm$best_model$Negative_Exponential, null = fit_Caffeine_imperm$all_models$No_effect))
r2_sanitation$Caffeine <- c(r.squaredLR(fit_Caffeine_sanitation$best_model$Negative_Exponential, null = fit_Caffeine_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    Diuron   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Diuron, x = imperm_list$Diuron,
                start_exp_neg = list(a = 100, b= -80, c = -0.1))

fit_Diuron_imperm <- fit_neg_exponential(y = Ys$Diuron, x = imperm_list$Diuron,
                                         start_exp_neg = list(a = 100, b= -80, c = -0.1))

example_neg_exp(y = Ys$Diuron, x = imperm_list$Diuron,
                start_exp_neg = as.list(coef(fit_Diuron_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Diuron, x = sanitation_list$Diuron,
                start_exp_neg = list(a = 100, b= -80, c = -0.1))

fit_Diuron_sanitation <- fit_neg_exponential(y = Ys$Diuron, x = sanitation_list$Diuron,
                                             start_exp_neg = list(a = 100, b= -80, c = -0.1))

example_neg_exp(y = Ys$Diuron, x = sanitation_list$Diuron,
                start_exp_neg = as.list(coef(fit_Diuron_sanitation$all_models$Negative_Exponential)))


lista_preditores_Diuron <- list(sanitation = fit_Diuron_sanitation$best_model$Negative_Exponential,
                                  imperm = fit_Diuron_imperm$best_model$Negative_Exponential,
                                  null = fit_Diuron_sanitation$all_models$No_effect)

AICctab(lista_preditores_Diuron, base = TRUE)

c_imperm$Diuron <- fit_Diuron_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Diuron <- fit_Diuron_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Diuron <- c(r.squaredLR(fit_Diuron_imperm$best_model$Negative_Exponential, null = fit_Diuron_imperm$all_models$No_effect))
r2_sanitation$Diuron <- c(r.squaredLR(fit_Diuron_sanitation$best_model$Negative_Exponential, null = fit_Diuron_sanitation$all_models$No_effect))





###############################################################
###############################################################
#########################    Atrazin   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Atrazin, x = imperm_list$Atrazin,
                start_exp_neg = list(a = 20, b= -15, c = -0.1))

fit_Atrazin_imperm <- fit_neg_exponential(y = Ys$Atrazin, x = imperm_list$Atrazin,
                                          start_exp_neg = list(a = 20, b= -15, c = -0.1))

example_neg_exp(y = Ys$Atrazin, x = imperm_list$Atrazin,
                start_exp_neg = as.list(coef(fit_Atrazin_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Atrazin, x = sanitation_list$Atrazin,
                start_exp_neg = list(a = 20, b= -15, c = -0.1))

fit_Atrazin_sanitation <- fit_neg_exponential(y = Ys$Atrazin, x = sanitation_list$Atrazin,
                                              start_exp_neg = list(a = 20, b= -15, c = -0.1))

example_neg_exp(y = Ys$Atrazin, x = sanitation_list$Atrazin,
                start_exp_neg = as.list(coef(fit_Atrazin_sanitation$all_models$Negative_Exponential)))


lista_preditores_Atrazin <- list(sanitation = fit_Atrazin_sanitation$best_model$Negative_Exponential,
                                imperm = fit_Atrazin_imperm$best_model$Negative_Exponential,
                                null = fit_Atrazin_sanitation$all_models$No_effect)

AICctab(lista_preditores_Atrazin, base = TRUE)

c_imperm$Atrazin <- fit_Atrazin_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Atrazin <- fit_Atrazin_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Atrazin <- c(r.squaredLR(fit_Atrazin_imperm$best_model$Negative_Exponential, null = fit_Atrazin_imperm$all_models$No_effect))
r2_sanitation$Atrazin <- c(r.squaredLR(fit_Atrazin_sanitation$best_model$Negative_Exponential, null = fit_Atrazin_sanitation$all_models$No_effect))



###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################




###############################################################
###############################################################
#########################    SNRI   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$SNRI, x = imperm_list$SNRI,
                start_exp_neg = list(a = 200, b= -100, c = -0.1))

fit_SNRI_imperm <- fit_neg_exponential(y = Ys$SNRI, x = imperm_list$SNRI,
                                       start_exp_neg = list(a = 200, b= -100, c = -0.1))

example_neg_exp(y = Ys$SNRI, x = imperm_list$SNRI,
                start_exp_neg = as.list(coef(fit_SNRI_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$SNRI, x = sanitation_list$SNRI,
                start_exp_neg = list(a = 200, b= -100, c = -0.1))

fit_SNRI_sanitation <- fit_neg_exponential(y = Ys$SNRI, x = sanitation_list$SNRI,
                                           start_exp_neg = list(a = 200, b= -100, c = -0.1))

example_neg_exp(y = Ys$SNRI, x = sanitation_list$SNRI,
                start_exp_neg = as.list(coef(fit_SNRI_sanitation$all_models$Negative_Exponential)))


lista_preditores_SNRI <- list(sanitation = fit_SNRI_sanitation$best_model$Negative_Exponential,
                                 imperm = fit_SNRI_imperm$best_model$Negative_Exponential,
                                 null = fit_SNRI_sanitation$all_models$No_effect)

AICctab(lista_preditores_SNRI, base = TRUE)

c_imperm$SNRI <- fit_SNRI_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$SNRI <- fit_SNRI_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$SNRI <- c(r.squaredLR(fit_SNRI_imperm$best_model$Negative_Exponential, null = fit_SNRI_imperm$all_models$No_effect))
r2_sanitation$SNRI <- c(r.squaredLR(fit_SNRI_sanitation$best_model$Negative_Exponential, null = fit_SNRI_sanitation$all_models$No_effect))







###############################################################
###############################################################
#########################    n_pharma   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$n_pharma, x = imperm_list$n_pharma,
                start_exp_neg = list(a = 25, b= -10, c = -1))

fit_n_pharma_imperm <- fit_neg_exponential(y = Ys$n_pharma, x = imperm_list$n_pharma,
                                           start_exp_neg = list(a = 25, b= -10, c = -1))

example_neg_exp(y = Ys$n_pharma, x = imperm_list$n_pharma,
                start_exp_neg = as.list(coef(fit_n_pharma_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$n_pharma, x = sanitation_list$n_pharma,
                start_exp_neg = list(a = 25, b= -10, c = -1))

fit_n_pharma_sanitation <- fit_neg_exponential(y = Ys$n_pharma, x = sanitation_list$n_pharma,
                                               start_exp_neg = list(a = 25, b= -10, c = -1))

example_neg_exp(y = Ys$n_pharma, x = sanitation_list$n_pharma,
                start_exp_neg = as.list(coef(fit_n_pharma_sanitation$all_models$Negative_Exponential)))


lista_preditores_n_pharma <- list(sanitation = fit_n_pharma_sanitation$best_model$Negative_Exponential,
                              imperm = fit_n_pharma_imperm$best_model$Negative_Exponential,
                              null = fit_n_pharma_sanitation$all_models$No_effect)

AICctab(lista_preditores_n_pharma, base = TRUE)

c_imperm$n_pharma <- fit_n_pharma_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$n_pharma <- fit_n_pharma_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$n_pharma <- c(r.squaredLR(fit_n_pharma_imperm$best_model$Negative_Exponential, null = fit_n_pharma_imperm$all_models$No_effect))
r2_sanitation$n_pharma <- c(r.squaredLR(fit_n_pharma_sanitation$best_model$Negative_Exponential, null = fit_n_pharma_sanitation$all_models$No_effect))






###############################################################
###############################################################
#########################    all   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$all, x = imperm_list$all,
                start_exp_neg = list(a = 300, b= -100, c = -1))

fit_all_imperm <- fit_neg_exponential(y = Ys$all, x = imperm_list$all,
                                      start_exp_neg = list(a = 300, b= -100, c = -1))

example_neg_exp(y = Ys$all, x = imperm_list$all,
                start_exp_neg = as.list(coef(fit_all_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$all, x = sanitation_list$all,
                start_exp_neg = list(a = 300, b= -100, c = -1))

fit_all_sanitation <- fit_neg_exponential(y = Ys$all, x = sanitation_list$all,
                                          start_exp_neg = list(a = 300, b= -100, c = -1))

example_neg_exp(y = Ys$all, x = sanitation_list$all,
                start_exp_neg = as.list(coef(fit_all_sanitation$all_models$Negative_Exponential)))


lista_preditores_all <- list(sanitation = fit_all_sanitation$best_model$Negative_Exponential,
                                  imperm = fit_all_imperm$best_model$Negative_Exponential,
                                  null = fit_all_sanitation$all_models$No_effect)

AICctab(lista_preditores_all, base = TRUE)

c_imperm$all <- fit_all_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$all <- fit_all_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$all <- c(r.squaredLR(fit_all_imperm$best_model$Negative_Exponential, null = fit_all_imperm$all_models$No_effect))
r2_sanitation$all <- c(r.squaredLR(fit_all_sanitation$best_model$Negative_Exponential, null = fit_all_sanitation$all_models$No_effect))




###############################################################
###############################################################
#########################    Tricyclic   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Tricyclic, x = imperm_list$Tricyclic,
                start_exp_neg = list(a = 50, b= -10, c = -0.1))

fit_Tricyclic_imperm <- fit_neg_exponential(y = Ys$Tricyclic, x = imperm_list$Tricyclic,
                                            start_exp_neg = list(a = 50, b= -10, c = -0.1))

example_neg_exp(y = Ys$Tricyclic, x = imperm_list$Tricyclic,
                start_exp_neg = as.list(coef(fit_Tricyclic_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Tricyclic, x = sanitation_list$Tricyclic,
                start_exp_neg = list(a = 50, b= -10, c = -0.1))

fit_Tricyclic_sanitation <- fit_neg_exponential(y = Ys$Tricyclic, x = sanitation_list$Tricyclic,
                                                start_exp_neg = list(a = 50, b= -10, c = -0.1))

example_neg_exp(y = Ys$Tricyclic, x = sanitation_list$Tricyclic,
                start_exp_neg = as.list(coef(fit_Tricyclic_sanitation$all_models$Negative_Exponential)))


lista_preditores_Tricyclic <- list(sanitation = fit_Tricyclic_sanitation$best_model$Negative_Exponential,
                             imperm = fit_Tricyclic_imperm$best_model$Negative_Exponential,
                             null = fit_Tricyclic_sanitation$all_models$No_effect)

AICctab(lista_preditores_Tricyclic, base = TRUE)

c_imperm$Tricyclic <- fit_Tricyclic_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Tricyclic <- fit_Tricyclic_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Tricyclic <- c(r.squaredLR(fit_Tricyclic_imperm$best_model$Negative_Exponential, null = fit_Tricyclic_imperm$all_models$No_effect))
r2_sanitation$Tricyclic <- c(r.squaredLR(fit_Tricyclic_sanitation$best_model$Negative_Exponential, null = fit_Tricyclic_sanitation$all_models$No_effect))






###############################################################
###############################################################
#########################    SSRI   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$SSRI, x = imperm_list$SSRI,
                start_exp_neg = list(a = 50, b= -10, c = -0.1))

fit_SSRI_imperm <- fit_neg_exponential(y = Ys$SSRI, x = imperm_list$SSRI,
                                            start_exp_neg = list(a = 50, b= -10, c = -0.1))

example_neg_exp(y = Ys$SSRI, x = imperm_list$SSRI,
                start_exp_neg = as.list(coef(fit_SSRI_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$SSRI, x = sanitation_list$SSRI,
                start_exp_neg = list(a = 180, b= -80, c = -0.5))

fit_SSRI_sanitation <- fit_neg_exponential(y = Ys$SSRI, x = sanitation_list$SSRI,
                                           start_exp_neg = list(a = 180, b= -80, c = -0.5))

example_neg_exp(y = Ys$SSRI, x = sanitation_list$SSRI,
                start_exp_neg = as.list(coef(fit_SSRI_sanitation$all_models$Negative_Exponential)))


lista_preditores_SSRI <- list(sanitation = fit_SSRI_sanitation$best_model$Negative_Exponential,
                                   imperm = fit_SSRI_imperm$best_model$Negative_Exponential,
                                   null = fit_SSRI_sanitation$all_models$No_effect)

AICctab(lista_preditores_SSRI, base = TRUE)

c_imperm$SSRI <- fit_SSRI_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$SSRI <- fit_SSRI_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$SSRI <- c(r.squaredLR(fit_SSRI_imperm$best_model$Negative_Exponential, null = fit_SSRI_imperm$all_models$No_effect))
r2_sanitation$SSRI <- c(r.squaredLR(fit_SSRI_sanitation$best_model$Negative_Exponential, null = fit_SSRI_sanitation$all_models$No_effect))








###############################################################
###############################################################
#########################    Aminoketone   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$Aminoketone, x = imperm_list$Aminoketone,
                start_exp_neg = list(a = 50, b= -10, c = -0.1))

fit_Aminoketone_imperm <- fit_neg_exponential(y = Ys$Aminoketone, x = imperm_list$Aminoketone,
                                       start_exp_neg = list(a = 50, b= -10, c = -0.1))

example_neg_exp(y = Ys$Aminoketone, x = imperm_list$Aminoketone,
                start_exp_neg = as.list(coef(fit_Aminoketone_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$Aminoketone, x = sanitation_list$Aminoketone,
                start_exp_neg = list(a = 50, b= -10, c = -0.1))

fit_Aminoketone_sanitation <- fit_neg_exponential(y = Ys$Aminoketone, x = sanitation_list$Aminoketone,
                                                  start_exp_neg = list(a = 50, b= -10, c = -0.1))

example_neg_exp(y = Ys$Aminoketone, x = sanitation_list$Aminoketone,
                start_exp_neg = as.list(coef(fit_Aminoketone_sanitation$all_models$Negative_Exponential)))


lista_preditores_Aminoketone <- list(sanitation = fit_Aminoketone_sanitation$all_models$Negative_Exponential,
                              imperm = fit_Aminoketone_imperm$all_models$Negative_Exponential,
                              null = fit_Aminoketone_sanitation$all_models$No_effect)

AICctab(lista_preditores_Aminoketone, base = TRUE)

c_imperm$Aminoketone <- fit_Aminoketone_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$Aminoketone <- fit_Aminoketone_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$Aminoketone <- c(r.squaredLR(fit_Aminoketone_imperm$all_models$Negative_Exponential, null = fit_Aminoketone_imperm$all_models$No_effect))
r2_sanitation$Aminoketone <- c(r.squaredLR(fit_Aminoketone_sanitation$all_models$Negative_Exponential, null = fit_Aminoketone_sanitation$all_models$No_effect))





###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################





###############################################################
###############################################################
#########################    sarscov2_n2   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$sarscov2_n2, x = imperm_list$sarscov2_n2,
                start_exp_neg = list(a = 350, b= -300, c = -0.1))

fit_sarscov2_n2_imperm <- fit_neg_exponential(y = Ys$sarscov2_n2, x = imperm_list$sarscov2_n2,
                                              start_exp_neg = list(a = 350, b= -300, c = -0.1))

example_neg_exp(y = Ys$sarscov2_n2, x = imperm_list$sarscov2_n2,
                start_exp_neg = as.list(coef(fit_sarscov2_n2_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$sarscov2_n2, x = sanitation_list$sarscov2_n2,
                start_exp_neg = list(a = 350, b= -300, c = -0.1))

fit_sarscov2_n2_sanitation <- fit_neg_exponential(y = Ys$sarscov2_n2, x = sanitation_list$sarscov2_n2,
                                                  start_exp_neg = list(a = 350, b= -300, c = -0.1))

example_neg_exp(y = Ys$sarscov2_n2, x = sanitation_list$sarscov2_n2,
                start_exp_neg = as.list(coef(fit_sarscov2_n2_sanitation$all_models$Negative_Exponential)))


lista_preditores_sarscov2_n2 <- list(sanitation = fit_sarscov2_n2_sanitation$all_models$Negative_Exponential,
                                     imperm = fit_sarscov2_n2_imperm$all_models$Negative_Exponential,
                                     null = fit_sarscov2_n2_sanitation$all_models$No_effect)

AICctab(lista_preditores_sarscov2_n2, base = TRUE)

c_imperm$sarscov2_n2 <- fit_sarscov2_n2_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$sarscov2_n2 <- fit_sarscov2_n2_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$sarscov2_n2 <- c(r.squaredLR(fit_sarscov2_n2_imperm$all_models$Negative_Exponential, null = fit_sarscov2_n2_imperm$all_models$No_effect))
r2_sanitation$sarscov2_n2 <- c(r.squaredLR(fit_sarscov2_n2_sanitation$all_models$Negative_Exponential, null = fit_sarscov2_n2_sanitation$all_models$No_effect))







###############################################################
###############################################################
#########################    sarscov2_n1   #############################

par(mfrow = c(1,1), mar = c(4,4,4,1))

example_neg_exp(y = Ys$sarscov2_n1, x = imperm_list$sarscov2_n1,
                start_exp_neg = list(a = 350, b= -300, c = -0.1))

fit_sarscov2_n1_imperm <- fit_neg_exponential(y = Ys$sarscov2_n1, x = imperm_list$sarscov2_n1,
                                              start_exp_neg = list(a = 350, b= -300, c = -0.1))

example_neg_exp(y = Ys$sarscov2_n1, x = imperm_list$sarscov2_n1,
                start_exp_neg = as.list(coef(fit_sarscov2_n1_imperm$all_models$Negative_Exponential)))



example_neg_exp(y = Ys$sarscov2_n1, x = sanitation_list$sarscov2_n1,
                start_exp_neg = list(a = 350, b= -300, c = -0.1))

fit_sarscov2_n1_sanitation <- fit_neg_exponential(y = Ys$sarscov2_n1, x = sanitation_list$sarscov2_n1,
                                                  start_exp_neg = list(a = 350, b= -300, c = -0.1))

example_neg_exp(y = Ys$sarscov2_n1, x = sanitation_list$sarscov2_n1,
                start_exp_neg = as.list(coef(fit_sarscov2_n1_sanitation$all_models$Negative_Exponential)))


lista_preditores_sarscov2_n1 <- list(sanitation = fit_sarscov2_n1_sanitation$all_models$Negative_Exponential,
                                     imperm = fit_sarscov2_n1_imperm$all_models$Negative_Exponential,
                                     null = fit_sarscov2_n1_sanitation$all_models$No_effect)

AICctab(lista_preditores_sarscov2_n1, base = TRUE)

c_imperm$sarscov2_n1 <- fit_sarscov2_n1_imperm$coefs_mean$mod_exp_neg[3]
c_sanitation$sarscov2_n1 <- fit_sarscov2_n1_sanitation$coefs_mean$mod_exp_neg[3]

r2_imperm$sarscov2_n1 <- c(r.squaredLR(fit_sarscov2_n1_imperm$all_models$Negative_Exponential, null = fit_sarscov2_n1_imperm$all_models$No_effect))
r2_sanitation$sarscov2_n1 <- c(r.squaredLR(fit_sarscov2_n1_sanitation$all_models$Negative_Exponential, null = fit_sarscov2_n1_sanitation$all_models$No_effect))








###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################



c_imperm <- unlist(c_imperm)
c_sanitation <- unlist(c_sanitation)

r2_imperm <- unlist(r2_imperm)
r2_sanitation <- unlist(r2_sanitation)

variable <- rep(names(r2_imperm), 2)
predictor <- c(rep("Impermeabilization", length(c_imperm)), rep("sanitation", length(c_imperm)))

c_neg_exp <- c(c_imperm, c_sanitation)
r2_neg_exp <- c(r2_imperm, r2_sanitation)

wide_predictors <- c(rep("water", 14),
                     rep("contaminants", 5),
                     rep("pharmaceuticals", 6),
                     rep("sarscov", 2))

data_neg_exp <- data.frame(variable, predictor,c_neg_exp, r2_neg_exp, wide_predictors)





library(glmmTMB)
library(emmeans)
library(DHARMa)
library(car)

model_r2_null <- glmmTMB(r2_neg_exp ~ 1 + (1|variable), data = data_neg_exp)
model_r2 <- glmmTMB(r2_neg_exp ~ predictor + (1|variable), data = data_neg_exp)
#model_r2_2 <- glmmTMB(r2_neg_exp ~ predictor + wide_predictors + (1|variable), data = data_neg_exp)
#model_r2_3 <- glmmTMB(r2_neg_exp ~ predictor * wide_predictors + (1|variable), data = data_neg_exp)

plot(simulateResiduals(model_r2))
AICctab(model_r2_null, model_r2, base = TRUE)



model_c_null <- glmmTMB(c_neg_exp ~ 1 + (1|variable), dispformula = ~  predictor + variable,
                        data = data_neg_exp)
model_c <- glmmTMB(c_neg_exp ~ predictor + (1|variable), dispformula = ~ predictor + variable,
                   data = data_neg_exp,control=glmmTMBControl(optimizer= nlminb, profile = TRUE))

#model_c_2 <- glmmTMB(c_neg_exp ~ predictor + wide_predictors + (1|variable), dispformula = ~ predictor +variable,
#                     data = data_neg_exp)
                   #control=glmmTMBControl(optimizer= nlminb, profile = TRUE))

#model_c_3 <- glmmTMB(c_neg_exp ~ predictor * wide_predictors + (1|variable), dispformula = ~ predictor +variable,
#                     data = data_neg_exp)
                   #control=glmmTMBControl(optimizer= nlminb, profile = TRUE))
plot(simulateResiduals(model_c))
AICctab(model_c_null, model_c, base = TRUE)




library(yarrr)
data_neg_exp$predictor_num <- as.numeric(as.numeric(as.factor(predictor)))




png("Resultados/r2_neg_exp.png", width = 12, height = 12, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,5,1,1))
plot(r2_neg_exp ~ predictor_num, data = data_neg_exp, xlim = c(0.75, 2.25), col = "white", xaxt = "n", xlab = "", ylab = "")

new_data <- data_neg_exp[wide_predictors == "water",]
for(i in 1:length(which(wide_predictors == "water"))){
  points <- new_data$r2_neg_exp[new_data$variable ==  names(r2_imperm)[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("deepskyblue1", trans.val = 0.5))
}
points(r2_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("deepskyblue1", trans.val = 0.3))

new_data <- data_neg_exp[wide_predictors == "contaminants",]
for(i in 1:length(which(wide_predictors == "contaminants"))){
  points <- new_data$r2_neg_exp[new_data$variable ==  names(r2_imperm[wide_predictors == "contaminants"])[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("black", trans.val = 0.5))
}
points(r2_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("black", trans.val = 0.3))


new_data <- data_neg_exp[wide_predictors == "pharmaceuticals",]
for(i in 1:length(which(wide_predictors == "pharmaceuticals"))){
  points <- new_data$r2_neg_exp[new_data$variable ==  names(r2_imperm[wide_predictors == "pharmaceuticals"])[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("gold2", trans.val = 0.5))
}
points(r2_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("gold2", trans.val = 0.3))


new_data <- data_neg_exp[wide_predictors == "sarscov",]
for(i in 1:length(which(wide_predictors == "sarscov"))){
  points <- new_data$r2_neg_exp[new_data$variable ==  names(r2_imperm[wide_predictors == "sarscov"])[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("firebrick1", trans.val = 0.5))
}
points(r2_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("firebrick1", trans.val = 0.3))


title(ylab = "Likelihood Ratio R²", cex.lab = 1.5)
axis(1, at = c(1, 2), labels = c("Impermeabilized", "Number of habitants"), line = 0, cex.axis = 1.25)
axis(1, at = c(1, 2), labels = c("area", "without sanitaton"), tick = FALSE, line = 1, cex.axis = 1.25)
dev.off()








png("Resultados/c_neg_exp.png", width = 12, height = 12, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,5,1,1))
plot(c_neg_exp ~ predictor_num, data = data_neg_exp, xlim = c(0.75, 2.25), col = "white", xaxt = "n", xlab = "", ylab = "")

new_data <- data_neg_exp[wide_predictors == "water",]
for(i in 1:length(which(wide_predictors == "water"))){
  points <- new_data$c_neg_exp[new_data$variable ==  names(r2_imperm)[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("deepskyblue1", trans.val = 0.5))
}
points(c_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("deepskyblue1", trans.val = 0.3))

new_data <- data_neg_exp[wide_predictors == "contaminants",]
for(i in 1:length(which(wide_predictors == "contaminants"))){
  points <- new_data$c_neg_exp[new_data$variable ==  names(r2_imperm[wide_predictors == "contaminants"])[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("black", trans.val = 0.5))
}
points(c_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("black", trans.val = 0.3))


new_data <- data_neg_exp[wide_predictors == "pharmaceuticals",]
for(i in 1:length(which(wide_predictors == "pharmaceuticals"))){
  points <- new_data$c_neg_exp[new_data$variable ==  names(r2_imperm[wide_predictors == "pharmaceuticals"])[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("gold2", trans.val = 0.5))
}
points(c_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("gold2", trans.val = 0.3))


new_data <- data_neg_exp[wide_predictors == "sarscov",]
for(i in 1:length(which(wide_predictors == "sarscov"))){
  points <- new_data$c_neg_exp[new_data$variable ==  names(r2_imperm[wide_predictors == "sarscov"])[[i]] ]
  lines(x = c(1, 2), y = points, lwd = 2, col = transparent("firebrick1", trans.val = 0.5))
}
points(c_neg_exp ~ predictor_num, pch = 16, cex = 3, data = new_data, col = transparent("firebrick1", trans.val = 0.3))


title(ylab = "Negative exponential: c", cex.lab = 1.5)
axis(1, at = c(1, 2), labels = c("Impermeabilized", "Number of habitants"), line = 0, cex.axis = 1.25)
axis(1, at = c(1, 2), labels = c("area", "without sanitaton"), tick = FALSE, line = 1, cex.axis = 1.25)
dev.off()
