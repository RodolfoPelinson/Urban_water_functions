usos_da_terra$prop_pasto_2019[is.na(usos_da_terra$prop_pasto_2019)] <- 0
usos_da_terra$prop_cana_2019[is.na(usos_da_terra$prop_cana_2019)] <- 0
usos_da_terra$prop_mosaico_2019[is.na(usos_da_terra$prop_mosaico_2019)] <- 0
usos_da_terra$prop_outras_culturas_2019[is.na(usos_da_terra$prop_outras_culturas_2019)] <- 0
usos_da_terra$prop_outras_nao_veg_2019[is.na(usos_da_terra$prop_outras_nao_veg_2019)] <- 0
usos_da_terra$prop_soja_2019[is.na(usos_da_terra$prop_soja_2019)] <- 0
usos_da_terra$prop_silvicultura_2019[is.na(usos_da_terra$prop_silvicultura_2019)] <- 0

#Não tem
#Soja
#Cana
#Outras não veg
#Outras

plot(usos_da_terra$prop_mosaico_2019, usos_da_terra$prop_pasto_2019)
cor(usos_da_terra$prop_mosaico_2019, usos_da_terra$prop_pasto_2019)

#Só pasto e mosaico

outros_usos <- data.frame(id = usos_da_terra$bacia_id_nova,
                          mosaico = usos_da_terra$prop_mosaico_2019,
                          pasto = usos_da_terra$prop_pasto_2019)

effects <- list()

library(DHARMa)
library(car)
library(vegan)


residuals_list <- list(
  do = residuals(fit_do$all_models$Negative_Exponential),
  pH = residuals(fit_pH$all_models$Negative_Exponential),
  redox = residuals(fit_redox$all_models$Negative_Exponential),
  turbidity = residuals(fit_turbidity$all_models$Linear),
  Temperatura = residuals(fit_Temperatura$all_models$Negative_Exponential),
  TOC = residuals(fit_TOC$all_models$Negative_Exponential),
  TC = residuals(fit_TC$all_models$Negative_Exponential),
  SPC = residuals(fit_SPC$all_models$Negative_Exponential),
  TN = residuals(fit_TN$all_models$Negative_Exponential),
  IC = residuals(fit_IC$all_models$`HOF_(Sigmoid)`),
  condutividade = residuals(fit_condutividade$all_models$Negative_Exponential),
  descarga = residuals(fit_descarga$all_models$Linear),
  clorofila = residuals(fit_clorofila$all_models$Linear),
  phycocianin = residuals(fit_phycocianin$all_models$Linear),
  Carbendazin = residuals(fit_Carbendazin$all_models$Negative_Exponential),
  Paracetamol = residuals(fit_Paracetamol$all_models$Negative_Exponential),
  Caffeine = residuals(fit_Caffeine$all_models$`HOF_(Sigmoid)`),
  Diuron = residuals(fit_Diuron$all_models$Linear),
  Atrazin = residuals(fit_Atrazin$all_models$Linear),
  SNRI = residuals(fit_SNRI$all_models$`HOF_(Sigmoid)`),
  n_pharma = residuals(fit_n_pharma$all_models$Negative_Exponential),
  all = residuals(fit_all$all_models$Linear),
  Tricyclic = residuals(fit_Tricyclic$all_models$`HOF_(Sigmoid)`),
  SSRI = residuals(fit_SSRI$all_models$Linear),
  Aminoketone = residuals(fit_Aminoketone$all_models$Linear),
  sarscov2_n2 = residuals(fit_sarscov2_n2$all_models$`HOF_(Sigmoid)`),
  sarscov2_n1 = residuals(fit_sarscov2_n1$all_models$`HOF_(Sigmoid)`)

)

lengths <- rep(NA, length(residuals_list))
for(i in 1:length(residuals_list)){
  lengths[[i]] <- length(residuals_list[[i]])
}

outros_usos_list <- list()
for(i in 1:length(residuals_list)){
  outros_usos_list[[i]] <- outros_usos
}



turbidity2 <-water_quality_agua_mean$turbidity_.NTU.
outros_usos_list[[which(names(residuals_list) == "turbidity")]] <- outros_usos[turbidity2 <= quantile(turbidity2, probs = c(0.95))*3,]

Carbendazin2 <- water_quality_contaminantes$Carbendazin
outros_usos_list[[which(names(residuals_list) == "Carbendazin")]] <- outros_usos[is.na(Carbendazin2) == FALSE,]

Paracetamol2 <- water_quality_contaminantes$Paracetamol
outros_usos_list[[which(names(residuals_list) == "Paracetamol")]] <- outros_usos[is.na(Paracetamol2) == FALSE,]

Caffeine2 <- water_quality_contaminantes$Caffeine
outros_usos_list[[which(names(residuals_list) == "Caffeine")]] <- outros_usos[is.na(Caffeine2) == FALSE,]

Diuron2 <- water_quality_contaminantes$Diuron
outros_usos_list[[which(names(residuals_list) == "Diuron")]] <- outros_usos[is.na(Diuron2) == FALSE,]

Atrazin2 <- water_quality_contaminantes$Atrazin
outros_usos_list[[which(names(residuals_list) == "Atrazin")]] <- outros_usos[is.na(Atrazin2) == FALSE,]


sarscov2_n12 <- sarscov2_n1_mean2$N1_copias_L/1000
sarscov2_n22 <- sarscov2_n2_mean2$N2_copias_L/1000
outros_usos_sarscov <-  outros_usos[match(sarscov2_n1_mean2$Group.1, map_biomas_dados$ID_geral),]
outros_usos_list[[which(names(residuals_list) == "sarscov2_n2")]] <- outros_usos_sarscov
outros_usos_list[[which(names(residuals_list) == "sarscov2_n1")]] <- outros_usos_sarscov


rows <- rep(NA, length(residuals_list))
for(i in 1:length(residuals_list)){
  rows[[i]] <- nrow(outros_usos_list[[i]])
}


###################################################################################
###################################################################################
###################################################################################

#models_null <- list()
models <- list()
residuals_DHARMa <- list()
Anovas <- list()
r2 <- list()

#AICctab <- list()

for(i in 1:length(residuals_list)){
  models[[i]] <- lm(residuals_list[[i]] ~ pasto + mosaico, data = outros_usos_list[[i]])
  Anovas[[i]] <- data.frame(Anova(models[[i]]))
  r2[[i]] <- summary(models[[i]])$adj.r.squared
  residuals_DHARMa[[i]] <- simulateResiduals(models[[i]])
}

names(models) <- names(residuals_list)
names(residuals_DHARMa) <- names(residuals_list)
names(Anovas) <- names(residuals_list)
names(r2) <- names(residuals_list)


pasto_coef <- list()
mosaico_coef <- list()


for(i in 1:length(Anovas)){
  if(Anovas[[i]]$Pr..F.[1] <= 0.05){
    pasto_coef[[i]] <- coef(models[[i]])[1:2]
  }else{pasto_coef[[i]] <- NA}
}
names(pasto_coef) <- names(Anovas)

for(i in 1:length(Anovas)){
  if(Anovas[[i]]$Pr..F.[2] <= 0.05){
    mosaico_coef[[i]] <- coef(models[[i]])[c(1,3)]
  }else{mosaico_coef[[i]] <- NA}
}
names(mosaico_coef) <- names(Anovas)



pasto_coef
mosaico_coef

important_pasture <- names(pasto_coef)[is.na(pasto_coef)==FALSE]
important_pasture

r2$SPC
r2$condutividade
r2$IC
r2$Diuron

write.csv(Anovas,"Resultados/residual_analyses.csv")

