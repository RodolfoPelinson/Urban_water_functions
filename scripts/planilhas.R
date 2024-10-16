library(vegan)
library(shape)
library(scales)

water_quality <- read.csv("data/water_quality_R_ultima_versao.csv")

water_quality_seca <- water_quality[water_quality$Survey == "seca 2021",]

colnames(water_quality_seca)

var_interesse_agua <- c("X.urb",
                        "chlorophyll_a",
                        "phycocyanin",
                        "Temperature_.oC.",
                        "DO_.mg.L.",
                        "Conductivity_.uS.cm.",
                        "pH",
                        "turbidity_.NTU.",
                        "redox_potential_.mV.",
                        "TOC",
                        "TC",
                        "IC",
                        "TN",
                        "TP_.Valderrama.",
                        "Discharge_.L.s.",
                        "SPC_.uS.cm.")


var_interesse_farmacos <- c("X.urb",colnames(water_quality_seca)[76:86])

var_interesse_contaminantes <- c("X.urb",colnames(water_quality_seca)[87:102])

var_interesse_farmacos_pa <- c("X.urb",colnames(water_quality_seca)[36:75])

####################3
#Ajeitando planilhas variaveis da água

water_quality_agua <- water_quality_seca[,match(var_interesse_agua, colnames(water_quality_seca))]
water_quality_agua <- apply(water_quality_agua, 2, as.numeric)

water_quality_agua_mean <- aggregate(water_quality_agua, by = list(water_quality_seca$Catchment),  FUN = mean, na.rm = TRUE)
water_quality_agua_sd <- aggregate(water_quality_agua, by = list(water_quality_seca$Catchment),  FUN = sd, na.rm = TRUE)

for(i in 3:ncol(water_quality_agua_sd)){
  water_quality_agua_sd[,i][water_quality_agua_sd[,i] == 0] <- Inf
  water_quality_agua_sd[,i][water_quality_agua_sd[,i] == Inf] <- min(water_quality_agua_sd[,i], na.rm = TRUE)
  water_quality_agua_sd[,i][is.na(water_quality_agua_sd[,i])] <- mean(water_quality_agua_sd[,i], na.rm = TRUE)
  weights_agua <- water_quality_agua_sd
  weights_agua[,i] <- 1/weights_agua[,i]
}

#water_quality_agua_sd$Conductivity_.uS.cm.[urb == 0]

#water_quality_agua_mean$Conductivity_.uS.cm.[urb == 0]


###############################################

origem_farmacos <- "Analise de agua 0-100 decil - USP_UNICAMP.xlsx. Dados - Microbacias hidrograficas - Luis Schiesari.xlsx"


water_quality_farmacos <- water_quality[water_quality$File_of_origin == origem_farmacos,]
water_quality_farmacos <- water_quality_farmacos[,match(var_interesse_farmacos, colnames(water_quality_farmacos))]
water_quality_farmacos <- data.frame(apply(water_quality_farmacos, 2, as.numeric))

water_quality_farmacos_pa <- water_quality[water_quality$File_of_origin == origem_farmacos,]
water_quality_farmacos_pa <- water_quality_farmacos_pa[,match(var_interesse_farmacos_pa, colnames(water_quality_farmacos_pa))]
water_quality_farmacos_pa <- data.frame(apply(water_quality_farmacos_pa, 2, as.numeric))


origem_contaminantes <- "Analise de agua 0-100 decil - USP_UNICAMP.xlsx. Dados - Microbacias hidrograficas - Luis Schiesari.xlsx"

water_quality_contaminantes <- water_quality[water_quality$File_of_origin == origem_contaminantes,]
water_quality_contaminantes <- water_quality_contaminantes[,match(var_interesse_contaminantes, colnames(water_quality_contaminantes))]
water_quality_contaminantes <- data.frame(apply(water_quality_contaminantes, 2, as.numeric))




##############################################

data_farmacos_class <- c("SNRI",	"Aminoketone",	"Aminoketone",	"SSRI",	"SSRI",	"SSRI",	"SSRI",	"SSRI",	"Tricyclic",	"Tricyclic"	,"SSRI")




data_farmacos_class <- unlist(c(data_farmacos_class))

classess <- unique(data_farmacos_class)

#classess <- classess[-1]

data_farmacos_by_class <- list()

data_farmacos <- data.frame(water_quality_farmacos[,-1])

for(i in 2:length(classess)){
  sums <- rowSums(data_farmacos[,data_farmacos_class == classess[i]]) 
  data_farmacos_by_class[[i]] <- sums
  names(data_farmacos_by_class)[i] <- classess[i]
}

data_farmacos_by_class[[1]] <- data_farmacos[,data_farmacos_class == classess[1]]
names(data_farmacos_by_class)[[1]] <- classess[1]

data_farmacos_by_class <- data.frame(data_farmacos_by_class)

data_farmacos_by_class$all <- rowSums(data_farmacos_by_class)

data_farmacos_by_class$n_pharma <- rowSums(water_quality_farmacos_pa[,-1])


data_farmacos_by_class$urb <- water_quality_farmacos[,1]



#############################################################3
porcentagens_farmacos_zero <- rep(NA, ncol(data_farmacos_by_class))
n_farmacos <- rep(NA, ncol(data_farmacos_by_class))

for(i in 1:ncol(data_farmacos_by_class)){
  n <- length(data_farmacos_by_class[,i])
  n_zero <- length(data_farmacos_by_class[,i][data_farmacos_by_class[,i] == 0])
  n_farmacos[i] <- n_zero
  porcentagens_farmacos_zero[i] <- (n_zero/n)
}
names(porcentagens_farmacos_zero) <- colnames(data_farmacos_by_class)
names(n_farmacos) <- colnames(data_farmacos_by_class)



porcentagens_antidepressants_zero <- rep(NA, ncol(data_farmacos))
n_antidepressants <- rep(NA, ncol(data_farmacos))

for(i in 1:ncol(data_farmacos)){
  n <- length(data_farmacos[,i])
  n_zero <- length(data_farmacos[,i][data_farmacos[,i] == 0])
  n_antidepressants[i] <- n_zero
  porcentagens_antidepressants_zero[i] <- (n_zero/n)
}
names(porcentagens_antidepressants_zero) <- colnames(data_farmacos)
names(n_antidepressants) <- colnames(data_farmacos)



porcentagens_contaminantes_zero <- rep(NA, ncol(water_quality_contaminantes))
n_contaminantes <- rep(NA, ncol(water_quality_contaminantes))

for(i in 1:ncol(water_quality_contaminantes)){
  n <- length(water_quality_contaminantes[,i])
  n_zero <- length(water_quality_contaminantes[,i][water_quality_contaminantes[,i] == 0])
  n_contaminantes[i] <- n_zero
  porcentagens_contaminantes_zero[i] <- (n_zero/n)
}
names(porcentagens_contaminantes_zero) <- colnames(water_quality_contaminantes)
names(n_contaminantes) <- colnames(water_quality_contaminantes)


zeros <- data.frame(N_zero = c(n_farmacos,
                      n_antidepressants,
                      n_contaminantes), porcentagens_zero = c(porcentagens_farmacos_zero,
                                                              porcentagens_antidepressants_zero,
                                                              porcentagens_contaminantes_zero))

write.csv(zeros, "Resultados/Porcentagem_de_zeros.csv")
##############################################





sarscov2 <- read.csv("data/Dados - Covid19 - Águas superficiais.csv")


sarscov2_n1 <- sarscov2[is.na(sarscov2$N1_copias_L) == FALSE,1:4]
sarscov2_n2 <- sarscov2[is.na(sarscov2$N2_copias_L) == FALSE,c(1:2,5:6)]


sarscov2_n1_mean <- aggregate(sarscov2_n1, by = list(sarscov2_n1$site),  FUN = mean, na.rm = TRUE)
sarscov2_n2_mean <- aggregate(sarscov2_n2, by = list(sarscov2_n2$site),  FUN = mean, na.rm = TRUE)

sarscov2_n1_mean$urb <- water_quality_seca$X.urb[match(sarscov2_n1_mean$Group.1, water_quality_seca$Catchment)]
sarscov2_n2_mean$urb <- water_quality_seca$X.urb[match(sarscov2_n2_mean$Group.1, water_quality_seca$Catchment)]


##################################

water_quality_agua_mean$turbidity_.NTU.

summary(water_quality_agua_mean$turbidity_.NTU.)

quantile(water_quality_agua_mean$turbidity_.NTU., probs = c(1))

quantile(water_quality_agua_mean$turbidity_.NTU., probs = c(0.95))*3



###################################

map_biomas_dados <- read.csv("data/consolidacao_dados_socioeconomicos_20240624.csv")

map_biomas_dados <- map_biomas_dados[match(water_quality_agua_mean$Group.1, map_biomas_dados$ID_geral),]
