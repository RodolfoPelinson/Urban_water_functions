
# Negative exponential models

#C parameter


means <- list()
means_b <- list()
lower_conf <- list()
upper_conf <- list() 

#Clorofila
means$Chlorophyll_a <- fit_clorofila$coefs_mean$mod_exp_neg[3]
lower_conf$Chlorophyll_a <- fit_clorofila$confint_mean$mod_exp_neg[3,1]
upper_conf$Chlorophyll_a <- fit_clorofila$confint_mean$mod_exp_neg[3,2]
means_b$Chlorophyll_a <- fit_clorofila$coefs_mean$mod_exp_neg[2]


#Phycocianin
#means$Phycocianin <- fit_phycocianin$coefs_mean$mod_exp_neg[3]
#lower_conf$Phycocianin <- fit_phycocianin$confint_mean$mod_exp_neg[3,1]
#upper_conf$Phycocianin <- fit_phycocianin$confint_mean$mod_exp_neg[3,2]


#Temperatura
means$Temperature <- fit_Temperatura$coefs_mean$mod_exp_neg[3]
lower_conf$Temperature <- fit_Temperatura$confint_mean$mod_exp_neg[3,1]
upper_conf$Temperature <- fit_Temperatura$confint_mean$mod_exp_neg[3,2]
means_b$Temperature <- fit_Temperatura$coefs_mean$mod_exp_neg[2]


#DO
means$DO <- fit_do$coefs_mean$mod_exp_neg[3]
lower_conf$DO <- fit_do$confint_mean$mod_exp_neg[3,1]
upper_conf$DO <- fit_do$confint_mean$mod_exp_neg[3,2]
means_b$DO <- fit_do$coefs_mean$mod_exp_neg[2]


#SPC
means$SPC <- fit_SPC$coefs_mean$mod_exp_neg[3]
lower_conf$SPC <- fit_SPC$confint_mean$mod_exp_neg[3,1]
upper_conf$SPC <- fit_SPC$confint_mean$mod_exp_neg[3,2]
means_b$SPC <- fit_SPC$coefs_mean$mod_exp_neg[2]

#Condutividade
#means$Conductivity <- fit_condutividade$coefs_mean$mod_exp_neg[3]
#lower_conf$Conductivity <- fit_condutividade$confint_mean$mod_exp_neg[3,1]
#upper_conf$Conductivity <- fit_condutividade$confint_mean$mod_exp_neg[3,2]

#pH
means$pH <- fit_pH$coefs_mean$mod_exp_neg[3]
lower_conf$pH <- fit_pH$confint_mean$mod_exp_neg[3,1]
upper_conf$pH <- fit_pH$confint_mean$mod_exp_neg[3,2]
means_b$pH <- fit_pH$coefs_mean$mod_exp_neg[2]

#redox potential
#means$Redox_potential <- fit_redox$coefs_mean$mod_exp_neg[3]
#lower_conf$Redox_potential <- fit_redox$confint_mean$mod_exp_neg[3,1]
#upper_conf$Redox_potential <- fit_redox$confint_mean$mod_exp_neg[3,2]

#Turbidity
means$Turbidity <- fit_turbidity$coefs_mean$mod_exp_neg[3]
lower_conf$Turbidity <- fit_turbidity$confint_mean$mod_exp_neg[3,1]
upper_conf$Turbidity <- fit_turbidity$confint_mean$mod_exp_neg[3,2]
means_b$Turbidity <- fit_turbidity$coefs_mean$mod_exp_neg[2]

#TOC
means$TOC <- fit_TOC$coefs_mean$mod_exp_neg[3]
lower_conf$TOC <- fit_TOC$confint_mean$mod_exp_neg[3,1]
upper_conf$TOC <- fit_TOC$confint_mean$mod_exp_neg[3,2]
means_b$TOC <- fit_TOC$coefs_mean$mod_exp_neg[2]

#TC
means$TC <- fit_TC$coefs_mean$mod_exp_neg[3]
lower_conf$TC <- fit_TC$confint_mean$mod_exp_neg[3,1]
upper_conf$TC <- fit_TC$confint_mean$mod_exp_neg[3,2]
means_b$TC <- fit_TC$coefs_mean$mod_exp_neg[2]

#IC
means$IC <- fit_IC$coefs_mean$mod_exp_neg[3]
lower_conf$IC <- fit_IC$confint_mean$mod_exp_neg[3,1]
upper_conf$IC <- fit_IC$confint_mean$mod_exp_neg[3,2]
means_b$IC <- fit_IC$coefs_mean$mod_exp_neg[2]

#TN
means$TN <- fit_TN$coefs_mean$mod_exp_neg[3]
lower_conf$TN <- fit_TN$confint_mean$mod_exp_neg[3,1]
upper_conf$TN <- fit_TN$confint_mean$mod_exp_neg[3,2]
means_b$TN <- fit_TN$coefs_mean$mod_exp_neg[2]

#Discharge
means$Discharge <- fit_descarga$coefs_mean$mod_exp_neg[3]
lower_conf$Discharge <- fit_descarga$confint_mean$mod_exp_neg[3,1]
upper_conf$Discharge <- fit_descarga$confint_mean$mod_exp_neg[3,2]
means_b$Discharge <- fit_descarga$coefs_mean$mod_exp_neg[2]



means <- unlist(means)
means_b <- unlist(means_b)
lower_conf <- unlist(lower_conf)
upper_conf <- unlist(upper_conf)

upper_conf[is.na(upper_conf)] <- 0
lower_conf[is.na(lower_conf)] <- NA
upper_conf[is.na(lower_conf)] <- NA

ord <- order(means)

upper_conf <- upper_conf[ord]
lower_conf <- lower_conf[ord]
means <- means[ord]

names <- names(means)
names <- gsub("\\.c","_", names)
names <- gsub('_c',"", names)
names <- gsub("_"," ",names)



png("Resultados/Água/Curvas/Negative_Exponential_c.png", width = 9, height = 9, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,8,1,1))
My_coefplot(means, upper_conf, lower_conf, species_labels = names, xlab = "Negative Exponential: c", cex.axis = 1,y_spa = 0,)
dev.off()




############################################################################
############################################################################
############################################################################


means_n_pharma <- list()
means_n_pharma_b <- list()
lower_n_pharma_conf <- list()
upper_n_pharma_conf <- list()

#N_pharma
means_n_pharma$N_pharmaceuticals <- fit_n_pharma$coefs_mean$mod_exp_neg[3]
lower_n_pharma_conf$N_pharmaceuticals <- fit_n_pharma$confint_mean$mod_exp_neg[3,1]
upper_n_pharma_conf$N_pharmaceuticals <- fit_n_pharma$confint_mean$mod_exp_neg[3,2]
means_n_pharma_b$N_pharmaceuticals <- fit_n_pharma$coefs_mean$mod_exp_neg[2]


#All antidepressants
means_n_pharma$All_antidepressants <- fit_all$coefs_mean$mod_exp_neg[3]
lower_n_pharma_conf$All_antidepressants <- fit_all$confint_mean$mod_exp_neg[3,1]
upper_n_pharma_conf$All_antidepressants <- fit_all$confint_mean$mod_exp_neg[3,2]
means_n_pharma_b$All_antidepressants <- fit_all$coefs_mean$mod_exp_neg[2]


#SSRI
means_n_pharma$SSRI <- fit_SSRI$coefs_mean$mod_exp_neg[3]
lower_n_pharma_conf$SSRI <- fit_SSRI$confint_mean$mod_exp_neg[3,1]
upper_n_pharma_conf$SSRI <- fit_SSRI$confint_mean$mod_exp_neg[3,2]
means_n_pharma_b$SSRI <- fit_SSRI$coefs_mean$mod_exp_neg[2]


#SNRI
means_n_pharma$SNRI <- fit_SNRI$coefs_mean$mod_exp_neg[3]
lower_n_pharma_conf$SNRI <- fit_SNRI$confint_mean$mod_exp_neg[3,1]
upper_n_pharma_conf$SNRI <- fit_SNRI$confint_mean$mod_exp_neg[3,2]
means_n_pharma_b$SNRI <- fit_SNRI$coefs_mean$mod_exp_neg[2]


#Aminoketone
means_n_pharma$Aminoketone <- fit_Aminoketone$coefs_mean$mod_exp_neg[3]
lower_n_pharma_conf$Aminoketone <- fit_Aminoketone$confint_mean$mod_exp_neg[3,1]
upper_n_pharma_conf$Aminoketone <- fit_Aminoketone$confint_mean$mod_exp_neg[3,2]
means_n_pharma_b$Aminoketone <- fit_Aminoketone$coefs_mean$mod_exp_neg[2]


#Tricyclic
means_n_pharma$Tricyclic <- fit_Tricyclic$coefs_mean$mod_exp_neg[3]
lower_n_pharma_conf$Tricyclic <- fit_Tricyclic$confint_mean$mod_exp_neg[3,1]
upper_n_pharma_conf$Tricyclic <- fit_Tricyclic$confint_mean$mod_exp_neg[3,2]
means_n_pharma_b$Tricyclic <- fit_Tricyclic$coefs_mean$mod_exp_neg[2]



means_n_pharma <- unlist(means_n_pharma)
means_n_pharma_b <- unlist(means_n_pharma_b)
lower_n_pharma_conf <- unlist(lower_n_pharma_conf)
upper_n_pharma_conf <- unlist(upper_n_pharma_conf)

upper_n_pharma_conf[is.na(upper_n_pharma_conf)] <- 0
lower_n_pharma_conf[is.na(lower_n_pharma_conf)] <- NA
upper_n_pharma_conf[is.na(lower_n_pharma_conf)] <- NA

ord <- order(means_n_pharma)

upper_n_pharma_conf <- upper_n_pharma_conf[ord]
lower_n_pharma_conf <- lower_n_pharma_conf[ord]
means_n_pharma <- means_n_pharma[ord]

names_n_pharma <- names(means_n_pharma)
names_n_pharma <- gsub("\\.c","_", names_n_pharma)
names_n_pharma <- gsub('_c',"", names_n_pharma)
names_n_pharma <- gsub("_"," ",names_n_pharma)



png("Resultados/Fármacos/Curvas/N_PHARMA_Negative_Exponential_c.png", width = 9, height = 9, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,8,1,1))
My_coefplot(means_n_pharma, upper_n_pharma_conf, lower_n_pharma_conf, species_labels = names_n_pharma, xlab = "Negative Exponential: c", cex.axis = 1,y_spa = 0,)
dev.off()






############################################################################
############################################################################
############################################################################


means_contaminants <- list()
means_contaminants_b <- list()

lower_contaminants_conf <- list()
upper_contaminants_conf <- list()

#24D
#means_contaminants$`24D` <- fit_X2.4D$coefs_mean$mod_exp_neg[3]
#lower_contaminants_conf$`24D` <- fit_X2.4D$confint_mean$mod_exp_neg[3,1]
#upper_contaminants_conf$`24D` <- fit_X2.4D$confint_mean$mod_exp_neg[3,2]
#lower_contaminants_conf$`24D` <- NA
#upper_contaminants_conf$`24D` <- NA

#BPA
#means_contaminants$BPA <- fit_BPA$coefs_mean$mod_exp_neg[3]
#lower_contaminants_conf$BPA <- fit_BPA$confint_mean$mod_exp_neg[3,1]
#upper_contaminants_conf$BPA <- fit_BPA$confint_mean$mod_exp_neg[3,2]

#Fipronil
#means_contaminants$Fipronil <- fit_Fipronil$coefs_mean$mod_exp_neg[3]
#lower_contaminants_conf$Fipronil <- fit_Fipronil$confint_mean$mod_exp_neg[3,1]
#upper_contaminants_conf$Fipronil <- fit_Fipronil$confint_mean$mod_exp_neg[3,2]

#Fipronil_Sulfona
#means_contaminants$Fipronil_Sulfona <- fit_Fipronil_Sulfona$coefs_mean$mod_exp_neg[3]
#lower_contaminants_conf$Fipronil_Sulfona <- fit_Fipronil_Sulfona$confint_mean$mod_exp_neg[3,1]
#upper_contaminants_conf$Fipronil_Sulfona <- fit_Fipronil_Sulfona$confint_mean$mod_exp_neg[3,2]


#DIA
#means_contaminants$DIA <- fit_DIA$coefs_mean$mod_exp_neg[3]
#lower_contaminants_conf$DIA <- fit_DIA$confint_mean$mod_exp_neg[3,1]
#upper_contaminants_conf$DIA <- fit_DIA$confint_mean$mod_exp_neg[3,2]


#Carbendazin
means_contaminants$Carbendazin <- fit_Carbendazin$coefs_mean$mod_exp_neg[3]
lower_contaminants_conf$Carbendazin <- fit_Carbendazin$confint_mean$mod_exp_neg[3,1]
upper_contaminants_conf$Carbendazin <- fit_Carbendazin$confint_mean$mod_exp_neg[3,2]
means_contaminants_b$Carbendazin <- fit_Carbendazin$coefs_mean$mod_exp_neg[2]


#Tebutiuron
#means_contaminants$Tebutiuron  <- fit_Atrazin$coefs_mean$mod_exp_neg[3]
#lower_contaminants_conf$Tebutiuron  <- fit_Atrazin$confint_mean$mod_exp_neg[3,1]
#upper_contaminants_conf$Tebutiuron  <- fit_Atrazin$confint_mean$mod_exp_neg[3,2]

#Atrazin
means_contaminants$Atrazin  <- fit_Atrazin$coefs_mean$mod_exp_neg[3]
lower_contaminants_conf$Atrazin  <- fit_Atrazin$confint_mean$mod_exp_neg[3,1]
upper_contaminants_conf$Atrazin  <- fit_Atrazin$confint_mean$mod_exp_neg[3,2]
means_contaminants_b$Atrazin <- fit_Atrazin$coefs_mean$mod_exp_neg[2]


#Diuron
means_contaminants$Diuron  <- fit_Diuron$coefs_mean$mod_exp_neg[3]
lower_contaminants_conf$Diuron  <- fit_Diuron$confint_mean$mod_exp_neg[3,1]
upper_contaminants_conf$Diuron  <- fit_Diuron$confint_mean$mod_exp_neg[3,2]
means_contaminants_b$Diuron <- fit_Diuron$coefs_mean$mod_exp_neg[2]


#Caffeine
means_contaminants$Caffeine  <- fit_Caffeine$coefs_mean$mod_exp_neg[3]
lower_contaminants_conf$Caffeine  <- fit_Caffeine$confint_mean$mod_exp_neg[3,1]
upper_contaminants_conf$Caffeine  <- fit_Caffeine$confint_mean$mod_exp_neg[3,2]
means_contaminants_b$Caffeine <- fit_Caffeine$coefs_mean$mod_exp_neg[2]


#Paracetamol
means_contaminants$Paracetamol  <- fit_Paracetamol$coefs_mean$mod_exp_neg[3]
lower_contaminants_conf$Paracetamol  <- fit_Paracetamol$confint_mean$mod_exp_neg[3,1]
upper_contaminants_conf$Paracetamol  <- fit_Paracetamol$confint_mean$mod_exp_neg[3,2]
means_contaminants_b$Paracetamol <- fit_Paracetamol$coefs_mean$mod_exp_neg[2]



means_contaminants <- unlist(means_contaminants)
means_contaminants_b <- unlist(means_contaminants_b)
lower_contaminants_conf <- unlist(lower_contaminants_conf)
upper_contaminants_conf <- unlist(upper_contaminants_conf)

upper_contaminants_conf[is.na(upper_contaminants_conf)] <- 0
lower_contaminants_conf[is.na(lower_contaminants_conf)] <- NA
upper_contaminants_conf[is.na(lower_contaminants_conf)] <- NA

ord <- order(means_contaminants)

upper_contaminants_conf <- upper_contaminants_conf[ord]
lower_contaminants_conf <- lower_contaminants_conf[ord]
means_contaminants <- means_contaminants[ord]

names_contaminants <- names(means_contaminants)
names_contaminants <- gsub("\\.c","_", names_contaminants)
names_contaminants <- gsub('_c',"", names_contaminants)
names_contaminants <- gsub("_"," ",names_contaminants)

names_contaminants[names_contaminants=="24D "] <- "2,4-D "

png("Resultados/Contaminantes/Curvas/Negative_Exponential_c.png", width = 9, height = 9, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,8,1,1))
My_coefplot(means_contaminants, upper_contaminants_conf, lower_contaminants_conf, species_labels = names_contaminants, xlab = "Negative Exponential: c",
            cex.axis = 1,y_spa = 0)#break.axis = c(-0.5,-3.4), at.xaxis = c(0,-0.1,-0.2,-0.3,-0.4,-3.5,-3.6,-3.7,-3.8))
dev.off()







############################################################################
############################################################################
############################################################################


means_antidepressants <- list()
lower_antidepressants_conf <- list()
upper_antidepressants_conf <- list()

#O_desmethylvenlafaxine
means_antidepressants$O_desmethylvenlafaxine <- fit_O_desmethylvenlafaxine$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$O_desmethylvenlafaxine <- fit_O_desmethylvenlafaxine$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$O_desmethylvenlafaxine <- fit_O_desmethylvenlafaxine$confint_mean$mod_exp_neg[3,2]


#Hydroxybupropion
means_antidepressants$Hydroxybupropion <- fit_Hydroxybupropion$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$Hydroxybupropion <- fit_Hydroxybupropion$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$Hydroxybupropion <- fit_Hydroxybupropion$confint_mean$mod_exp_neg[3,2]

#Bupropion
#means_antidepressants$Bupropion <- fit_Bupropion$coefs_mean$mod_exp_neg[3]
#lower_antidepressants_conf$Bupropion <- fit_Bupropion$confint_mean$mod_exp_neg[3,1]
#upper_antidepressants_conf$Bupropion <- fit_Bupropion$confint_mean$mod_exp_neg[3,2]

#Venlafaxine
means_antidepressants$Venlafaxine <- fit_Venlafaxine$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$Venlafaxine <- fit_Venlafaxine$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$Venlafaxine <- fit_Venlafaxine$confint_mean$mod_exp_neg[3,2]

#fit_Desmethylcitalopram
means_antidepressants$Desmethylcitalopram <- fit_Desmethylcitalopram$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$Desmethylcitalopram <- fit_Desmethylcitalopram$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$Desmethylcitalopram <- fit_Desmethylcitalopram$confint_mean$mod_exp_neg[3,2]

#Citalopram
#means_antidepressants$Citalopram <- fit_Citalopram$coefs_mean$mod_exp_neg[3]
#lower_antidepressants_conf$Citalopram <- fit_Citalopram$confint_mean$mod_exp_neg[3,1]
#upper_antidepressants_conf$Citalopram <- fit_Citalopram$confint_mean$mod_exp_neg[3,2]

#Fluoxetine
means_antidepressants$Fluoxetine <- fit_Fluoxetine$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$Fluoxetine <- fit_Fluoxetine$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$Fluoxetine <- fit_Fluoxetine$confint_mean$mod_exp_neg[3,2]

#Amitriptyline
means_antidepressants$Amitriptyline <- fit_Amitriptyline$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$Amitriptyline <- fit_Amitriptyline$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$Amitriptyline <- fit_Amitriptyline$confint_mean$mod_exp_neg[3,2]

#Sertraline
means_antidepressants$Sertraline <- fit_Sertraline$coefs_mean$mod_exp_neg[3]
lower_antidepressants_conf$Sertraline <- fit_Sertraline$confint_mean$mod_exp_neg[3,1]
upper_antidepressants_conf$Sertraline <- fit_Sertraline$confint_mean$mod_exp_neg[3,2]




means_antidepressants <- unlist(means_antidepressants)
lower_antidepressants_conf <- unlist(lower_antidepressants_conf)
upper_antidepressants_conf <- unlist(upper_antidepressants_conf)

upper_antidepressants_conf[is.na(upper_antidepressants_conf)] <- 0
lower_antidepressants_conf[is.na(lower_antidepressants_conf)] <- NA
upper_antidepressants_conf[is.na(lower_antidepressants_conf)] <- NA

ord <- order(means_antidepressants)

upper_antidepressants_conf <- upper_antidepressants_conf[ord]
lower_antidepressants_conf <- lower_antidepressants_conf[ord]
means_antidepressants <- means_antidepressants[ord]

names_antidepressants <- names(means_antidepressants)
names_antidepressants <- gsub("\\.c","_", names_antidepressants)
names_antidepressants <- gsub('_c',"", names_antidepressants)
names_antidepressants <- gsub("_"," ",names_antidepressants)



png("Resultados/Fármacos/Curvas/antidepressants_Negative_Exponential_c.png", width = 9, height = 9, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,8,1,1))
My_coefplot(means_antidepressants, upper_antidepressants_conf, lower_antidepressants_conf, species_labels = names_antidepressants, xlab = "Negative Exponential: c", cex.axis = 1,y_spa = 0,)
dev.off()






############################################################################
############################################################################
############################################################################


means_sarscov2 <- list()
means_sarscov2_b <- list()
lower_sarscov2_conf <- list()
upper_sarscov2_conf <- list()

#sarscov2_n1
means_sarscov2$sarscov2_n1 <- fit_sarscov2_n1$coefs_mean$mod_exp_neg[3]
lower_sarscov2_conf$sarscov2_n1 <- fit_sarscov2_n1$confint_mean$mod_exp_neg[3,1]
upper_sarscov2_conf$sarscov2_n1 <- fit_sarscov2_n1$confint_mean$mod_exp_neg[3,2]
means_sarscov2_b$sarscov2_n1 <- fit_sarscov2_n1$coefs_mean$mod_exp_neg[2]


#sarscov2_n2
means_sarscov2$sarscov2_n2 <- fit_sarscov2_n2$coefs_mean$mod_exp_neg[3]
lower_sarscov2_conf$sarscov2_n2 <- fit_sarscov2_n2$confint_mean$mod_exp_neg[3,1]
upper_sarscov2_conf$sarscov2_n2 <- fit_sarscov2_n2$confint_mean$mod_exp_neg[3,2]
means_sarscov2_b$sarscov2_n2 <- fit_sarscov2_n2$coefs_mean$mod_exp_neg[2]



means_sarscov2 <- unlist(means_sarscov2)
means_sarscov2_b <- unlist(means_sarscov2_b)
lower_sarscov2_conf <- unlist(lower_sarscov2_conf)
upper_sarscov2_conf <- unlist(upper_sarscov2_conf)

upper_sarscov2_conf[is.na(upper_sarscov2_conf)] <- 0
lower_sarscov2_conf[is.na(lower_sarscov2_conf)] <- NA
upper_sarscov2_conf[is.na(lower_sarscov2_conf)] <- NA

ord <- order(means_sarscov2)

upper_sarscov2_conf <- upper_sarscov2_conf[ord]
lower_sarscov2_conf <- lower_sarscov2_conf[ord]
means_sarscov2 <- means_sarscov2[ord]

names_sarscov2 <- names(means_sarscov2)
names_sarscov2 <- gsub("\\.c","_", names_sarscov2)
names_sarscov2 <- gsub('_c',"", names_sarscov2)
names_sarscov2 <- gsub("_"," ",names_sarscov2)

names_sarscov2 <- c("SARS-Cov-2 N2", "SARS-Cov-2 N1") 


png("Resultados/SARSCOV2/Curvas/sarscov2_Negative_Exponential_c.png", width = 9, height = 9, units = "cm", pointsize = 9, res = 600)
par(mar = c(4,8,1,1))
My_coefplot(means_sarscov2, upper_sarscov2_conf, lower_sarscov2_conf, species_labels = names_sarscov2, xlab = "Negative Exponential: c", cex.axis = 1,y_spa = 0,)
dev.off()

#Tirar ficocianina
#tirar condutividade
#?Tirar redox potential?


#tirar todos os antidepressivos, manter somatórias e número de antidepressivos

#tirar os que tem variaveis onde mais de 75% dos valores é zero
#Triclosan
#X2HA
#Carbofuran
#Ametryn
#Azoxistrobin
#BPA
#DIA
#Tebutiuron
#Fipronil
#24D

means
upper_conf
lower_conf
names


means_sarscov2
upper_sarscov2_conf
lower_sarscov2_conf


means_contaminants
upper_contaminants_conf
lower_contaminants_conf


means_n_pharma
upper_n_pharma_conf
lower_n_pharma_conf

all_means <- c(means, NA, means_contaminants, NA, means_n_pharma, NA, means_sarscov2)
all_means_b <- c(means_b, NA, means_contaminants_b, NA, means_n_pharma_b, NA, means_sarscov2_b)

all_upper_conf <- c(upper_conf,NA, upper_contaminants_conf,NA,upper_n_pharma_conf,NA, upper_sarscov2_conf)
all_lower_conf <- c(lower_conf,NA, lower_contaminants_conf,NA, lower_n_pharma_conf,NA, lower_sarscov2_conf)
all_names <- c(names,NA, names_contaminants,NA, names_n_pharma,NA, names_sarscov2)


png("Resultados/Negative_Exponential_c.png", width = 12, height = 12, units = "cm", pointsize = 9, res = 600)

par(mar = c(4,10,1,1))
My_coefplot(all_means, all_upper_conf, all_lower_conf, species_labels = all_names, xlab = "Negative Exponential: c", cex.axis = 1,y_spa = 0, xlim = c(-0.7, 0))
abline(h = c(3, 10, 16), col = "grey50", lwd = 2)
text(x = -0.7, y = 27, labels = "Water", adj = c(0,0.5))
text(x = -0.7, y = 26, labels = "parameters", adj = c(0,0.5))
text(x = -0.7, y = 15, labels = "Contaminants", adj = c(0,0.5))
text(x = -0.7, y = 9, labels = "Pharmaceuticals", adj = c(0,0.5))
text(x = -0.7, y = 2, labels = "SARS-Cov-2", adj = c(0,0.5))

dev.off()
