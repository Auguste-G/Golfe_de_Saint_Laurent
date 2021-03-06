#########################
### Projet St Laurent ###
#########################

rm(list = ls()) # Clear Global Environnement

#Importation des packages
library(knitr)      # Format table dans les sorties
library(ggplot2)    # Pour de beaux graphiques
library(corrplot)   # Pour avoir une repr�sentation des corr�lations
library(tidyr)      # Pour supprimer les NA facilement
library(FactoMineR) # our r�aliser l'ACP
library(factoextra) # Pour repr�senter les r�sultats issus de FactoMiner
library(car)        # Pour analyser les sommes des carr�s
library(emmeans)    # moyennes ajust�es
library(RgoogleMaps)# Pour faire de jolies cartes
library(plotrix)    # Pour faire des floating sur la map
library(plyr)       # Pour produire des r�sum�s 


### I. Manipulation des donn�es ###
###################################

# Objectif 1: D�crire l'�volution temporelle de la biomasse ainsi que son organisation en fonction de covariables environnementales.
# Objectif 2: Pr�dire la r�ponse d'une esp�ce � un changement climatique ou une d�formation du milieu de vie 
  
### A. Importation du jeu de donn�es ###
###----------------------------------###

laurent <- read.csv("C:/Users/Gardette/Documents/APT/2A/SIMM/Statistiques/St_laurent/StLaurent.csv", header = TRUE, sep=";", dec = ",")

# Visualisation des donn�es brutes
knitr::kable(head(laurent))
summary(laurent)   #Les variables ont toutes �taient consid�r�es comme quantitative ! 
dim(laurent) # Il y a 452 lignes et 12 colonnes

# Conversion des variables qualitatives
laurent$year <- as.factor(laurent$year)
laurent$strate <- as.factor(laurent$strate)
laurent$sediment <- as.factor(laurent$sediment)
laurent$starfish <- as.factor(laurent$starfish)
laurent$urchin <- as.factor(laurent$urchin)

# V�rification
summary(laurent)  # Attention pr�sence de 6 NA dans les temp�ratures 

# Elimination des lignes o� la temp�rature est manquante
laurent<- drop_na(laurent)
dim(laurent)
summary(laurent)


### B. Visualisation des variables ###
###--------------------------------###


dev.off()
par(mfrow = c(1,1))

hist(laurent$invertebrate, xlab = "Indice de pr�sence d'invert�br�s", ylab = 'Occurence', 
     main = "R�partition de l'indice invert�br�s", breaks = 30, col = 'red')
hist(laurent$totconsum, xlab = "Indice de pr�sence de pr�dateur", ylab = 'Occurence', 
     main = "R�partition de l'indice de pr�dateur", breaks = 30, col = 'red')

# R�cup�ration de la position et de la carte de notre jeu de donn�e ia la library RgoogleMaps
bbox  <- qbbox( lat = laurent$latitude, lon = laurent$longitude)
nzoom <- min(MaxZoom(bbox$latR, bbox$lonR, size = c(640,640))) - 1
MyMap <- GetMap(center = c(mean(bbox$latR), mean(bbox$lonR)), zoom = nzoom, maptype = "terrain" , destfile =paste("carte.png", sep =""))

#R�partition des zones de pr�l�vement
PlotOnStaticMap(MyMap, lat = laurent$latitude, lon = laurent$longitude, pch = 21, bg="#8073ac80", col = "#2d004b", cex = 1)
title(main = list("Postion des lieux de pr�l�vement", cex = 1, font = 2), line = -1.5)


### 1. Analyse des variables quantitatives ###
###========================================###


dev.off()
par(mfrow = c(1,1))

#profondeur
cor.test(laurent$invertebrate,laurent$depth)
plot(x = laurent$depth, y = laurent$invertebrate, xlab="profondeur ",ylab = "Invert�br�s", main="Profondeur en fonction des invert�br�s", col="red")
hist(laurent$depth, xlab = "profondeur", ylab = 'Occurence', main = "Repartititon des valeurs de profondeur", breaks = 30, col='red')

#latitude
cor.test(laurent$invertebrate,laurent$latitude)
plot(x = laurent$latitude, y = laurent$invertebrate, xlab="latitude ",ylab = "Invert�br�s", main="latitude en fonction des invert�br�s", col="red")
hist(laurent$latitude, xlab = "latitude", ylab = 'Occurence', main = "Repartititon des valeurs de latitude", breaks = 30, col='red')

#longitude
cor.test(laurent$invertebrate,laurent$longitude)
plot(x = laurent$longitude, y = laurent$invertebrate, xlab="longitude ",ylab = "Invert�br�s", main="longitude en fonction des invert�br�s", col="red")
hist(laurent$longitude, xlab = "longitude", ylab = 'Occurence', main = "Repartititon des valeurs de longitude", breaks = 30, col='red')

hist( laurent$invertebrate , laurent$longitude )



#temperature
cor.test(laurent$invertebrate,laurent$temperature)
plot(x = laurent$temperature, y = laurent$invertebrate, xlab="temperature ",ylab = "Invert�br�s", main="temperature en fonction des invert�br�s", col="red")
hist(laurent$temperature, xlab = "temperature", ylab = 'Occurence', main = "Repartititon des valeurs de temperature", breaks = 30, col='red')

#dtow
cor.test(laurent$invertebrate,laurent$dtow)
plot(x = laurent$dtow, y = laurent$invertebrate, xlab="dtow ",ylab = "Invert�br�s", main="temperature en fonction de dtow", col="red")
hist(laurent$dtow, xlab = "dtow", ylab = 'Occurence', main = "Repartititon des valeurs de dtow", breaks = 30, col='red')
   
 
### 2. Analyse des variables qualitatives ###
###=======================================###


#S�diment
boxplot(laurent$invertebrate ~ laurent$sediment, ylab="Indice invert�br�", xlab="Typde de d�diment",
        main="R�partition des invert�br�s selon le type de s�diment",col="gold",cex.main=1.3, cex.lab=1.2)

#Ann�e 
boxplot(laurent$invertebrate ~ laurent$year, ylab="Indice invert�br�", xlab="Ann�e",
        main="R�partition des invert�br�s selon les ann�es",col="gold",cex.main=1.3, cex.lab=1.2)

#Etoiles de mer
boxplot(laurent$invertebrate ~ laurent$starfish, ylab="Indice invert�br�",xlab="Absence/Pr�sence d'�toile de mer",
        main="R�partition des invert�br�s selon la pr�sence d'�toile de mer",col="gold",cex.main=1.3, cex.lab=1.2)

#Oursins
boxplot(laurent$invertebrate ~ laurent$urchin, ylab="Indice invert�br�",
        xlab="Absence/Pr�sence d'oursins",main="R�partition des invert�br�s selon la pr�sence d'oursins",col="gold",cex.main=1.3, cex.lab=1.2)

#Strates
boxplot(laurent$invertebrate ~ laurent$strate, ylab="Indice invert�br�", xlab="Strates",
        main="R�partition des invert�br�s selon les strates",col="gold",cex.main=1.3, cex.lab=1.2)


### C. Nettoyage du jeu de donn�e ###
###-------------------------------###

### 1. Modification des donn�es ###
###=============================###


#Cr�ation d'une nouvelle table pour r�aliser les modifications et conserver l'origine
newlaurent  <- laurent

# Elimination des lignes correspondants aux s�diments de type 5
newlaurent  <- newlaurent[newlaurent$sediment != "5",]

# Suppression des valeurs influentes de totconsum 
newlaurent  <- newlaurent[newlaurent$totconsum < 1.5,]

# Suppression des valeurs influentes de l'indice d'invert�br�
newlaurent  <- newlaurent[newlaurent$invertebrate < 170,]

# Suppression des valeurs influentes de profondeur
newlaurent  <- newlaurent[newlaurent$depth < 500,]

# Prise en compte de la variable dtow pour les indices d'invert�br� et pr�dateur
newlaurent$invertebrate <- newlaurent$invertebrate/newlaurent$dtow
newlaurent$totconsum    <- newlaurent$totconsum/newlaurent$dtow

# Elimination de la variable dtow devenue obsol�te 
newlaurent <- subset(newlaurent, select = -dtow)


### 2. V�rification de l'impact des modifications ###
###===============================================###


#S�diment
boxplot(newlaurent$invertebrate ~ newlaurent$sediment, ylab="Indice invert�br�", xlab="Typde de d�diment",
        main="R�partition des invert�br�s selon le type de s�diment",col="gold",cex.main=1.3, cex.lab=1.2)

boxplot(newlaurent$totconsum ~ newlaurent$sediment, ylab="Indice Pr�dateur", xlab="Typde de d�diment",
        main="R�partition des Pr�dateur selon le type de s�diment",col="gold",cex.main=1.3, cex.lab=1.2)

#Indices
hist(newlaurent$invertebrate, xlab = "Indice de pr�sence d'invert�br�s", ylab = 'Occurence', main = 'R�partition de l indice invert�br�s', breaks = 30, col = 'red')
hist(newlaurent$totconsum, xlab = "Indice de pr�sence de pr�dateur", ylab = 'Occurence', main = 'R�partition de l indice de pr�dateur', breaks = 30, col = 'red')

# Suppression de la valeurs influente de totconsum 
newlaurent  <- newlaurent[newlaurent$totconsum < 1,]

# Suppression de la valeurs influente de l'indice d'invert�br�
newlaurent  <- newlaurent[newlaurent$invertebrate < 50,]

#Indices apr�s nettoyage
hist(newlaurent$invertebrate, xlab = "Indice de pr�sence d'invert�br�s", ylab = 'Occurence', main = 'R�partition de l indice invert�br�s', breaks = 30, col = 'red')
hist(newlaurent$totconsum, xlab = "Indice de pr�sence de pr�dateur", ylab = 'Occurence', main = 'R�partition de l indice de pr�dateur', breaks = 30, col = 'red')


### 3. Pr�paration pour les r�gressions lin�aires ###
###===============================================###


# Pr�sence de valeur o� notre indicateur est �gal a 0. Donc n�cessit� de supprimer/modifier cette valeur avant de passer en log
# Choix de la garder et de donner la premi�re valeur rencontr� au dessus du 0
newlaurent$invertebrate[newlaurent$invertebrate == 0] <- min(newlaurent$invertebrate[newlaurent$invertebrate > 0])
newlaurent$totconsum[newlaurent$totconsum == 0] <- min(newlaurent$totconsum[newlaurent$totconsum > 0])

# Cr�ation nouvelle variable en log d'invert�br� et Totconsum
newlaurent$loginv <- log10(newlaurent$invertebrate)
newlaurent$logcon <- log10(newlaurent$totconsum)


### II. Analyse des corr�lations ###
####################################

### A. Corr�lation des variables quantitatives ###
###--------------------------------------------###


correlation1 <- cor(newlaurent[,c(3:7,9)])
kable(correlation1,digits = 2)
par(mfrow = c(1,1))
corrplot(correlation1)


### B. Analyse en composantes principales ###
###---------------------------------------###


# Cr�ation d'un sous-Jeu de donn�e avec les variables quantitatives uniquement
laurentPCA <- newlaurent[, c(3:7,9)]

# ACP
resPCA <- PCA(laurentPCA, scale.unit = TRUE,ncp = 5, graph = F) 

#Etude des valeurs propres 
get_eigenvalue(resPCA)
fviz_eig(resPCA, addlabels = TRUE, ylim = c(0,45))

#Etude des variables
var<- get_pca_var(resPCA)
fviz_pca_var(resPCA, geom = c("text","arrow"), col.var = "cos2", axes = 1:2) + theme_classic()


### III. Influence des facteurs biotiques ###
#############################################

### A. Analyse des param�tres biotiques quantitatifs ###
###--------------------------------------------------###


dev.off()
par(mfrow = c(2,2))

### Regression lin�raire simple
# Analyse par rapport � la pr�sence des pr�dateurs (totconsum)
reg.con <- lm(logcon  ~ invertebrate , data = newlaurent)
plot(reg.con)
summary(reg.con)
# P value proche de 0.05, l�g�rement au dessus. Difficile de conclure 


### B. Analyse des param�tres biotiques qualitatifs  ###
###--------------------------------------------------###

# Pr�sentation du mod�le
table(newlaurent$starfish, newlaurent$urchin)

### 1. Oursins ###
###============###


### Regression lin�aire logistique 
par(mfrow = c(1,1))
reglog.urch = glm(urchin ~ temperature + depth + latitude + longitude + year + starfish + sediment + invertebrate, family = binomial(link = logit), data = newlaurent)
summary(reglog.urch)
# Cartaines variables quantitative peuvent expliquer la pr�sence d'oursin: Temperature, depth, latitude, invertebrate 
# Certaines varaibles qualitative peuvent expliquer la pr�sence d'oursin : s�diment 1-3-4 et certaines strates  (pas d'influence des ann�es)


### 2. Etoile de mer ###
###==================###


### Regression lin�aire logistique 
par(mfrow = c(1,1))
reglog.etoi = glm(starfish ~ temperature + depth + latitude + longitude + year + urchin + sediment + invertebrate, family = binomial(link = logit), data = newlaurent)
summary(reglog.etoi)
# Qu"une seule variable quantitative peut expliquer la pr�sence d'oursin : la Temperature
# Certaines varaibles qualitative peuvent expliquer la pr�sence d'oursin : La pr�sence d'oursin et la quantit� d'invert�br�


### IV. Impact des facteurs abiotiques ###
##########################################

### A. Etude des varaibles quantitatives abiotiques ###
###-------------------------------------------------###

dev.off()
par(mfrow = c(2,2))

### Regression lin�aire multiple 
reg.multi.quanti <- lm(loginv~longitude+latitude+depth+temperature+totconsum, data = newlaurent)
plot(reg.multi.quanti)
summary(reg.multi.quanti)


### B. Etude des varaibles qualitatives abiotiques ###
###------------------------------------------------###

### 1. Visualisation cartographique ###
###=================================###


dev.off()
par(mfrow = c(1,1))

# Cartographie des lieux de pr�l�vement en fonction de l'indice d'invert�br� colorier par ann�e
PlotOnStaticMap(MyMap)
for(n in 1:nrow(newlaurent)){
  newcoord  <- LatLon2XY.centered(MyMap, lat = newlaurent[n,"latitude"], lon = newlaurent[n, "longitude"], zoom =  nzoom)
  r         <- newlaurent[n, "invertebrate"]/3
  couleur   <- if(newlaurent$year[n] == "2003") "red" else if(newlaurent$year[n] == "2004")   "green" else if(newlaurent$year[n] == "2005") "blue"
  floating.pie(xpos = newcoord$newX, newcoord$newY, x = 100, radius = r, col = couleur, lwd = 2) 
}
legend("bottomleft",title = "Year", legend = c('2003', '2004', '2005'), fill = c('red','green','blue') , bg ="white", cex=0.6)
title(main = list("Indice d'invert�br� selon l'ann�e", cex = 1, font = 2), line = -1.5)


### 2. Test Anova � 1 facteur ###
###===========================###

dev.off()
par(mfrow = c(2,2))

### a. Mod�lisation effet des ann�es

mod.year = lm(loginv ~ year, data = newlaurent)
summary(mod.year)
plot(mod.year)
anova(mod.year)
Anova(mod.year) # L'ann�e ne semble pas significative significatif 

### b. Mod�lisation effet des strates

mod.strate = lm(loginv ~ strate, data = newlaurent)
summary(mod.strate)
plot(mod.strate)
anova(mod.strate)
Anova(mod.strate)# Les strates semblent avoir un effet significatif sur notre indice d'invert�br�

### c. Mod�lisation effet des s�diments

# Visualisation densit� des invert�br�s selon le type de s�diment
dev.off()
par(mfrow = c(1,1))
PlotOnStaticMap(MyMap)
for(n in 1:nrow(newlaurent)){
  newcoord  <- LatLon2XY.centered(MyMap, lat = newlaurent[n,"latitude"], lon = newlaurent[n, "longitude"], zoom =  nzoom) #R�cup�re la position
  r         <- newlaurent[n, "invertebrate"]/3
  couleur   <- if(newlaurent$sediment[n] == 1) "red" else if(newlaurent$sediment[n] == 2)   "green" else if(newlaurent$sediment[n] == 3) "blue" else if(newlaurent$sediment[n] == 4) "yellow"
  floating.pie(xpos = newcoord$newX, newcoord$newY, x = 100, radius = r, col = couleur, lwd = 2) 
}
legend("bottomleft",title = "Type de Sediment", legend = c('1', '2', '3', '4'), fill = c('red','green','blue','yellow') , bg ="white", cex=0.6)
title(main = list("Indice d'invert�br� selon le type de s�diment", cex = 1, font = 2), line = -1.5)

#Mod�lisation
dev.off()
par(mfrow = c(2,2))
mod.sedim = lm(loginv ~ sediment, data = newlaurent)
summary(mod.sedim)
plot(mod.sedim)
anova(mod.sedim)
Anova(mod.sedim) # Les s�diments semblent avoir un effet significatif sur notre indice d'invert�br�


### 3. Test Anova � 2 facteur ###
###===========================###

dev.off()
par(mfrow = c(2,2))

# Pr�sentation du mod�le
table(newlaurent$sediment, newlaurent$strate)

# Mod�lisation : Uniquement avec strate et s�diment comme les ann�es ne sont pas influentes
mod.strate.sedim = lm(loginv ~ strate + sediment, data = newlaurent)
summary(mod.strate.sedim)
plot(mod.strate.sedim)
anova(mod.strate.sedim)
Anova(mod.strate.sedim)


### 4. Test de comparaison de moyenne ###
###===================================###


strate.lsms <- lsmeans(mod.strate.sedim, pairwise ~ strate, adjust = "bonf") 
sedim.lsms <- lsmeans(mod.strate.sedim, pairwise ~ sediment, adjust. = "bonf") 

strate.lsms
sedim.lsms 

plot(strate.lsms, comparisons = TRUE) # Seulement certaines strates ont une moyenne diff�rente
plot(sedim.lsms, comparisons = TRUE) # Les s�diments 4 et 2 ont une moyenne identique


### 5. Test de comparaison de moyenne ajust�e ###
###===========================================###


lsmeans(mod.strate.sedim, pairwise ~ sediment, p.adjust.methods = "bonf")
lsmeans(mod.strate.sedim, pairwise ~ strate, p.adjust.methods = "bonf")

#Quand est-ce qu'on ajuste ou pas ???


### V. Mod�le final ###
#######################

### A. Mod�lisation de l'indice d'invert�br� : Mod�le Ancova ###
###----------------------------------------------------------###

# Influence de param�tres quantitatifs : lattitude, depth, temp�rature
# Influence de param�tres qualitatifs : S�diment

dev.off()
par(mfrow = c(2,2))

### 1. Comparaison des mod�les ###
###============================###


mod.final.0 <- lm(loginv ~ 1, data = newlaurent)
ggplot(newlaurent, aes(x=temperature, y = loginv, shape = sediment, color = sediment)) + geom_point() + theme_bw()


# Mod�le 1
mod.final.1 <- lm(loginv ~ sediment*(latitude + temperature + depth), data = newlaurent  )
anova(mod.final.0,mod.final.1)
anova(mod.final.1)
Anova(mod.final.1)

# Mod�le 2
mod.final.2 <- lm(loginv ~ sediment + latitude + temperature  + depth  + sediment:depth +  sediment:latitude, data = newlaurent  )
anova(mod.final.2,mod.final.1)
anova(mod.final.2)
Anova(mod.final.2)


#  Mod�le 3 
mod.final.3 <- lm(loginv ~ sediment + latitude + temperature  + depth +  sediment:depth, data = newlaurent  )
anova(mod.final.3,mod.final.2)
anova(mod.final.3)
Anova(mod.final.3)

#  Mod�le 4 
mod.final.4 <- lm(loginv ~ sediment + latitude + temperature  + depth , data = newlaurent  )
anova(mod.final.4,mod.final.2)
anova(mod.final.4,mod.final.3)
anova(mod.final.4)
Anova(mod.final.4)
summary(mod.final.4)

#  Mod�le 5 
mod.final.5 <- lm(loginv ~ sediment + latitude + temperature , data = newlaurent  )
anova(mod.final.5,mod.final.4)
Anova(mod.final.5)

#  Mod�le 6
mod.final.6 <- lm(loginv ~ sediment + latitude + temperature , data = newlaurent  )
anova(mod.final.5,mod.final.4)
Anova(mod.final.6)

### 2. Repr�sentation graphique du mod�le ###
###=======================================###


dev.off()
par(mfrow = c(1,1))

# R�sum� des moyennes de Newlaurent

summ  <- ddply(newlaurent,.(sediment),summarise,moypro = mean(depth), moylat = mean(latitude), moytemp = mean(temperature))
kable(summ)

moyloginv = mean(newlaurent$loginv)
moytemp = mean(newlaurent$temperature)
moypro = mean(newlaurent$depth)
moylat = mean(newlaurent$latitude)

newlaurent$pred1 = predict(mod.final.1)
newlaurent$pred2 = predict(mod.final.2)
newlaurent$pred3 = predict(mod.final.3)
newlaurent$pred4 = predict(mod.final.4)

# Graphiques

ggplot(newlaurent, aes(x=temperature, y = loginv, shape = sediment, color = sediment)) + geom_point() + geom_line(aes( y = pred1))  + theme_bw()
ggplot(newlaurent, aes(x=temperature, y = loginv, shape = sediment, color = sediment)) + geom_point() + geom_line(aes( y = pred2))  + theme_bw()
ggplot(newlaurent, aes(x=temperature, y = loginv, shape = sediment, color = sediment)) + geom_point() + geom_line(aes( y = pred3))  + theme_bw()
ggplot(newlaurent, aes(x=temperature, y = loginv, shape = sediment, color = sediment)) + geom_point() + geom_line(aes( y = pred4)) + geom_vline(data = summ, aes(xintercept = moytemp, color = sediment, linetype = sediment)) + geom_vline(aes(xintercept=moytemp), linetype = 1 , color = "black") + theme_bw()


#  Autres Graphiques 

ggplot(newlaurent, aes(x=depth, y = loginv, shape = sediment, color = sediment)) + geom_point() + geom_line(aes( y = pred4)) + geom_vline(data = summ, aes(xintercept = moypro, color = sediment, linetype = sediment)) + geom_vline(aes(xintercept=moypro), linetype = 1 , color = "black") + theme_bw()

ggplot(newlaurent, aes(x=latitude, y = loginv, shape = sediment, color = sediment)) + geom_point() + geom_line(aes( y = pred4)) +  geom_vline(data = summ, aes(xintercept = moylat, color = sediment, linetype = sediment)) + geom_vline(aes(xintercept=moylat), linetype = 1 , color = "black") + theme_bw()


### 3. Comparaison des plotes ###
###===========================###

dev.off()
par(mfrow = c(2,2))


plot(mod.final.1)
plot(mod.final.2)
plot(mod.final.3)
plot(mod.final.4)


### 4. Comparaison des moyennes ###
###=============================###

emmeans(mod.final.1, pairwise~sediment)
emmeans(mod.final.4, pairwise~sediment)

### B. Mod�lisation de la pr�sence d'oursins ###
###------------------------------------------###

dev.off()

mod.urch.final = glm(urchin ~ sediment + starfish + depth + latitude + temperature, family = binomial(link = logit), data = newlaurent)
summary(mod.urch.final)


### C. Mod�lisation de la pr�sence d'�toile de mer ###
###------------------------------------------------###


mod.etoi.final= glm(starfish ~ urchin + temperature + invertebrate, family = binomial(link = logit), data = newlaurent)
summary(mod.etoi.final)

