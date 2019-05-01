VAPOR = read.csv("data/VAPOR.csv", header = TRUE)

##extraction des colonnes pour traitement pré-operatoire 
#(seulement colonne 1 à 20 : de "age" à "reprise au bloc"
preOpVap = VAPOR[,c(1:20)]

#extraction des colonnes pour traitement post-opératoire
#colonne 21 à 41
postOpVap = VAPOR[,c(21:41)] 

#on eleve la colonne 14 qui correspond à la technique utilisée 
preOpVap = preOpVap[,-14]

##réecrire les labels
labelsPreOpTraitee = c("Age","Comorbidité cardio Vx",
                       "Durée traitement médical (mois)","Porteur de sonde",
                       "IPSS P.O","QoL P.O","Qmax P.0 (ml/s)",
                       "PSA (ng/ml)","Volume prostatique (ml)",
                       "Résidu post mictionnel (ml)","Indication",
                       "Anestésie","Evenement H.D","Transfusion PerO",
                       "Temps OP","Volume résequé (ml)","Délais ablation (jours)",
                       "Caillotage","Reprise au bloc"
)

colnames(preOpVap) <- labelsPreOpTraitee;

### correspondance valeurs des variables indication,..
indication = c("Retention Vésicale Algue","Échec du traitement médical",
               "Retention vesicale chronique","insuffisance rénale",
               "Lithiase vésicale","diverticule",
               "hematurie","infection")
anesthesie = c("Anesthésie Loco-régionale",
               "Anésthésie Générale")
evenement_HD = c("non","hypotension","bradycardie","malaise vagual")
non_oui = c("non","oui")


####################### Remplace valeurs num par character #############
for(i in seq(1,length(anesthesie)))
  preOpVap$Anestésie[preOpVap$Anestésie==i]=anesthesie[i] 

for(i in seq(1,length(indication)))
  preOpVap$Indication[preOpVap$Indication==i]=indication[i] 

for(i in seq(0,length(evenement_HD)-1))
  preOpVap$`Evenement H.D`[preOpVap$`Evenement H.D`==i]=evenement_HD[i+1] 

for(i in seq(0,1)){
  preOpVap$`Comorbidité cardio Vx`[preOpVap$`Comorbidité cardio Vx`==i]<- non_oui[i+1]
  preOpVap$`Transfusion PerO`[preOpVap$`Transfusion PerO`==i]<- non_oui[i+1]
  preOpVap$Caillotage[preOpVap$Caillotage==i] <- non_oui[i+1]
  preOpVap$`Reprise au bloc`[preOpVap$`Reprise au bloc`==i]<- non_oui[i+1]
  preOpVap$`Porteur de sonde`[preOpVap$`Porteur de sonde`==i] <- non_oui[i+1]
}
##########################################################################

resetDf <- function(){
  VAPOR = read.csv("data/VAPOR.csv", header = TRUE)
  preOpVap = VAPOR[,c(1:20)]
  preOpVap = preOpVap[,-14]
  
  postOpVap = VAPOR[,c(21:41)] 
  
  colnames(preOpVap) <- labelsPreOpTraitee;
}



############################### Analyse descriptive ###############################

str(preOpVap)
summary(preOpVap)

median(preOpVap$Age)
#71
mean(preOpVap$Age)
#70.375  
sd(preOpVap$Age)
#6.554261
## etude porte sur personne agés
par(mfrow=c(2,1))
hist(preOpVap$Age)
qqnorm(preOpVap$Age)
qqline(preOpVap$Age)
#variable Age semble suivre une loi normal

critere_oui_non_en_fonction_de_age = function(critere, nomCritere){
  bp = boxplot(preOpVap$Age ~ critere,
               col = "khaki2", border = "black",
               main = paste(nomCritere, ""),
               ylab = "âges[années]"
  )
  text(seq(ncol(bp$stats))+0.1, bp$stats[1, ] + 1, bp$n, col = "red") 
  tapply(preOpVap$Age, critere, mean)
}
critere_oui_non_en_fonction_duree = function(critere, nomCritere){
  bp = boxplot(preOpVap$`Durée traitement médical (mois)` ~ critere,
               col = "darkseagreen", border = "black",
               main = paste(nomCritere, ""),
               ylab = "durée[mois]"
  )
  text(seq(ncol(bp$stats))+0.1, bp$stats[1, ] + 1, bp$n, col = "red") 
  tapply(preOpVap$`Durée traitement médical (mois)`, critere, mean)
}

par(mfrow=c(2,2))
critere_oui_non_en_fonction_de_age(preOpVap$`Porteur de sonde`, "Porteur de sonde")
critere_oui_non_en_fonction_de_age(preOpVap$Caillotage, "Présence de caillotage")
critere_oui_non_en_fonction_de_age(preOpVap$`Reprise au bloc`, "Reprise au bloc")
critere_oui_non_en_fonction_de_age(preOpVap$`Comorbidité cardio Vx`, "Comorbidité")

critere_oui_non_en_fonction_duree(preOpVap$`Porteur de sonde`, "Porteur de sonde")
critere_oui_non_en_fonction_duree(preOpVap$Caillotage, "Présence de caillotage")
critere_oui_non_en_fonction_duree(preOpVap$`Reprise au bloc`, "Reprise au bloc")
critere_oui_non_en_fonction_duree(preOpVap$`Comorbidité cardio Vx`, "Comorbidité")

par(mfrow=c(1,1))
##IPSS => plus c'est élevé, plus patient gené
plot(jitter(preOpVap$Age), jitter(preOpVap$`IPSS P.O`), main="IPSS P.O", xlab="âge[années]", ylab="")
#peut être lien ?

##QOL => plus c'est élevé, plus patient insatisfait
plot(jitter(preOpVap$Age), jitter(preOpVap$`QoL P.O`), main="Qol P.O", xlab="âge[années]", ylab="")
#peut être lien ?

hist(preOpVap$`Durée traitement médical (mois)`,main="Distribution de la durée du traitement médical",
     ylab="Effectifs",xlab="nombre de mois", xaxt="n", ylim=c(0,15),col="darkred",  labels = TRUE)
axis(1,seq(0,160,20))


plot(jitter(preOpVap$Age), jitter(preOpVap$`Durée traitement médical (mois)`),main="Durée du traitement en fonction de l'âge", xlab="âge[années]", ylab="Durée traitement[mois]")
#pas de lien visible


## pour les dissimilarité, utilisé daisy (pour donnée hétérogène)
## cmdscale
library(cluster)
preOpVap = VAPOR[,c(1:20)]
preOpVap = preOpVap[,-14]
colnames(preOpVap) <- labelsPreOpTraitee;

D=daisy(preOpVap, metric = "gower")

par(mfrow=c(2,1))
Cord2 = cmdscale(D)
plot(Cord2, type="p")

Cord3 = cmdscale(D, k=3)
plot(Cord3[,1],Cord3[,3], type="p")

## on affiche sur la premiere et troisieme dimensions pour avoir une autre vue. => permet d'avoir plus de recul sur certains
## points semble parfois proche mais ne le sont pas tant que ça.

############################### Clustering des données ###############################
vap = preOpVap[preOpVap$`Porteur de sonde`==0,]
#garder seulement les patients porteur de sonde
vap <- lapply(vap, FUN = as.numeric)
vap <- as.data.frame(vap)
colnames(vap) <- labelsPreOpTraitee;
vap$`Evenement H.D` = NULL
vap$`Porteur de sonde` = NULL
vap$`Transfusion PerO` = NULL

vap.scale = scale(vap)

A=daisy(vap.scale, metric = "euclidean")

par(mfrow=c(2,1))
Cord2 = cmdscale(A)
plot(Cord2, type="p")

Cord3 = cmdscale(A, k=3)
plot(Cord3[,1],Cord3[,3], type="p")


par(mfrow=c(1,1))
cah.ward <- hclust(A, method="ward.D2")
plot(cah.ward, cex = 0.7, hang = -1) # display dendrogram
groupes.cah <- cutree(cah.ward, k=3) # cut tree into 3 clusters
table(groupes.cah)
# draw dendogram with red borders around the 3 clusters
rect.hclust(cah.ward, k=3, border=2:5) 

cah.average <- hclust(A, method="average")
plot(cah.average, cex = 0.7, hang = -1) # display dendrogram
groupes.average <- cutree(cah.average, k=4) # cut tree into 4 clusters
table(groupes.average)
# draw dendogram with red borders around the 4 clusters
rect.hclust(cah.average, k=4, border=2:5) 

## Partionning Around Medoids  PAM
pam = pam(A, k=4)
pam
pam$silinfo
plot(pam)


pam3 = pam(A, k=3)
pam3
pam3$silinfo
plot(pam3)

## on part sur 4 classes




