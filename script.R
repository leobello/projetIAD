VAPOR = read.csv("data/VAPOR.csv", header = TRUE)

#RTUPB et VBBPS, methode similaire, peuvent être traité comme un seul ensemble de données
VBBPS = read.csv("data/VBBPS.csv", header = TRUE)
RTUPB = read.csv("data/RTUPB.csv", header = TRUE)

##extraction des colonnes pour traitement pré-operatoire 
#(seulement colonne 1 à 20 : de "age" à "reprise au bloc"
preOpVap = VAPOR[,c(1:20)]

preOpRtu = RTUPB[,c(1:20)]
preOpVbb = VBBPS[,c(1:20)]
#concatenation car methode similaire (traitée comme un seul ensemble de données).
preOp_RtETVb = rbind(preOpRtu, preOpVbb)


##recupère les labels des variables
labelsPreOp = colnames(preOpVap)


##pré OP => état de santé des patients à l'arrivé (état + ou - en forme)
## tous des cancers de la prostate avec degrée + ou - sevère
## trouver des critères qui distinguent les groupes de patients


##POST op => même combat

##Analyse descriptive###

  ##Pré-op VAPOR 
nrow(preOpVap)
# 40 individus dans la méthode VAPOR


mean(preOpVap$Age..ans.)
sd(preOpVap$Age..ans.)
## etude porte sur personne agés


hist(preOpVap$Durée.traitement.médical..mois.,main="Distribution de la durée du traitement médical",
     ylab="Effectifs",xlab="nombre de mois", xaxt="n", ylim=c(0,15),col="darkred",  labels = TRUE)
axis(1,seq(0,160,20))



table(preOpVap$Evenement.H.D, preOpVap$Transfusion.PerO)
##si il ya une perturbation arterielle durant Op,il y a eu transfusion
## ici seulement 2 transfusions sur les 40 patients


##IPSS => plus c'est élevé, plus patient gené
table(preOpVap$IPSS.P.O)

##QOL => plus c'est élevé, plus patient insatisfait
table(preOpVap$QoL.P.O)



par(mfrow=c(2,2))
indication = table(preOpVap$Indication)
colorsIndic = c("gray","lightgreen","blue","green","yellow","red","brown", "blue")

labelIndic = c("Retention Vésicale Algue",
               "Échec du traitement médical",
               "Retention vesicale chronique",
               "insuffisance rénale",
               "Lithiase vésicale",
               "diverticule",
               "hematurie",
               "infection")


pie(indication, labels=indication, col=colorsIndic, main="Répartition de la présence de caillotage")
legend("bottomleft", xpd = TRUE, legend = labelIndic, fill=colorsIndic)



caillotage = table(preOpVap$caillotage)
colors = c("gray","lightgreen")
pie(caillotage, labels=caillotage, col=colors, main="Répartition de la présence de caillotage")
legend("bottomleft", xpd = TRUE, legend = c("NON", "OUI"), fill=colors)


reprise = table(preOpVap$reprise.au.bloc)
colors = c("gray","lightyellow")
pie(reprise, labels=reprise, col=colors, main="Répartition des reprises au bloc")
legend("bottomleft", xpd = TRUE, legend = c("NON", "OUI"), fill=colors)


comorb = table(preOpVap$Comorbidité.CardioVx)
colors = c("gray","lightskyblue")
pie(comorb, labels=comorb, col=colors, main="Répartition de la présence de comorbidité")
legend("bottomleft", xpd = TRUE, legend = c("NON", "OUI"), fill=colors)



## pour les dissimilarité, utilisé daisy (pour donnée hétérogène)
## cmdscale
## library(cluster)


## utilisé au moins de méthode pour faire découpage des classes
## puis regardé si couple elem dans premier découpage se trouve dans une seul classe du deuxieme découpage ou pas
## ex P1 : 4classes, P2: 3classes
## confronter 2 partitions => rand(P1, P2)







