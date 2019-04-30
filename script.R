VAPOR = read.csv("data/VAPOR.csv", header = TRUE)

#RTUPB et VBBPS, methode similaire, peuvent être traité comme un seul ensemble de données
VBBPS = read.csv("data/VBBPS.csv", header = TRUE)
RTUPB = read.csv("data/RTUPB.csv", header = TRUE)

##extraction des colonnes pour traitement pré-operatoire 
#(seulement colonne 1 à 20 : de "age" à "reprise au bloc"
preOpVap = VAPOR[,c(1:20)]
preOpRtu = RTUPB[,c(1:20)]
preOpVbb = VBBPS[,c(1:20)]

#extraction des colonnes pour traitement post-opératoire
#colonne 21 à 41
postOpVap = VAPOR[,c(21:41)] 
postOpRtu = RTUPB[,c(21:41)]
postOpVbb = VBBPS[,c(21:41)]

#concatenation car methode similaire (traitée comme un seul ensemble de données).
preOp_RtETVb = rbind(preOpRtu, preOpVbb)
postOp_RtETVb = rbind(postOpRtu, postOpVbb)

#on eleve la colonne 14 qui correspond à la technique utilisée 
preOpVap = preOpVap[,-14]
preOp_RtETVb = preOp_RtETVb[,-14]


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

labelsPostOpTraitee = c("IPSS 1","QoL 1","Qmax 1 (ml/s)",
                        "IPSS 2","Qol 2","Qmax 2 (ml/s)",
                        "IPSS 3","Qol 3","Qmax 3 (ml/s)",
                        "IPSS 4","Qol 4","Qmax 4 (ml/s)",
                        "IPSS 5","Qol 5","Qmax 5 (ml/s)",
                        "IPSS 6","Qol 6","Qmax 6 (ml/s)",
                        "IPSS 7","Qol 7","Qmax 7 (ml/s)"
)

colnames(preOpVap) <- labelsPreOpTraitee;
colnames(preOp_RtETVb) <- labelsPreOpTraitee


colnames(postOpRtu) <- labelsPostOpTraitee;
colnames(postOpVap) <- labelsPostOpTraitee;
colnames(postOpVbb) <- labelsPostOpTraitee;


indication = c("Retention Vésicale Algue","Échec du traitement médical",
               "Retention vesicale chronique","insuffisance rénale",
               "Lithiase vésicale","diverticule",
               "hematurie","infection")
anesthesie = c("Anesthésie Loco-régionale",
               "Anésthésie Générale")
evenement_HD = c("non","hypotension","bradycardie","malaise vagual")
non_oui = c("non","oui")

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




##pré OP => état de santé des patients à l'arrivé (état + ou - en forme)
## tous des cancers de la prostate avec degrée + ou - sevère
## trouver des critères qui distinguent les groupes de patients

##POST op => même combat

##Analyse descriptive###

  ##Pré-op VAPOR 
#nature du jeu de donnée
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

par(mfrow=c(1,1))
hist(preOpVap$`Durée traitement médical (mois)`,main="Distribution de la durée du traitement médical",
     ylab="Effectifs",xlab="nombre de mois", xaxt="n", ylim=c(0,15),col="darkred",  labels = TRUE)
axis(1,seq(0,160,20))


boxplot(preOpVap$Age ~ preOpVap$`Porteur de sonde`,
        col = "purple", border = "black",
        main = "Porteur de sonde en fonction de l'âge",
        ylab = "âges[années]"
)

tapply(preOpVap$Age, preOpVap$`Porteur de sonde`, mean)
#non      oui 
#67.30769 76.07143 
#parmis les patients, ceux qui ne portaient pas de sonde ont en moyenne 67 ans, contre 76 ans pour ceux
#qui en étaient équipé.

par(mfrow=c(2,1))
##IPSS => plus c'est élevé, plus patient gené
table(preOpVap$`IPSS P.O`)
plot(jitter(preOpVap$Age), jitter(preOpVap$`IPSS P.O`))


##QOL => plus c'est élevé, plus patient insatisfait
table(preOpVap$`QoL P.O`)
plot(jitter(preOpVap$Age), jitter(preOpVap$`QoL P.O`))




par(mfrow=c(2,2))
bp = boxplot(preOpVap$Age ~ preOpVap$`Porteur de sonde`,
        col = "purple", border = "black",
        main = "Porteur de sonde en fonction de l'âge",
        ylab = "âges[années]")
text(seq(ncol(bp$stats))+0.1, bp$stats[1, ] + 1, bp$n, col = "red")

bp = boxplot(preOpVap$Age ~ preOpVap$Caillotage,
        col = "purple", border = "black",
        main = "Présence de caillotage en fonction de l'âge",
        ylab = "âges[années]"
)
text(seq(ncol(bp$stats))+0.1, bp$stats[1, ] + 1, bp$n, col = "red")

bp = boxplot(preOpVap$Age ~ preOpVap$`Reprise au bloc`,
        col = "purple", border = "black",
        main = "Reprise au bloc en fonction de l'âge",
        ylab = "âges[années]"
)
text(seq(ncol(bp$stats))+0.1, bp$stats[1, ] + 1, bp$n, col = "red")


bp = boxplot(preOpVap$Age ~ preOpVap$`Comorbidité cardio Vx`,
        col = "purple", border = "black",
        main = "Comorbidité en fonction de l'âge",
        ylab = "âges[années]"
)
text(seq(ncol(bp$stats))+0.1, bp$stats[1, ] + 1, bp$n, col = "red")




## pour les dissimilarité, utilisé daisy (pour donnée hétérogène)
## cmdscale
## library(cluster)


## utilisé au moins de méthode pour faire découpage des classes
## puis regardé si couple elem dans premier découpage se trouve dans une seul classe du deuxieme découpage ou pas
## ex P1 : 4classes, P2: 3classes
## confronter 2 partitions => rand(P1, P2)







