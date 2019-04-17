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

##recupère les labels des variables
labelsPreOp = colnames(preOpVap)


##réecrire les labels
labelsPreOpTraitee = c("Age",
                  "Comorbidité cardio Vx",
                  "Durée traitement médical (mois)",
                  "Porteur de sonde",
                  "IPSS P.O",
                  "QoL P.O",
                  "Qmax P.0 (ml/s)",
                  "PSA (ng/ml)",
                  "Volume prostatique (ml)",
                  "Résidu post mictionnel (ml)",
                  "Indication",
                  "Anestésie",
                  "Evenement H.D",
                  "Technique",
                  "Transfusion PerO",
                  "Temps OP",
                  "Volume résequé (ml)",
                  "Délais ablation (jours",
                  "Caillotage",
                  "Reprise au bloc"
)

labelsPostOpTraitee = c("IPSS 1",
                       "QoL 1",
                       "Qmax 1 (ml/s)",
                       "IPSS 2",
                       "Qol 2",
                       "Qmax 2 (ml/s)",
                       "IPSS 3",
                       "Qol 3",
                       "Qmax 3 (ml/s)",
                       "IPSS 4",
                       "Qol 4",
                       "Qmax 4 (ml/s)",
                       "IPSS 5",
                       "Qol 5",
                       "Qmax 5 (ml/s)",
                       "IPSS 6",
                       "Qol 6",
                       "Qmax 6 (ml/s)",
                       "IPSS 7",
                       "Qol 7",
                       "Qmax 7 (ml/s)"
)

colnames(preOpRtu) <- labelsPreOpTraitee;
colnames(preOpVap) <- labelsPreOpTraitee;
colnames(preOpVbb) <- labelsPreOpTraitee;

colnames(postOpRtu) <- labelsPostOpTraitee;
colnames(postOpVap) <- labelsPostOpTraitee;
colnames(postOpVbb) <- labelsPostOpTraitee;

##Analyse descriptive###

  ##Pré-op VAPOR 
    

#Post-op VAPOR

#évolution de l'IPSS
boxplot(postOpVap$`IPSS 1`,
        postOpVap$`IPSS 2`,
        postOpVap$`IPSS 3`,
        postOpVap$`IPSS 4`,
        postOpVap$`IPSS 5`,
        postOpVap$`IPSS 6`,
        postOpVap$`IPSS 7`,
        names=c(1,3,6,9,12,15,18),
        main="Evolution de l'IPSS post-opération",
        xlab="Mois de mesure de l'IPSS")

#évolution de la moyenne de Qol

#extraction de toutes les mesures IPSS
ipssVap = postOpVap[,seq(1,21,3)];
#moyennes des IPSS (deuxième argument 1 pour ligne 2 pour colonne)
moyennes = apply(ipssVap,2,mean);


# plot(postOpVap$`IPSS 1`,
#      postOpVap$`IPSS 2`,
#      xlab = "IPSS 1",
#      ylab = "IPSS 2",
#      type = "l")

## pour les dissimilarité, utilisé daisy (pour donnée hétérogène)
