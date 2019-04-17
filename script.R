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

colnames(preOpRtu) <- labelsPreOpTraitee;
colnames(preOpVap) <- labelsPreOpTraitee;
colnames(preOpVbb) <- labelsPreOpTraitee;

##Analyse descriptive###

  ##Pré-op VAPOR 
    



## pour les dissimilarité, utilisé daisy (pour donnée hétérogène)
