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

##TODO : réecrire les labels




##Analyse descriptive###

  ##Pré-op VAPOR 
    



## pour les dissimilarité, utilisé daisy (pour donnée hétérogène)
