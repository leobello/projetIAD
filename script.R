VAPOR = read.csv("data/VAPOR.csv", header = TRUE)

#RTUPB et VBBPS, methode similaire, peuvent être traité comme un seul ensemble de données
VBBPS = read.csv("data/VBBPS.csv", header = TRUE)
RTUPB = read.csv("data/RTUPB.csv", header = TRUE)


#ligne vide entre 37 et 70 donc suppression de ces dernieres
RTUPB = RTUPB[-c(37:70),]

##extraction des colonnes pour traitement pré-operatoire 
#(seulement colonne 1 à 20 : de "age" à "reprise au bloc"
preOpVap = VAPOR[,c(1:20)]

preOpRtu = RTUPB[,c(1:20)]
preOpVbb = VBBPS[,c(1:20)]
#concatenation car methode similaire donc peuvent être traitée comme un seul ensemble de données.
preOp_RtETVb = rbind(preOpRtu, preOpVbb)


##recupère les labels sur la premiere ligne
labelsPreOp = colnames(preOpVap)



##Analyse descriptive###

  ##Pré-op VAPOR 


