VAPOR = read.csv("VAPOR.csv", header = TRUE)

#RTUPB et VBBPS, methode similaire, peuvent être traité comme un seul ensemble de données
RTUPB = read.csv("RTUPB.csv", header = TRUE)
VBBPS = read.csv("VBBPS.csv", header = TRUE)

labelVapor <- attributes(VAPOR)$row.names
labelRTUPB <- attributes(RTUPB)$row.names
labelVBBS <- attributes(VBBPS)$row.names

boxplot(RTUPB)
plot(RTUPB)
plot(VAPOR)
