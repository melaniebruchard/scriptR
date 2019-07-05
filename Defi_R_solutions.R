#Problème1
#création des vecteurs
vec1=rnorm(n=10)
vec2=rnorm(n=100)
vec3=rnorm(n=1000)

#création de vecteur contenant les histogrammes : mauvaise idée car non représentable graphiquement
hist_vec1 <- hist(vec1, col ="blue", xlab = "axe X", ylab = "axe y",main = "groupeB"  )
hist_vec2 <- hist(vec2 , col = "white", xlab = "Axe Z", ylab = "Axe 2",main = "Groupe B - 2ème")
hist_vec3 <- hist(vec3, col="red", xlab= "axe gaelle", ylab=" axe Pierre", main = "Bertrand")

#création d'un pdf de 3 pages avec un graph par page
pdf(file = "hist_vec1.pdf")  
hist(vec1, col ="blue", xlab = "axe X", ylab = "axe y",main = "groupeB"  )
dev.off()

#attention mettre le par après le pdf, par sert a diviser la feuille recevant les graphiques
par(mfrow=c(3,1))

#création du PDF avec les 3 graphs sur une seule page
pdf(file = "hist_allvec.pdf")  
par(mfrow=c(3,1))
hist(vec1, col ="blue", xlab = "axe X", ylab = "axe y",main = "groupeB"  )
hist(vec2 , col = "white", xlab = "Axe Z", ylab = "Axe 2",main = "Groupe B - 2ème")
hist(vec3, col="red", xlab= "axe gaelle", ylab=" axe Pierre", main = "Bertrand")
dev.off()

#Bonus
#création d'un vecteur définissant les limites des axes x pour tous les graphs pour harmonisation entre les graphs
seq <- seq(floor(min(c(vec1, vec2, vec3))), ceiling(max(c(vec1, vec2, vec3))), 0.5)

#création pdf avec 3 histo sur une page à la meme echelle
pdf(file = "hist_allvecsurunepage.pdf")  
par(mfrow=c(3,1))

hist(vec1, breaks = seq, col ="blue", xlab = "axe X", ylab = "axe y",main = "groupeB"  )
hist(vec2, breaks = seq, col = "white", xlab = "Axe Z", ylab = "Axe 2", main = "Groupe B - 2ème" )
hist(vec3, breaks = seq, col="red", xlab= "axe gaelle", ylab=" axe Pierre", main = "Bertrand")
dev.off()

#Problème2
#création des vecteurs
MLR<-read.table("MLR_Infos.txt")
mito <- read.table("Mito_Genes.txt", header = T)
MLR2<-read.table("MLR_Infos_2errors.txt")

#visualisation des vecteurs
View(mito)
View(MLR)

#obtenir les dimensions des vecteurs
dim(mito)
dim(MLR)
#la première colonne est sélectionnée pour mito
row.names(mito)<-mito[,1]
#rechercher les corrélations entre les deux fichiers (quelque soit les lignes)
row.names(MLR2) %in% row.names(mito)
#pour voir le résumé des résultats au lieu du tableau en entier
summary (row.names(MLR2) %in% row.names(mito))
row.names(mito) %in% row.names(MLR2)
summary(row.names(mito) %in% row.names(MLR2))
#pour déterminer quels sont les gènes différents (donne la ligne)
which(row.names(mito) %in% row.names(MLR2)== F )
which(row.names(MLR2) %in% row.names(mito) == F )
#pour donner le nom des gènes
row.names(MLR2)[c(26,48)]
row.names(mito)[c(26,48)]
#remplacer les lignes fausses de MLR2 par les lignes vraies du fichier mito
row.names(MLR2)[c(26,48)] <- row.names(mito)[c(26,48)]
#enregistrer le fichier corrigé avec un nouveau nom
write.table(MLR2, file="MLR_correct.txt")

#Problem3
colnames(mito)
#enlève la première colonne [,-1] du tableau mito pour n'avoir que des données numériques
mito <- mito[,-1]
colnames(mito)
classI <- MLR$MLRClassification == "ClassI"
classI <- rownames(MLR)[classI]
classI

classII <- MLR$MLRClassification == "ClassII"
classII <- rownames(MLR)[classII]
classII

classIII <- MLR$MLRClassification == "ClassIII"
classIII <- rownames(MLR)[classIII]
classIII

par(mfrow=c(1,1))
hist(mito[classI,1])

#boucle class I avec nom de graph "temps"
for (i in 1:ncol(mito))
{hist(mito[classI,i], main = i)}

#boucle class II avec nom de graph "temps"
for (i in 1:ncol(mito))
{hist(mito[classII,i], main = i)}

#boucle class III avec nom de graph "temps"
for (i in 1:ncol(mito))
{hist(mito[classIII,i], main = i)}
