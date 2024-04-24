########################
########################
#Codice progetto per il corso di Analisi di Mercato Quantitative
#Analisi alle risposte di un questionario, segmentazione e
#profilazione clienti, proposte di prodotti ad hoc
#
#Nicola Pesaresi
#######################
#######################

#preprocessing
dat<- read.csv("Dati_Fidelity.csv", header=T, sep=";")
#View(dat)
str(dat)
dat<- data.frame(lapply(dat, as.factor))
dat$Data.di.nascita<- as.Date(dat$Data.di.nascita, format="%d/%m/%Y")
dat[,c(10:16,27:35)]<-sapply(dat[,c(10:16,27:35)], as.numeric)
str(dat)

#ANALISI ESPLORATIVA
library(ggplot2)
library(magrittr) #pipe operator
nrow(dat)
table(dat$Genere)

#trasformo data di nascita in età
dat$Data.di.nascita<- floor(difftime(Sys.Date(), 
                                     dat$Data.di.nascita, units="days")/365)
colnames(dat)[37]<- "Eta"
dat$Eta<- as.double(dat$Eta)

mean(dat$Eta)
mean(dat[which(dat$Genere=="M"),"Eta"])
mean(dat[which(dat$Genere=="F"),"Eta"])
hist(dat$Eta, breaks=max(dat$Eta)-min(dat$Eta), col="white",
     xlim=c(20,70), ylim=c(0,20),
     main="", xlab="Distribuzione età", ylab= "Frequenza")
abline(v=mean(dat$Eta), col="red", cex=10)
abline(v=mean(dat[which(dat$Genere=="M"),"Eta"]), col="blue", cex=10)
abline(v=mean(dat[which(dat$Genere=="F"),"Eta"]), col="pink", cex=10)

#outliers
boxplot<-boxplot(dat$Eta, xlab="Box-plot età rispondenti")
length(boxplot$out)

#rimuovo valore più estremo
dat<- dat[-which.max(dat$Eta),]

#binning variabile età
Eta.bin<- ifelse(dat$Eta <= 25, "under 25",
                 ifelse(dat$Eta <=27, "under 27",
                        ifelse(dat$Eta <=29, "under 29","over 30")))

dat<- cbind(dat, Eta.bin)
dat$Eta.bin<- factor(dat$Eta.bin, levels=c("under 25","under 27", "under 29","over 30"))


#--------------------
#Abitudini consumatori (domande 2-9)
abit<- dat[c(2:9,36,37,38)]

#problemi con opzioni di risposta
temp<-which(abit$In.genere..i.prodotti.di.profumeria.Lei.=="online")
abit$In.genere..i.prodotti.di.profumeria.Lei.[temp]<-"li acquisto online"
temp<-which(abit$In.genere..i.prodotti.di.profumeria.Lei.=="li acquista dove capita, ipermercati, ingrosso, etc, online ")
abit$In.genere..i.prodotti.di.profumeria.Lei.[temp]<-"li acquista dove capita, ipermercati, ingrosso, etc"
temp<-which(abit$In.genere..i.prodotti.di.profumeria.Lei.=="li acquista in alcune profumerie fisse, li acquista dove capita, ipermercati, ingrosso, etc, Farmacia")
abit$In.genere..i.prodotti.di.profumeria.Lei.[temp]<-"li acquista dove capita, ipermercati, ingrosso, etc"
abit$In.genere..i.prodotti.di.profumeria.Lei.<-factor(abit$In.genere..i.prodotti.di.profumeria.Lei.)

temp<-which(abit$Quanto.spende.in.media.ogni.volta.=="10 euro ")
abit$Quanto.spende.in.media.ogni.volta.[temp]<-"Meno di 20 euro"
temp<-which(abit$Quanto.spende.in.media.ogni.volta.=="20 euro, 40 euro")
abit$Quanto.spende.in.media.ogni.volta.[temp]<-"30 euro"
temp<-which(abit$Quanto.spende.in.media.ogni.volta.=="20 euro, meno di 20 euro")
abit$Quanto.spende.in.media.ogni.volta.[temp]<-"20 euro"
temp<-which(abit$Quanto.spende.in.media.ogni.volta.=="40 euro, Oltre i 50 euro")
abit$Quanto.spende.in.media.ogni.volta.[temp]<-"Oltre i 50 euro"
abit$Quanto.spende.in.media.ogni.volta.<-factor(abit$Quanto.spende.in.media.ogni.volta.)

#riordino levels
abit$Quante.volte.va.in.profumeria.<- factor(abit$Quante.volte.va.in.profumeria.,
                                             levels=c("una volta al mese", "una volta ogni tre mesi", "ancora più raramente"))
abit$Quanto.spende.in.media.ogni.volta.<- factor(abit$Quanto.spende.in.media.ogni.volta.,
                                                 levels=c("Meno di 20 euro","20 euro","30 euro","40 euro","Oltre i 50 euro"))

#GGally::ggpairs(abit)

#par(mar=c(20,3,1,1))

#grafici abit~Genere
barplot(table(abit$Genere,abit$In.genere..i.prodotti.di.profumeria.Lei.), 
        beside=T,col=c("pink","lightblue"))
barplot(table(abit$Genere,abit$Quante.volte.va.in.profumeria.), 
        beside=T,col=c("pink","lightblue"), main="Quante volte va in profumeria?")
barplot(table(abit$Genere,abit$Quanto.spende.in.media.ogni.volta.), 
        beside=T,col=c("pink","lightblue"), main="Quanto spende in media?")
barplot(table(abit$Genere,abit$In.genere..i.prodotti.di.profumeria.Lei.), 
        beside=T,col=c("pink","lightblue"))
barplot(table(abit$Genere,abit$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.il.trucco.), 
        beside=T,col=c("pink","lightblue"))
barplot(table(abit$Genere,abit$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.la.cura.del.corpo.e.del.viso.), 
        beside=T,col=c("pink","lightblue"))
barplot(table(abit$Genere,abit$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.profumarsi.), 
        beside=T,col=c("pink","lightblue"))
barplot(table(abit$Genere,abit$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.l.abbronzatura.), 
        beside=T,col=c("pink","lightblue"))
barplot(table(abit$Genere,abit$In.genere..in.profumeria.lei.compra), 
        beside=T,col=c("pink","lightblue"), main="In genere in profumeria lei compra:")

#grafici abit~Eta.bin
barplot(table(abit$Eta.bin,abit$In.genere..i.prodotti.di.profumeria.Lei.), beside=T, names=c("dove capita", "alcune profumerie", "una profumeria fissa", "online"),
        main="In genere i prodotti di profumeria lei:", col=c("lightgreen","green", "forestgreen", "darkgreen"))
legend("topright", legend=levels(abit$Eta.bin), fill=c("lightgreen","green", "forestgreen", "darkgreen")) 
barplot(table(abit$Eta.bin,abit$Quante.volte.va.in.profumeria.), beside=T)
barplot(table(abit$Eta.bin,abit$Quanto.spende.in.media.ogni.volta.), beside=T)
barplot(table(abit$Eta.bin,abit$In.genere..in.profumeria.lei.compra), beside=T)


#Esigenze consumatori (domande 10-16)
need<- dat[10:16]
colnames(need)<- c("cortesia","interpretazione esigenze",
                   "segnalazione promozioni","consigli di utilizzo","prezzi",
                   "sconto", "tester regalati")
#GGally::ggpairs(need)

par(mfrow=c(1,7))
boxplot(need$cortesia, main="cortesia")
boxplot(need$`interpretazione esigenze`, main="interpretazine esigenze")
boxplot(need$`segnalazione promozioni`, main="segnalazione promozioni")
boxplot(need$`consigli di utilizzo`, main="consigli di utilizzo")
boxplot(need$prezzi, main="prezzi")
boxplot(need$sconto, main="sconto")
boxplot(need$`tester regalati`, main="tester regalati")
par(mfrow=c(1,1))

colMeans(need)
apply(need, 2, var)


#Profilazione stile di vita (Domande 17-26)
lifestyle<- dat[17:26]
colnames(lifestyle)<-gsub("Corrispondono.al.suo.stile.di.vita.le.seguenti.affermazioni...","",colnames(lifestyle))

#sostituisco Sì e No con 1 e 0
lifestyle[] <- lapply(lifestyle, as.character)
lifestyle[lifestyle=="Sì"]<-1
lifestyle[lifestyle=="No"]<-0
lifestyle[] <- lapply(lifestyle, as.numeric)

#GGally::ggpairs(lifestyle)

apply(lifestyle,2,table)

withins<-rep(0,5)
for (i in 1:5) {
  #temp<-kmeans(lifestyle, centers=i, nstart=10)
  temp<- klaR::kmodes(lifestyle, i, iter.max=100)
  withins[i]<- sum(temp$withindiff)
}
plot(withins, type="l")

pca<-princomp(cor(lifestyle))
summary(pca)

ca<-FactoMineR::CA(lifestyle)
summary(ca)



#Fidelity Card (domande 27-35)
fidelity<- dat[27:35]
colnames(fidelity)<- paste("Profilo",c(1:9))
#GGally::ggpairs(fidelity)

par(mfrow=c(1,9))
boxplot(fidelity$`Profilo 1`, main="Profilo 1")
boxplot(fidelity$`Profilo 2`, main="Profilo 2")
boxplot(fidelity$`Profilo 3`, main="Profilo 3")
boxplot(fidelity$`Profilo 4`, main="Profilo 4")
boxplot(fidelity$`Profilo 5`, main="Profilo 5")
boxplot(fidelity$`Profilo 6`, main="Profilo 6")
boxplot(fidelity$`Profilo 7`, main="Profilo 7")
boxplot(fidelity$`Profilo 8`, main="Profilo 8")
boxplot(fidelity$`Profilo 9`, main="Profilo 9")
par(mfrow=c(1,1))

colMeans(fidelity)
apply(fidelity, 2, var)

#modello lm su punteggio profili
profili<- matrix(c("Vacanza Maldive","Feste a tema","","Palestra"	,"Fisso",
                     "Vacanza Maldive",	""	,"Rivista di moda",	"Centro estetico",	"Variabile",
                     "Beauty farm",	"Convention",	""	,"Palestra",	"Variabile",
                     "Beauty farm",	""	,"Rivista di moda",	"Hair-stylist",	"Fisso",
                     "Beauty farm",	"Feste a tema",	"",	"Centro estetico",	"Fisso",
                     "Vacanza avventurosa",	""	,"Rivista di moda",	"Palestra",	"Fisso",
                     "Vacanza avventurosa",	"Convention",	"",	"Centro estetico"	,"Fisso",
                     "Vacanza Maldive",	"Convention",	""	,"Hair-stylist",	"Fisso",
                     "Vacanza avventurosa",	"Feste a tema",	""	,"Hair-stylist"	,"Variabile"),
                      dimnames=list(c("Profilo 1","Profilo 2","Profilo 3","Profilo 4","Profilo 5",
                                  "Profilo 6","Profilo 7","Profilo 8","Profilo 9"),
                                  c("Premio concorso","Eventi","Abbonamento",	"Card","Sconto Card")),
                     nrow=9, byrow=T)
profili.punt<- as.data.frame(cbind(profili, punteggio=(colMeans(fidelity))))

punt.lm<- lm(punteggio ~ ., profili.punt)
summary(punt.lm)
punt.lm<- lm(punteggio ~ `Premio concorso` + Eventi + Card, profili.punt)
summary(punt.lm)


#Unisco nuove colonne
dat2<-data.frame(abit, need, lifestyle, fidelity)

#modello lm su punteggio profili per categoria
library(tidyr)

punt.long <- dat2 %>% 
  pivot_longer(
    cols = "Profilo.1":"Profilo.9", 
    names_to = "Profilo",
    values_to = "Punteggio"
  )

profili<- as.data.frame(profili, stringsAsFactors = T, row.names = NULL)
punt.long.descr<- profili[rep(1:9,95),]
punt.long<- cbind(punt.long,punt.long.descr)

punt.lm<- lm(Punteggio ~ ., data=punt.long[,- 29])
summary(punt.lm)

punt.lm<- lm(Punteggio ~ Genere + Eta.bin + `Premio concorso` + Eventi + Card + 
               In.genere..i.prodotti.di.profumeria.Lei. +
               tester.regalati +
               Sono.una.persona.dinamica.con.tanti.interessi. 
               
               , data=punt.long)
summary(punt.lm)

#ggcorrplot::ggcorrplot(cor(punt.long))

#Acquisti
acquisti<- cbind(table(dat2$Genere, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.il.trucco.),
                 table(dat2$Genere, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.la.cura.del.corpo.e.del.viso.),
                 table(dat2$Genere, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.profumarsi.),
                 table(dat2$Genere, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.l.abbronzatura.))
acquisti<- acquisti[,c(2,4,6,8)] #solo colonne "sì"
colnames(acquisti)<- c("Trucco","Cura del corpo e viso", "Profumi","Abbronzatura")


acquistiEta<- cbind(table(dat2$Eta.bin, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.il.trucco.),
                    table(dat2$Eta.bin, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.la.cura.del.corpo.e.del.viso.),
                    table(dat2$Eta.bin, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.profumarsi.),
                    table(dat2$Eta.bin, dat2$Ha.acquistato.di.recente.in.una.qualunque.profumeria.prodotti...per.l.abbronzatura.))
acquistiEta<- acquistiEta[,c(2,4,6,8)] #solo colonne "sì"
colnames(acquistiEta)<- c("Trucco","Cura del corpo e viso", "Profumi","Abbronzatura")
barplot(acquistiEta, 
        beside=T,col=c("lightgreen","green", "forestgreen", "darkgreen"),
        main="Prodotti recentemente acquistati per fascia di età:")
legend("topright", legend=levels(abit$Eta.bin), fill=c("lightgreen","green", "forestgreen", "darkgreen")) 

barplot(acquisti, 
        beside=T,col=c("pink","lightblue"), main="Prodotti recentemente acquistati per genere:")

colSums(acquisti)/sum(acquisti)*100


#----------------
#Clustering
library(cluster)
library(FactoMineR)
library(factoextra)

selez<- c(2,3,4,8,9,10,15,16,17,18,19,21,23,24)
dat.selez<- dat2[,selez]
dat.selez<-fastDummies::dummy_cols(dat.selez, remove_selected_columns = T)
pca<- PCA(dat.selez, ncp=10)
summary(pca)
obj<- pca$var$coord

#segmentazione
selez<- c(2,3,4,8,9,10,15,16,17,18,19,21,23,24)
dat.selez<- dat2[,selez]
dat.selez<-fastDummies::dummy_cols(dat.selez, remove_selected_columns = T)
#pca<- prcomp(dat.selez)
#summary(pca)
#obj<- pca$x[,1:6]

dist<-dist(dat.selez, method="manhattan")
h<-hclust(dist, method="ward.D2")
plot(h)

#cut<- cutree(h, 4)
#table(cut)

silhouette_score <- function(df, i){
  hc<- cutree(h,i)
  ss <- silhouette(hc, dist)
  mean(ss[, 3])
}
k <- 2:8
avg_sil <- sapply(k, silhouette_score, df=dat2)
sil.df<-data.frame(k, avg_sil)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE) #k=2
which.max(avg_sil) +1 

plot(h)
rect.hclust(h,3)
rect.hclust(h,4, border="blue")
cut<-cutree(h,3)
table(cut)

colnames(dat.selez)
table(dat2[cut==1,"Genere"])
table(dat2[cut==2,"Genere"])
table(dat2[cut==3,"Genere"])
#table(dat2[cut==4,"Genere"])
mean(dat2[cut==1,"Eta"])
mean(dat2[cut==2,"Eta"])
mean(dat2[cut==3,"Eta"])
#mean(dat2[cut==4,"Eta"])

data.frame(C1=colMeans(dat.selez[cut==1,]),
           C2=colMeans(dat.selez[cut==2,]),
           C3=colMeans(dat.selez[cut==3,]) ) %>%
  round(2) -> cluster.stats

cluster.stats
#write.table(cluster.stats, sep=";", file("clusters.table.csv"))

#calcolo t-stat per valutare quali variabili hanno
#contribuito maggiormente alla partizione
s2<- sapply(dat.selez, var)
s2k1<-sapply(dat.selez[cut==1,], var)
s2k2<-sapply(dat.selez[cut==2,], var)
s2k3<-sapply(dat.selez[cut==3,], var)
n<-nrow(dat.selez)
n1<-nrow(dat.selez[cut==1,])
n2<-nrow(dat.selez[cut==2,])
n3<-nrow(dat.selez[cut==3,])

s2p1<- ((n1 - 1)*s2k1 + (n-1)*s2)/(n1 + n -2)
s2p2<- ((n2 - 1)*s2k2 + (n-1)*s2)/(n2 + n -2)
s2p3<- ((n3 - 1)*s2k3 + (n-1)*s2)/(n3 + n -2)

x.bar<- colMeans(dat.selez)
x.bar1<- colMeans(dat.selez[cut==1,])
x.bar2<- colMeans(dat.selez[cut==2,])
x.bar3<- colMeans(dat.selez[cut==3,])


t1<- (x.bar1 - x.bar)/sqrt(s2p1*(1/n1 + 1/n))
t2<- (x.bar2 - x.bar)/sqrt(s2p2*(1/n2 + 1/n))
t3<- (x.bar3 - x.bar)/sqrt(s2p3*(1/n3 + 1/n))

t.stat<- round(data.frame(t1, t2, t3, row.names = colnames(dat.selez)),2)
t.stat

#write.table(t.stat, sep=";", file("tstat.table.csv"))

#Relazioni cluster - fidelity card

colMeans(fidelity)
apply(fidelity, 2, var)

#modello lm su punteggio profili
str(profili)
profili.punt.clust<- as.data.frame(cbind(profili, 
                                         punteggio.C1=round((colMeans(fidelity[cut==1,])),2),
                                         punteggio.C2=round((colMeans(fidelity[cut==2,])),2),
                                         punteggio.C3=round((colMeans(fidelity[cut==3,])),2)))
profili.punt.clust

punt.clust<-as.data.frame(cbind(
                                                      punteggio.C1=round((colMeans(fidelity[cut==1,])),2),
                                                      punteggio.C2=round((colMeans(fidelity[cut==2,])),2),
                                                      punteggio.C3=round((colMeans(fidelity[cut==3,])),2)))
punt.clust

#write.table(profili.punt.clust, sep=";", file("punt.clust.csv"))

summary(lm(punteggio.C1 ~ ., profili.punt.clust[,-c(7,8)]))
summary(lm(punteggio.C2 ~ ., profili.punt.clust[,-c(6,8)]))
summary(lm(punteggio.C3 ~ ., profili.punt.clust[,-c(6,7)]))

