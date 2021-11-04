##### nMDS ORDINATION

install.packages("ggplot2")
install.packages("ggrepel")
install.packages("vegan")
install.packages("viridis")
library(ggplot2)
library(ggrepel)
library(vegan)
library(viridis)

setwd("C:/Bell1%/1%SQSQRT")
getwd()

#Function metaMDS performs Nonmetric Multidimensional Scaling (NMDS), and tries to find a stable solution using several random starts
Foram.sp <- as.data.frame(read.csv("Bell_1%SQSQRT.csv", sep=";", row.names=1))
nmds_Foram<-metaMDS(Foram.sp, distance = "euclidean", k=2, autotransform = F)
# k = number of dimensions
nmds_Foram

ordiplot(nmds_Foram)
ordiplot(nmds_Foram, type = "t")

Legend=rep(NA, 34)
ind=sample(1:17)
ind2=sample(18:30)
ind3=sample(31:34)
Legend[ind]="Transect A"
Legend[ind2]="Transect B"
Legend[ind3]="Transect C"
Legend

data.scores=as.data.frame(scores(nmds_Foram))
#Function to access either species or site scores for specified axes in some ordination methods
data.scores$sites=rownames(data.scores)
species.scores=as.data.frame(scores(nmds_Foram,"species"))
species.scores$species=rownames(species.scores)

# Stress value to be plotted on the graph
nmds_Foram["stress"]

# Write .csv files of species and sites nMDS scores
write.csv2(species.scores,"species_score_nMDS.csv")
write.csv2(data.scores,"sites_score_nMDS.csv")

#Let's do the plot
x11(); # optional
nMDS <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=Legend),shape=18,size=3) +
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2),shape=16,colour="grey40",size=2) + #plot species dots
  scale_colour_viridis_d() +
  coord_equal() +
  theme_classic() +
  theme (panel.border = element_rect(fill=NA, linetype="solid", colour="black")) +
  geom_text_repel(data=data.scores,aes(x=NMDS1,y=NMDS2,label=sites),alpha=0.75,size=3) +
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.75,fontface="italic",size=3) +
  ggtitle("NMDS (2k) >1%\nstress=0.12")
nMDS

# Fit Environmental Data -----------
# Creiamo un vettore di variabili ambientali
# envfit è una funzione del pacchetto vegan
EnvBell <- as.data.frame(read.csv("Bell_env.csv", sep=";", row.names=1))

# Se ci sono dati (ambientali) mancanti 
efBell <- envfit(nmds_Foram, EnvBell, permutations = 999, na.rm = TRUE) # rimuovo le righe con dati mancanti
efBell #ef= environmental fit
plot(efBell)

mul <- ordiArrowMul(efBell) #trova il fattore moltiplicativo per scalare i vettori da plottare in un grafico di ordinazione
mul

#efBell.df$NMDS1 = efBell.df$NMDS1 * mul
#efBell.df$NMDS2 = efBell.df$NMDS2 * mul

# Rendiamo l'output di envfit un dataframe in modo da plottare i vettori con ggplot
efBell.df <- as.data.frame(efBell$vectors$arrows*sqrt(efBell$vectors$r) * mul)  #########QUI SPERO D AVER FATTO GIUSTO!!!
efBell.df$EnvVar <- rownames(efBell.df)
efBell.df

#Plot with sites names and environmental variables
ggplot(nmds_Foram.df) +
  theme_classic() +
  theme (panel.border = element_rect(fill = NA, linetype = "solid", colour = "black")) +
  geom_point(data = nmds_Foram.df, aes(x=NMDS1, y=NMDS2, colour=Legend)) +
  geom_text(data = nmds_Foram.df, aes(x=NMDS1, y=NMDS2, label = sites, vjust = "bottom"),
            size = 2.5, nudge_x = 0.02, nudge_y = 0.08) +
  geom_segment(data = efBell.df, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
                      arrow = arrow(length = unit(0.02, "npc")), inherit.aes = FALSE) +
  geom_text(data = efBell.df, aes(x=NMDS1, y=NMDS2, label = EnvVar, 
                                 vjust = "bottom", fontface = "bold"), size = 2.8, nudge_y = 0.14)

#Plot with species and environmental variables
ggplot(nmds_Foram.df) +
  theme_classic() +
  theme (panel.border = element_rect(fill = NA, linetype = "solid", colour = "black")) +
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2),shape=16,colour="red",size=1) +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            vjust = "bottom", size = 3, nudge_y = -0.15)    +
  geom_segment(data = efBell.df, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.02, "npc")), inherit.aes = FALSE) +
  geom_text(data = efBell.df, aes(x=NMDS1, y=NMDS2, label = EnvVar, 
                                  vjust = "bottom", fontface = "bold"), size = 2.8, nudge_y = 0.14)

#Plot SPECIES+SITES+ENVIRONMENTAL VARIABLES
ggplot(nmds_Foram.df) +
  theme_classic() +
  theme (panel.border = element_rect(fill = NA, linetype = "solid", colour = "black")) +
  geom_point(data = nmds_Foram.df, aes(x=NMDS1, y=NMDS2, colour=Legend)) +
  geom_text(data = nmds_Foram.df, aes(x=NMDS1, y=NMDS2, label = sites, vjust = "bottom"),
            size = 2.5, nudge_x = 0.02, nudge_y = 0.08) +
  geom_segment(data = efBell.df, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.02, "npc")), inherit.aes = FALSE) +
  geom_text(data = efBell.df, aes(x=NMDS1, y=NMDS2, label = EnvVar, 
                                  vjust = "bottom", fontface = "bold"), size = 2.8, nudge_y = 0.14) +
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2),shape=16,colour="grey40",size=1) +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            vjust = "bottom", size = 3, nudge_y = -0.15)

#Plot the environmental variables related to the clusters of MRT
# Aggiungiamo una colonna con il numero di cluster assegnato a ogni campione
Clust.Forams <- read.csv("clusters_mod.csv", sep = ";", row.names = 1)
nmds_Foram.df$cluster <- Clust.Forams[,1]

# Visualizziamo tutte le palette e scegliamone una; poi otteniamo ogni colore 
# in formato esadecimale per poterlo impostare manualmente
display.brewer.all()
display.brewer.pal(n=9, name="Set1")
brewer.pal(n=9, name="Set1")

ggplot(nmds_Foram.df) +
  theme_classic() +
  theme (panel.border = element_rect(fill = NA, linetype = "solid", colour = "black")) +
  geom_point(data = nmds_Foram.df, aes(x=NMDS1, y=NMDS2, colour=factor(cluster))) +
  geom_text(data = nmds_Foram.df, aes(x=NMDS1, y=NMDS2, label = sites, vjust = "bottom"),
            size = 3, nudge_y = -0.15) +
  scale_color_manual(values=c("1"="#E41A1C", "2"="#377EB8", "3"="#4DAF4A", 
                              "4"="#984EA3")) +
  geom_segment(data = efBell.df, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.02, "npc")), inherit.aes = FALSE) +
  geom_text(data = efBell.df, aes(x=NMDS1, y=NMDS2, label = EnvVar, 
                                 vjust = "bottom", fontface = "bold"), size = 3, nudge_x =0.19) +
  labs(colour = "Cluster")
