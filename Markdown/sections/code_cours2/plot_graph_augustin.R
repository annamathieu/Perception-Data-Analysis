library(tidyverse)
library(ggrepel)


#### Création du jeu de donnée ####
stimuli <- read.table("Markdown/dataset/Atomic Habits.csv", sep = ",", stringsAsFactors = TRUE, header = TRUE)

stimuliNArm <- stimuli %>% filter(Date.de.soumission != "")


stim <- stimuliNArm[6:19]

colnames(stim) <- c(1:14)

f <- function(x){
  df <- data.frame(matrix(1:14, nrow = 1))
  colnames(df) <- as.character(unlist(x))
  return(df)
}

df <- apply(stim, 1, FUN = f, simplify = TRUE )
df <- as.data.frame(df)
df <- as.data.frame(do.call(rbind, lst))

#### Affichage graphique ####

# le df1 est le dataframe d'Audrey
df1 <- read.table("ranking.csv", header = TRUE, sep = ",")

dfLena <- read.table("habits_large.csv")


library(smacof)
un_df1 <- unfolding(df1)
plot(un_df1)
names(un_df1)

conf.row <- as.data.frame(un_df1$conf.row)
conf.col <- as.data.frmae(un_df1$conf.col)

row.names(conf.col) <- c("circuit_court",
                         "sans_emballage",
                         "seconde_main",
                         "regime_vegetarien",
                         "appareils_branches",
                         "ustensiles_manuels",
                         "temperature",
                         "saisonnalite",
                         "emprunt",
                         "IA",
                         "avion",
                         "covoit",
                         "deforestation",
                         "trajets_courts")

  
  
conf.row %>% ggplot(aes(x = D1, y = D2)) +
  geom_point(col = "blue") +
  geom_text_repel(aes(label = rownames(conf.row)), col = "blue") +
  geom_point(data = conf.col, aes(x = D1, y = D2), col = "red") +
  geom_text_repel(data = conf.col, aes(label = rownames(conf.col)), col = "red")
  

conf.row$responsable <- as.factor(stimuliNArm$À.quel.point.vous.sentez.vous.responsable.de.réduire.votre.impact.environnemental....1...Pas.du.tout.responsable..5...Très.responsable.)

responsabilite <- conf.row %>% group_by(responsable) %>% 
  summarise(D1 = mean(D1), D2 = mean(D2))
responsabilite <- as.data.frame(responsabilite)
row.names(responsabilite) <- c("pas_du_tout_responsable",
                               "peu_responsable",
                               "moyennement_responsable",
                               "responsable",
                               "tres_responsable")


conf.row %>% ggplot(aes(x = D1, y = D2)) +
  geom_point(col = "blue") +
  geom_text_repel(aes(label = rownames(conf.row)), col = "blue") +
  geom_point(data = conf.col, aes(x = D1, y = D2), col = "red") +
  geom_text_repel(data = conf.col, aes(label = rownames(conf.col)), col = "red") +
  geom_point(data = responsabilite, aes(x = D1, y = D2), col = "darkgreen") +
  geom_text_repel(data = responsabilite, aes(label = rownames(responsabilite)), col = "darkgreen")


conf.row$crise <- as.factor(stimuliNArm$Quand.vous.entendez.parler.de.crise.climatique..ressentez.vous.plutôt.......choix.unique.)

crise_eco <- conf.row %>% group_by(crise) %>% 
  summarise(D1 = mean(D1), D2 = mean(D2))
crise_eco <- as.data.frame(crise_eco)
row.names(crise_eco) <- c("Autre",
                               "De l'espoir",
                               "De l'indifference",
                               "De l'urgence a agir",
                               "De la resignation")

conf.row %>% ggplot(aes(x = D1, y = D2)) +
  geom_point(col = "blue") +
  geom_text_repel(aes(label = rownames(conf.row)), col = "blue") +
  geom_point(data = conf.col, aes(x = D1, y = D2), col = "red") +
  geom_text_repel(data = conf.col, aes(label = rownames(conf.col)), col = "red") +
  geom_point(data = responsabilite, aes(x = D1, y = D2), col = "darkgreen") +
  geom_text_repel(data = responsabilite, aes(label = rownames(responsabilite)), col = "darkgreen") +
  geom_point(data = crise_eco, aes(x = D1, y = D2), col = "darkorange") +
  geom_text_repel(data = crise_eco, aes(label = rownames(crise_eco)), col = "darkorange")


