
####################################################################
# Chargement et nettoyage des données 
# Préparation du data frame contenant en ligne les ind, en col les rangs
###################################################################
library(tidyr)
library(tidyverse)

atomic <- read.table("Atomic Habits.csv", header = T, stringsAsFactors = T, sep = ",")
names(atomic)

# changer le nom des colonnes => passer en n° de ranking de 1 à 14
names(atomic)[6:19] <- as.character(1:14) ; names(atomic)

# supprimer les donn&es manquantes 
atomic <- drop_na(atomic)

new <- atomic[,6:19]

# Extraire la liste complète des assertions
assertions <- sort(unique(unlist(new))) # vecteur d'assertions

# Créer une matrice : chaque valeur = position (ordre) de l'assertion
m <- t(apply(new, 1, function(x) match(assertions, x)))

resultat <- data.frame(m)

# Ajouter les noms de colonnes (assertions)
colnames(resultat) <- (assertions) ; resultat


# Export d'un fichier csv contenant le df analysable
write.csv(resultat, "resultat.csv")



####################################################################
# Comparaison des df des autres groupes
###################################################################

group1 <- read.table("lena.csv", stringsAsFactors = T, header = T, sep= ",")
group2 <- read.table("audrey.csv", stringsAsFactors = T, header = T, sep= ",")

# Groupe 1 : ont conservé l'id 5 (mais pas de réponses pour l'illustratif), et l'id réponse
dim(group1) # 47 15
names(group1)

# Groupe 2 : Tableau de même dimension que le nôtre 
dim(group2)

# Comparaison entre les valeurs des df des autres groupes

#### groupe 1
names(resultat) == names(group1)[2:15] # comparaison des noms des colonnes 
sum(resultat != group1[2:47,2:15])  # comparaison des valeurs 

### groupe 2
sum(group1[2:47,2:15] != group2)  # comparaison des valeurs 

# Ca marche pas trop bon :()

####################################################################
# Utilisation du package smacof pour créer le plan factoriel
# des profils idéaux 
###################################################################

# Package Smackof : faire tourner le package
install.packages("smacof")
require(smacof)
library(ggplot2)
library(plotly)
library(ggrepel)

data <- group1[,-1]

# changement des noms des colonnes 
names(data)
assertions <- c(
  "circuits_courts",
  "mobilite_douce",
  "zero_dechet",
  "sans_ia",
  "sans_avion",
  "produits_saison",
  "outils_manuel",
  "chauffage_bas",
  "covoiturage_train",
  "antideforestation",
  "location_pret",
  "vegetalien",
  "seconde_main",
  "veille_off"
)

names(data) <- assertions

unfolded <- unfolding(data)
plot(unfolded)


# Dictionnaire de unfolded
names(unfolded)

# [1] "obsdiss"        "confdist"       "dhat"           "iord"           "conf.row"       "conf.col"       "stress"        
# [8] "pstress"        "spp.row"        "spp.col"        "congvec"        "weightmat"      "ndim"           "model"         
# [15] "niter"          "nind"           "nobj"           "trans"          "conditionality" "call"   

unfolded$conf.row # coordonnées des individus => représentés par leurs idéaux 
unfolded$conf.col # coordonnées des assertions => coordonnées des stimulis   



conf_assertions <- as.data.frame(unfolded$conf.col)
conf_ind <- as.data.frame(unfolded$conf.row)
p <- ggplot(conf_ind, aes(x = D1, y = D2)) 
p + geom_point(size = 1, colour = "black") + coord_fixed() + 
  geom_point(aes(x = D1, y = D2), conf_items, colour = "cadetblue") + 
  geom_text_repel(aes(x = D1, y = D2, label = rownames(conf_items)), 
                  conf_assertions, colour = "cadetblue", vjust = -0.8) 
ggtitle("Unfolding Configuration")



# Ajouter de la data illustrative 
illus <- atomic[,c(1,20:21)]
names(illus)[2:3] <- c("Responsabilite_env", "Sentiment_env")  

illus[,1:3] <- lapply(illus, as.factor)

levels(illus$Responsabilite_env) <- c("pas_du_tout_responsable", "peu_responsable", "moy_responsable", "responsable", "tres_responsable")

# nrow(illus)
# nrow(group1)

new_data <- cbind(as.data.frame(unfolded$conf.row[-1,]), illus[,-1])


# calcul des coordonnées des modalités de respo et de sentiment

points_moyens_responsabilite <- aggregate(
  x = new_data[, c("D1", "D2")], # Les variables sur lesquelles calculer la moyenne
  by = list(Responsabilite_env = new_data$Responsabilite_env), # La variable de regroupement
  FUN = mean # La fonction à appliquer (la moyenne)
)

points_moyens_sentiment <- aggregate(
  x = new_data[, c("D1", "D2")], # Les variables sur lesquelles calculer la moyenne
  by = list(Sentiment_env = new_data$Sentiment_env), # La variable de regroupement
  FUN = mean # La fonction à appliquer (la moyenne)
)


# Graphique pimpé avec data illustrative
p2 <- ggplot(conf_ind, aes(x = D1, y = D2)) 

p2 <- p2 +
  
  # Individus
  geom_point(size = 1, colour = "black") + coord_fixed() + 
  
  # ASSERTIONS
  geom_point(aes(x = D1, y = D2), conf_assertions, colour = "cadetblue") + 
  geom_text_repel(aes(x = D1, y = D2, label = rownames(conf_assertions)), 
                  conf_assertions, colour = "cadetblue", vjust = -0.8, size = 3) +
  
  # Responsabilité environnementale 
  geom_point(aes(x=D1, y=D2), points_moyens_responsabilite, colour = "red", size = 1) +
  geom_text_repel(aes(x=D1, y=D2), label = levels(illus$Responsabilite_env), points_moyens_responsabilite, 
                  colour = "red", vjust = -0.8 , size = 3) + 
  
  # SENTIMENT environnemental
  geom_point(aes(x=D1, y=D2), points_moyens_sentiment, colour = "green3", size = 1) +
  geom_text_repel(aes(x=D1, y=D2), label = levels(illus$Sentiment_env)[-1], points_moyens_sentiment, 
                  colour = "green3", vjust = -0.8 , size = 3) +
  
  
  labs(title = "Unfolding Configuration") +
  
  theme(title = element_text(color = "darkgreen", hjust = -0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "grey95", color = NA),
        
        
  )

ggplotly(p2)
####################################################
# version plotly 
####################################################

assertions_long <- names(resultat)

fig <- plot_ly() %>%
  
  # Individus
  add_trace(data = conf_ind,
            x = ~D1, y = ~D2,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'black', size = 5),
            name = 'Individus') %>%
  
  # Assertions
  add_trace(data = conf_assertions,
            x = ~D1, y = ~D2,
            type = 'scatter', mode = 'markers+text',
            marker = list(color = 'cadetblue', size = 5),
            text = ~rownames(conf_assertions),
            textfont = list(color = 'cadetblue'),
            textposition = 'top center',
            hovertext = ~assertions_long,  # texte au survol
            hoverinfo = 'text',
            name = 'Assertions') %>%
  
  # Responsabilité environnementale
  add_trace(data = points_moyens_responsabilite,
            x = ~D1, y = ~D2,
            type = 'scatter', mode = 'markers+text',
            marker = list(color = 'red', size = 5),
            text = ~levels(illus$Responsabilite_env),
            textfont = list(color = 'red'),
            textposition = 'top center',
            name = 'Responsabilité') %>%
  
  # Sentiment environnemental
  add_trace(data = points_moyens_sentiment,
            x = ~D1, y = ~D2,
            type = 'scatter', mode = 'markers+text',
            marker = list(color = 'green', size = 5),
            text = ~levels(illus$Sentiment_env)[-1],
            textfont = list(color = 'green'),
            textposition = 'top center',
            name = 'Sentiment') %>%
  

  layout(title = list(text = "Unfolding Configuration", x = 0.5),
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         plot_bgcolor = 'rgb(242, 242, 242)',
         showlegend = TRUE)

fig

####################################################
# Métriques sur les données de ranking
mean <- sapply(data, FUN = mean)

# TOP 3 des assertions
which.min(mean) # chauffage_bas 

names(sort(mean)[1:3]) # "chauffage_bas"   "produits_saison" "mobilite_douce" 

# BOTTOM 3 des assertions
names(sort(mean, decreasing = TRUE))[1:3] # [1] "vegetalien" "antideforestation" "sans_avion"       


data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Ranking")

ggplot(data_long, aes(x = reorder(Variable, Ranking, mean), y = Ranking)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Boxplot des rankings sur 14 atomic habits",
       x = "Variable",
       y = "Ranking") +
  theme(axis.text.x= element_text(angle =45 , hjust=1), 
        plot.title = element_text(hjust = 0.5))



####################################################################
# Faire tourner l'algorithme à la main 
###################################################################

# => durant une prochaine séance 


