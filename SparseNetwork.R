##################################################################
#####    RÉSEAUX PARCIMONIEUX ET DÉTECTION DE COMMUNAUTÉS    #####
#####              Projet de Compressed Sensing              #####
#####            Gautier APPERT & Guillaume SALHA            #####
#####               ENSAE ParisTech - Avril 2016              #####
##################################################################





##### ÉTAPE 1 -  Installation de packages utiles #####


install.packages('igraph')
library(igraph)


install.packages('Rcsdp')
library(Rcsdp)





##### ÉTAPE 2 - Stochastic Block Models : premières visualisations #####


# Génération d'un graphe aléatoire à partir du Stochastic Block Model 
# G(n,p,q) avec n = 200, p = 0.3 et q = 0.005

Graph <- sample_sbm(n=200, pref.matrix=matrix(c(0.3,0.005,0.005,0.3),2,2), block.sizes=c(100,100))


# Récupération de la matrice d'adjacence du graphe

S <- get.adjacency(Graph)


# Elle est au format sparse. Pour la récupérer dans un format classique, nous pouvons
# utiliser la commande as.matrix

A <- as.matrix(get.adjacency(Graph))

View(A)


# Visualisation du graphe, avec les deux communautés en rouge et en bleu

plot(Graph, layout = layout.fruchterman.reingold,
     vertex.size = 5,
     vertex.color = c(rep("red",100),rep("blue",100)),
     vertex.label = NA,
     vertex.frame.color = "white",
     vertex.label.color = "black",
     vertex.label.family = "sans",
     edge.width = 0.1,  
     edge.color = "black")





##### ÉTAPE 3 - Automatisation de la représentation graphique #####


# Nous créons une fonction plotGraph afin de représenter automatiquement les graphes créés
# --> "Graph" correspond au graphe créé via le SBM
# --> "Labels" correspond aux couleurs associés aux Labels (vecteur 1/-1 => vecteur 'red'/'blue')

plotGraph <- function(Graph,Labels){

  plot(Graph, layout = layout.fruchterman.reingold,
       vertex.size = 5,
       vertex.color = Labels,
       vertex.label = NA,
       vertex.frame.color = "white",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       edge.width = 0.3,  
       edge.color = "black")

}


# Quelques exemples :

# Le graphe de l'étape 2
Labels <- c(rep("red",100),rep("blue",100))
plotGraph(Graph,Labels)

# Un graphe un peu moins "dense"
Graph2 <- sample_sbm(n=200, pref.matrix=matrix(c(0.01,0.005,0.005,0.01),2,2), block.sizes=c(100,100))
plotGraph(Graph2,Labels)

