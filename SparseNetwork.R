##################################################################
#####    RÉSEAUX PARCIMONIEUX ET DÉTECTION DE COMMUNAUTÉS    #####
#####              Projet de Compressed Sensing              #####
#####            Gautier APPERT & Guillaume SALHA            #####
#####               ENSAE ParisTech - Avril 2016             #####
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
Graph2 <- sample_sbm(n=200, pref.matrix=matrix(c(0.03,0.005,0.005,0.03),2,2), block.sizes=c(100,100))
plotGraph(Graph2,Labels)





##### ÉTAPE 4 - Clustering Spectral #####


# Algorithme de Clustering Spectral dans le cadre de la recherche de deux communautés
# (k-means sur le second vecteur propre) et sur le Laplacien non-normalisé du graphe
# --> A désigne la matrice (dense) d'adjacence du graphe

spectralClustering <- function(A){
  
  # Matrice des degrés
  D <- diag(rowSums(A))
  
  # Laplacien
  L <- D - A
  
  # Décomposition spectrale de L
  # et calcul du second vecteur propre
  eigenDecomp <- eigen(L)
  vect <- eigenDecomp$vectors[,nrow(L)-1]
  
  # Clustering
  community <- sign(vect)
  
  # Retour des résultats
  return(community)
}


# Application au graphe précédent

A <- as.matrix(get.adjacency(Graph))
testGraph <- spectralClustering(A)

# Code couleur en fonction des prédictions de communautés
colorsSC1 <- ifelse(testGraph==1,'red','blue')

# On vérifie graphiquement que le clustering est parfait
plotGraph(Graph,colorsSC1)





##### ÉTAPE 5 - Méthode de Guédon and Vershynin (2015) #####


# Dans leur article, Guédon and Vershynin (2015) présentent une méthode flexible pour prouver 
# la précision de certains problèmes d'optimisation SDP pour la résolution de problèmes 
# stochastiques variés. La méthode, basée notamment sur l'inégalité de Grothendieck et sur une
# déviation en cut-norm, est générale. Nous l'appliquons ici au problème de détection 
# de communautés, notamment au sein de réseaux parcimonieux.


# Méthode de Guédon and Vershynin
# --> A désigne la matrice (dense) d'adjacence du graphe

comDetect <- function(A) {
  
  # Scalaire lambda du problème SDP
  lambda <- mean(colSums(A))
  
  # Résolution du problème SDP, grâce au package Rcsdp et surtout la fonction csdp()
  # (Formulation du problème primal via la trace, cf la documentation du package)
  C <- list(list(diag(nrow(A)))) ; X <- list(A-lambda)
  n <- nrow(A)
  Z <- csdp(X,C,n,list(type=c("s"),size=n))$X[[1]]
  
  # Décomposition spectral de Z
  # Calcul du vecteur propre associé à la plus grande valeur propre
  xHat <- eigen(Z)$vectors[,1]
  
  # Clustering
  community <- sign(xHat)

  # Retour des résultats
  return(community)
  
}


# Application au graphe précédent

A <- as.matrix(get.adjacency(Graph))
testGraph <- comDetect(A)

# Code couleur en fonction des prédictions de communautés
colorsCD1 <- ifelse(testGraph==1,'red','blue')

# On vérifie graphiquement que le clustering est parfait
plotGraph(Graph,colorsCD1)






##### ÉTAPE 6 - Automatisation du calcul du taux de bonne reconstitution des communautés #####


# --> Pred désigne les prédictions entrées (vecteur de 'red' ou 'blue')
computeAccuracy <- function(Pred){
  
  n <- length(Pred)
  trueLabels <- c(rep('blue',n/2), rep('red',n/2))
  
  # Calcul du taux de bonnes prédictions
  accuracy <- max(table(trueLabels==Pred))/n
  
  # Retour des résultats
  return(accuracy)
}



##### ÉTAPE 6 - Automatisation du calcul du taux de bonne reconstitution des communautés #####


# --> Pred désigne les prédictions entrées (vecteur de 'red' ou 'blue')
computeAccuracy <- function(Pred){
  
  n <- length(Pred)
  trueLabels <- c(rep('blue',n/2), rep('red',n/2))
  
  # Calcul du taux de bonnes prédictions
  accuracy <- max(table(trueLabels==Pred))/n
  
  # Retour des résultats
  return(accuracy)
}





##### ÉTAPE 7 - Réseaux parcimonieux et détection de communautés #####

# Désormais, nous créons une fonction qui va nous permettre de comparer l'efficacité des 
# deux méthodes créées précédemment. Pour des paramètres spécifiés en argument :
# 1) nous générons N graphes du SBM G(n,p,q)
# 2) nous tentons de reconstruire les communautés pour chaque graphe, avec les deux méthodes :
#     - clustering spectral
#     - méthode de Guédon et Vershynin
# 3) nous renvoyons les taux de bonne prédiction moyens et les écarts-types
# (approche Monte Carlo)


monteCarloAnalysis <- function(N, n, p, q){
  
  # Création de vecteurs pour stocker les résultats
  scoreSC <- rep(0,N) ; scoreCD <- rep(0,N)
  
  # Boucle Monte Carlo
  for(i in 1:N){
    
    # Stochastic Block Model
    G <- sample_sbm(n=n, pref.matrix=matrix(c(p,q,q,p),2,2), block.sizes=c(n/2,n/2))
    
    # Récupération de la matrice d'adjacence
    A <- as.matrix(get.adjacency(G))
    
    # Détection de communautés via les deux méthodes
    testG <- comDetect(A) ; colorsCD <- ifelse(testG==1,'red','blue')
    testG2 <- spectralClustering(A) ; colorsSC <- ifelse(testG2==1, 'red', 'blue')
    
    # Stockage des résultats
    scoreSC[i] <- computeAccuracy(colorsSC)
    scoreCD[i] <- computeAccuracy(colorsCD)
    message("************** Loop : ", i, " *************" )
  }
  
  # Retour des résultats
  message("Précision moyenne Clustering Spectral : ", mean(scoreSC))
  message("Écart-type : ", sd(scoreSC))
  message("Précision moyenne Méthode de Guédon and Vershynin : ", mean(scoreCD))
  message("Écart-type : ", sd(scoreCD))
  c( mean(scoreSC), sd(scoreSC),mean(scoreCD), sd(scoreCD) )
}



# Quelques exemples d'application (voir rapport)


A <- monteCarloAnalysis(100,200,0.3,0.05)

B <- monteCarloAnalysis(100,1000,0.03,0.001)

C <- monteCarloAnalysis(100,1000,0.005,0.0005)






##### ÉTAPE 8 - General Stochastic Block Models #####


# Code pour visualiser la génération d'un graphe à 4 communautés
# à partir d'un General Stochastic Block Model

probaMatrix <- matrix(0.005,4,4)
diag(probaMatrix) <- rep(0.3,4)
G1 <- sample_sbm(n=200, pref.matrix=probaMatrix, block.sizes=c(50,50,50,50))

Labels <- c(rep("red",50),rep("blue",50),rep("yellow",50),rep("green",50))

plotGraph(G1,Labels)
