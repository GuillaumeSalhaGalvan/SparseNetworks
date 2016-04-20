# SparseNetworks
[ENSAE] Review and implementations from the article "Community Detection in Sparse Networks via Grothendieck's Inequality" 

L'objectif de ce projet est de proposer une revue de l'article **Community Detection in Sparse Networks via Grothendieck's Inequality** de Guédon et Vershynin (2015). Dans cet article, les auteurs présentent une méthode flexible pour prouver la précision de certains **problèmes d'optimisation SDP** (pour *semidefinite programming*) 
pour la résolution de problèmes stochastiques variés. La méthode, basée notamment sur **l'inégalité de Grothendieck**, est générale.

Nous l'appliquons ici au problème de **détection de communautés** et notamment au sein de **réseaux parcimonieux**.
Après un tour d'horizon des principaux résultats mathématiques de l'article, nous proposons une implémentation sur des graphes construits via le classique **Stochastic Block Model**. Ces implémentations illustrent la capacité de la méthode à détecter la structure de communauté même au sein de réseaux parcimonieux, avec un taux d'erreur arbitrairement fixé. 

Nous comparons enfin la méthode avec le **clustering spectral**, via des simulations Monte Carlo. 
Cette approche alternative permet d'obtenir des résultats satisfaisant pour des graphes relativement denses, mais s'avère moins adaptée que celle de Guédon et Vershynin (2015) pour des graphes parcimonieux, pour des raisons intuitives et mathématiques détaillées dans l'article.

===============


<sup>*Gautier Appert and Guillaume Salha*

<sup>*École Nationale de la Statistique et de l'Administration Économique (ENSAE ParisTech)*

<sup>*3A Data Science - Statistics and Learning*

<sup>*March 2016*
