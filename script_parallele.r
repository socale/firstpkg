# calcul parallelle avec R

# fonction permettant de calculer le logarithme de n nombre
logarithme=function(x){
  return(log(x))
}

logarithme_for=function(x){
  y=rep(NA,length(x))
  for(i in 1:length(x))
    y[i]=log(x[i])
  return(y)
}

# script pour la parallelisation
log_paral=function(x){
  # nombre de coeurs diponibles sur mon pC
  nbre=parallel::detectCores()
  ncores=nbre-1
  m=length(x)
  # initialiser l'environnement de parallelisation ie les clusters, de preference avec nombre de cluster -1.
  cl=parallel::makeCluster(ncores)
  # déclaration du cluster initialise precedemment. ceci permet de faire recours a des coeurs diponibles sur un cluster de calcul pour le calcul parallel
  doParallel::registerDoParallel(cl)
  # separer les donnees en autant de groupe que de coeurs: pour chaque coeur retourne les indice a evaluer par ce coeur.
  # A chaque appel, retourne les indice du processus courant
  vect_iter=itertools::isplitIndices(n=m,chunks=ncores) #chunks prend le nombre de coeurs a utiliser

  # utilisation de foreach pour le calcul de log(x) en parrallele
  res <- foreach(i=vect_iter, .combine='c',.packages = "firstpkg") %dopar% {
    # si besoin de faire un simple do en sequentille, utiliser %do%
    # si besoin d'utiliser la parallelisation, alors utiliser %dopar% et peciser les noms de package et et fonction et variables locale a exporter par chaque processus
   log(x[i])
  }

  # on libere les coeurs precedemment occupes
  parallel::stopCluster(cl)
  return(res)
}

# test des performance de mes 3 algorithme: Globalement, on perd du temps avec la parallelisation car l'operation log est tres rapide et prend
# moins de temps que les echanges entre les processus

n=5
mb <- microbenchmark(logarithme(seq(1,n)),log_paral(seq(1,n)),logarithme_for(seq(1,n)),times=5L)
mb
