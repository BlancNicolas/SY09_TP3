#-----------QUESTION 1.1

front.ceuc <- function(mu, Xaff, zaff, discretisation=50)
{
  deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
  deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
  minX <- min(X[,1])-deltaX
  maxX <- max(X[,1])+deltaX
  minY <- min(X[,2])-deltaY
  maxY <- max(X[,2])+deltaY
  
  # grille d'affichage 
  grilleX <- seq(from=minX,to=maxX,by=deltaX)
  naffX <- length(grilleX)
  grilleY <- seq(from=minY,to=maxY,by=deltaY)
  naffY <- length(grilleY)
  grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  
  # calcul des valeurs de la fonction 
  valf <- ceuc.val(grille,mu)
  plot(Xaff, col=c("red","green","blue","magenta","orange")[zaff], asp=1)
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}

ceuc.app <- function(Xapp, zapp)
{
  
  napp <- nrow(Xapp);
  p <- ncol(Xapp);
  matclasse1 <- matrix(0, napp, p);
  matclasse2 <- matrix(0, napp, p);
  mu <- matrix(nrow = 2, ncol = p);
  j = 1;
  k = 1 ; 
  for (i in 1:napp)
  {
    if (zapp[i] == 1){
      matclasse1[j,] = as.matrix(Xapp[i,]);
      j = j + 1;
    }
    else {matclasse2[k,] = as.matrix(Xapp[i,]);
    k = k+1;} #on stocke dans la matrice pour la classe 2 à l'indice k l'individu associé 
  }
  mu[1,] <- (apply(matclasse1[1:j-1,], 2, mean));#applying mean on columns
  mu[2,] <- (apply(matclasse2[1:k-1,],2, mean));
  return(mu);

}

ceuc.val <- function(Xtst, mu) 
{
  ntst <- nrow(Xtst)
  p <- ncol(Xtst)
  etiquette <- vector("numeric", ntst);
  distance <- distXY(Xtst, mu);
  for (i in 1:ntst) {
    if (distance[i, 1] < distance[i, 2])#on cherche à savoir à quelle classe appartient l'individu
    {
      etiquette[i] = 1;
    }
    else etiquette[i] = 2;
  }
  
  return(etiquette);
}


#----------QUESTION 1.2

front.kppv <- function(Xapp, zapp, K, discretisation=50)
{
  deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
  deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
  minX <- min(X[,1])-deltaX
  maxX <- max(X[,1])+deltaX
  minY <- min(X[,2])-deltaY
  maxY <- max(X[,2])+deltaY
  
  # grille d'affichage 
  grilleX <- seq(from=minX,to=maxX,by=deltaX)
  naffX <- length(grilleX)
  grilleY <- seq(from=minY,to=maxY,by=deltaY)
  naffY <- length(grilleY)
  grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  
  # calcul des valeurs de la fonction 
  valf <- kppv.val(Xapp, zapp, K, grille)
  plot(Xapp, col=c("red","green","blue","magenta","orange")[zapp], asp=1)
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}

kppv.val <- function(Xapp, zapp, K, Xtst)
{
  etiquette <- vector(length = nrow(Xtst))
  distanceXtstXapp <- matrix(ncol = nrow(Xtst), nrow = nrow(Xapp))
  
  distanceXtstXapp <- distXY(Xapp,Xtst)
  
  sort_table <- apply(distanceXtstXapp, 2, order); #le 2 signifie qu'on fait le trie sur les colonnes
  sort_table2 <- sort_table;
  
  for(i in 1:K){
    for(j in 1:ncol(sort_table2)){
      sort_table2[i,j] <- zapp[sort_table[i,j]];
    }
  }
  sort_table2 <- sort_table2[1:K,];
  if (is.null(dim(sort_table2))){
    return (sort_table2);
  } 
  else return(round(apply(sort_table2, 2, mean)));
  
}

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv){
  length_nppv <- length(nppv);
  listError <- vector("numeric", length_nppv);
  erreur_opt <- 0;
  for (i in nppv){
    result <- kppv.val(Xapp, zapp, i, Xval);
    erreur <- sum((result == zval) == TRUE)/length(zval)
    if (erreur > erreur_opt){
      erreur_opt <- erreur;
      min <- i
    }
  }
  
  return (min);
}



#euclidean test 
donn <- read.csv("Synth1-40.csv")
X <- donn[,1:2]
z <- donn[,3]

donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

mu <- ceuc.app(Xapp, zapp)
front.ceuc(mu, Xapp, zapp, 1000)

#kpp test 

donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xval <- donn.sep$Xval
zval <- donn.sep$zval
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

Kopt <- kppv.tune(Xapp, zapp, Xval, zval,2*(1:6)-1)
front.kppv(Xapp, zapp, Kopt, 1000)

#Partie 1.2

#Question 1


#fonctions arnaud 



