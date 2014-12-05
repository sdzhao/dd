## fxns.R

## **************************************************************
## dependency detection
## **************************************************************
dyn.load("dd.so");
source("dd.R");

## **************************************************************
## spearman
## **************************************************************
spearman <- function(U,V)
{
    test <- cor.test(U,V,method="spearman");
    return(list(D=test$estimate,p=test$p.value));
}

## **************************************************************
## max test
## **************************************************************
maxtest <- function(U,V)
{
    m <- max(pmin(U,V));
    n <- length(U);
    nU <- sum(U>=m);
    nV <- sum(V>=m);
    return(list(D=m,p=1-phyper(0,nU,n-nU,nV)));
}
