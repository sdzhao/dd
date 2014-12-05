## example.R

rm(list=ls());
source("fxns.R");
library(MASS);

env <- 1; ## set random seed for generating test statistics

## **************************************************************
## parameters
## **************************************************************
n <- 1e5;
betaUs <- c(0.51,0.6,0.7);
betaV <- 0.5;
nu <- sqrt(2*log(n));

## **************************************************************
## function to report results
## **************************************************************
report <- function(U,V,Su,Sv)
{
    ## make test stats two-sided
    U <- abs(U);
    V <- abs(V);
    
    retD <- retp <- c();
    
    fit <- dd(U,V,5000,5000);
    retD <- c(retD,fit$D); retp <- c(retp,fit$p);
    
    fit <- spearman(U,V);
    retD <- c(retD,fit$D); retp <- c(retp,fit$p);
    
    fit <- maxtest(abs(U),abs(V));
    retD <- c(retD,fit$D); retp <- c(retp,fit$p);
    
    return(list(D=retD,p=retp));
}

## **************************************************************
## vary betas
## **************************************************************
null.D <- null.p <- alt.D <- alt.p <- matrix(NA,nrow=3,ncol=length(betaUs));

for(i in 1:length(betaUs))
{
    ## different values for beta (dependency)
    b <- max(betaUs[i],betaV)+0.01;
    mu <- sqrt(2*(betaUs[i]-0.5)*log(n));
    
    cat(i);
    ## ==========================================================
    ## null
    ## ==========================================================
    ## fixed signal positions
    set.seed(1);
    X.null <- sample(1:n,floor(n^round(1-betaUs[i],2)),replace=FALSE);
    Y.null <- sample(1:n,floor(n^round(1-betaV,2)),replace=FALSE);
    
    set.seed(env);
    U <- rnorm(n); U[X.null] <- rnorm(length(X.null),mean=mu,1);
    V <- rnorm(n); V[Y.null] <- rnorm(length(Y.null),mean=nu,1);
    res <- report(U,V,Su,Sv);
    null.D[,i] <- res$D; null.p[,i] <- res$p;
    
    ## ===========================================================
    ## alternative
    ## ===========================================================
    ## 10: U=signal,V=not; 01: opposite; 11: both signal
    set.seed(1);
    alt10 <- sample(1:n,
                    floor(n*(n^-betaUs[i]-n^-(betaUs[i]+betaV)-n^-b)),
                    replace=FALSE);
    if(length(alt10)>0)
    {
        alt01 <- sample((1:n)[-alt10],
                        floor(n*(n^-betaV-n^-(betaUs[i]+betaV)-n^-b)),
                        replace=FALSE);
    } else
    {
        alt01 <- sample(1:n,
                        floor(n*(n^-betaV-n^-(betaUs[i]+betaV)-n^-b)),
                        replace=FALSE);
    }
    if(length(c(alt10,alt01)>0))
    {
        alt11 <- sample((1:n)[-c(alt10,alt01)],
                        floor(n*(n^-(betaUs[i]+betaV)+n^-b)),
                        replace=FALSE);
    } else
    {
        alt11 <- sample(1:n,
                        floor(n*(n^-(betaUs[i]+betaV)+n^-b)),
                        replace=FALSE);
    }
    
    set.seed(env);
    U <- rnorm(n); U[c(alt10,alt11)] <- rnorm(length(c(alt10,alt11)),mean=mu,1);
    V <- rnorm(n); V[c(alt01,alt11)] <- rnorm(length(c(alt01,alt11)),mean=nu,1);
    res <- report(U,V,Su,Sv);
    alt.D[,i] <- res$D; alt.p[,i] <- res$p;
}
