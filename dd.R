## dd.R
## C code looks at grid of all pairwise points
## nU and nV are integers, only look at smallest points from 1:nU and 1:nV
dd <- function(U,V,nU=NULL,nV=NULL)
{
    if(length(U)!=length(V))
    { cat("U and V must be of same length\n"); return(NA); }
    if(sum(duplicated(U))>0||sum(duplicated(V))>0)
    { cat("no tied data allowed\n"); return(NA); }
    if(sum(is.na(U)||is.na(V))>0)
    { cat("no missing data allowed\n"); return(NA); }
    
    ## the C code is written in terms of empirical CDFs
    ## take negatives of U and V
    ## this is equivalent to writing C code for empirical survival fxns
    U <- -as.numeric(U);
    V <- -as.numeric(V);

    ord <- sort.list(U,method="quick",na.last=NA);
    U <- U[ord];
    V <- V[ord];
    Vord <- sort.list(V,method="quick",na.last=NA)-1; ## subtract 1 bc C indices start at 0
    n <- length(U);

    if(is.null(nU)){ nU <- n; } else { nU <- min(n,nU); }
    if(is.null(nV)){ nV <- n; } else { nV <- min(n,nV); }
    
    ret <- .Call("dd",as.numeric(U),as.integer(Vord),
              as.integer(nU),as.integer(nV));
    D <- ret[1]*sqrt(n/log(n));
    p <- 1-exp(-D^-2);
    return(list(D=D,p=p,Us=ord[1:ret[2]],Vs=ord[Vord+1][1:ret[3]]));
}
