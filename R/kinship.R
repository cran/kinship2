# Automatically generated from all.nw using noweb
kinship <- function(id, ...) {
    UseMethod('kinship')
    }

kinship.default <- function(id, dadid, momid, ...) {
    n <- length(id)
    if (n==1) 
        return(matrix(.5,1,1, dimnames=list(id, id)))
    if (any(duplicated(id))) stop("All id values must be unique")
    kmat <- diag(n+1) /2
    kmat[n+1,n+1]    <- 0 

    pdepth <- kindepth(id, dadid, momid)
    mrow <- match(momid, id, nomatch=n+1) #row number of the mother
    drow <- match(dadid, id, nomatch=n+1) #row number of the dad 

    for (depth in 1:max(pdepth)) {
        indx <- (1:n)[pdepth==depth]
        for (i in indx) {
            mom <- mrow[i]
            dad <- drow[i]
            kmat[i,]  <- kmat[,i] <- (kmat[mom,] + kmat[dad,])/2
            kmat[i,i] <- (1+ kmat[mom,dad])/2
            }
        }
    
    kmat <- kmat[1:n,1:n]
    dimnames(kmat) <- list(NULL, id)
    kmat
    }
kinship.pedigree <- function(id, ...) {
    n <- length(id$id)
    if (n==1) 
        return(matrix(.5,1,1, dimnames=list(id$id, id$id)))
    if (any(duplicated(id$id))) stop("All id values must be unique")
    kmat <- diag(n+1) /2
    kmat[n+1,n+1]    <- 0 

    pdepth <- kindepth(id)
    mrow <- ifelse(id$mindex ==0, n+1, id$mindex)
    drow <- ifelse(id$findex ==0, n+1, id$findex)

    for (depth in 1:max(pdepth)) {
        indx <- (1:n)[pdepth==depth]
        for (i in indx) {
            mom <- mrow[i]
            dad <- drow[i]
            kmat[i,]  <- kmat[,i] <- (kmat[mom,] + kmat[dad,])/2
            kmat[i,i] <- (1+ kmat[mom,dad])/2
            }
        }
    
    kmat <- kmat[1:n,1:n]
    dimnames(kmat) <- list(id$id, id$id)
    kmat
    }    
kinship.pedigreeList <- function(id, ...) {
    famlist <- unique(id$famid)
    nfam <- length(famlist)
    matlist <- vector("list", nfam)
   
    for (i in 1:length(famlist)) {
        tped <- id[i]  #pedigree for this family
        temp <- try(kinship(tped), silent=TRUE)
        if (class(temp)=="try-error") 
            stop(paste("In family", famlist[i], ":", temp))
        else matlist[[i]] <- as(forceSymmetric(temp), "dsCMatrix")
    }
    result <- bdiag(matlist)
    if (any(duplicated(id$id)))
        dimnames(result) <- list(NULL, paste(id$famid, id$id, sep='/'))
    else dimnames(result) <- list(id$id, id$id)
    
    result
}
