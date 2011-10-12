# Automatically generated from all.nw using noweb
kinship <- function(id, ...) {
    UseMethod('kinship')
    }

kinship.default <- function(id, dadid, momid, ...) {
    n <- length(id)
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
    
    kmat <- forceSymmetric(Matrix(kmat[1:n,1:n]))
    dimnames(kmat) <- list(id, id)
    kmat
    }
kinship.pedigree <- function(id, ...) {
    n <- length(id$id)
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
    
    kmat <- forceSymmetric(Matrix(kmat[1:n,1:n]))
    dimnames(kmat) <- list(id$id, id$id)
    kmat
    }    
kinship.pedigreeList <- function(id, ...) {
    plist <- id  #rename, to make the code below easier to read
    if (any(duplicated(plist$id))) addfamid <- TRUE
    else                           addfamid <- FALSE
    famlist <- unique(plist$famid)
    rowindex <- integer(0)
    nrows <- integer(0)
    currentrow <- 0L
    elements <- NULL
    newid <- NULL
    for (i in 1:length(famlist)) {
        tped <- plist[i]  #pedigree for this family
        temp <- try(kinship(tped), silent=TRUE)
        if (class(temp)=="try-error") 
            stop(paste("In family", famlist[i], ":", temp))
        else kmat <- as(temp, "dsCMatrix")
        related <- (rowSums(kmat>0) >1) #this person is related to someone else
        if (any(related)) {
            temp <- kmat[related, related, drop=FALSE]
            elements <- c(elements, temp@x)
            rowindex <- c(rowindex, temp@i + currentrow)
            nrows <- c(nrows, diff(temp@p))
            currentrow <- currentrow + nrow(temp)
            }
        if (any(!related)) {
            temp <- kmat[!related, !related, drop=FALSE]
            elements <- c(elements, temp@x)
            rowindex <- c(rowindex, temp@i + currentrow)
            nrows <- c(nrows, diff(temp@p))
            currentrow <- currentrow + nrow(temp)
            }

        if (addfamid) {
            temp <- paste(famlist[i], c(tped$id[related], tped$id[!related]), 
                          sep='/')
            newid <- c(newid, temp)
            }
        else newid <- c(newid, tped$id[related], tped$id[!related])
        }
    new("dsCMatrix", i=rowindex, p=cumsum(c(0L, nrows)), 
        Dim=c(currentrow, currentrow), Dimnames=list(newid, newid),
        x=elements, uplo="U", factors=list())

    }                           
