# Automatically generated from all.nw using noweb

pedigree.trim <- function(removeID, ped){
  ## trim subjects from a pedigree who match the removeID 
  ## trim relation matrix as well

  if(class(ped) != "pedigree")
    stop("Must be a pegigree object.\n")


  keep <- is.na(match(ped$id, removeID))
  keep.relate <- is.na(match(ped$id[ped$relation[,1]], removeID)) &
                 is.na(match(ped$id[ped$relation[,2]], removeID))

  nOrig <- length(ped$id)
  nNew <- sum(keep)
  
  if(nNew > 0) {
  
    newAffected <- if(is.null(ped$affected)) newAffected <- rep(0, nOrig)
    
    if(is.matrix(ped$affected)) {
      newAffected <- ped$affected[keep,]
    } else {
      newAffected <- ped$affected[keep]
    }
    
    id.new <- ped$id[keep] 
    
    
    ## step1: update the father and mother indices
    fid.new <- mid.new <- rep(NA, length(id.new))
    fid.new[ped$findex[keep]>0] <- ped$id[ped$findex[keep]]
    mid.new[ped$mindex[keep]>0] <- ped$id[ped$mindex[keep]]
    
    ## step2: any subject that is to be removed, remove them from being parents
    fid.new[fid.new %in% removeID] <- NA
    mid.new[mid.new %in% removeID] <- NA
    
    ## make new pedigree object with only essential items
    newPed <- pedigree(id=id.new,
                       dadid=fid.new,
                       momid=mid.new, 
                       missid=ped$missid,
                       sex=as.numeric(ped$sex[keep]))
    
    ## trim non-required objects from ped
    if(!is.null(ped$affected)) newPed$affected <- newAffected
    if(!is.null(ped$status)) newPed$status <-  ped$status[keep]
    
    if(!is.null(ped$famid)) newPed$famid <- ped$famid[keep]
    if(!is.null(ped$relation)) 
      newPed$relation <- ped$relation[keep.relate,,drop=FALSE]

  } else {
    ## empty pedigree
    newPed <- list(id=NULL, dadid=NULL, momid=NULL, sex=NULL)
    class(newPed) <- "pedigree"
  }    
  return(newPed)
}


