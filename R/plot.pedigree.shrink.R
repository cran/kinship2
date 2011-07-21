# Automatically generated from all.nw using noweb
#$Log: plot.pedigree.shrink.q,v $
#Revision 1.4  2010/09/03 21:12:16  sinnwell
#use shrunk "avail" vector for the colored labels
#
#Revision 1.3  2009/11/19 14:57:18  sinnwell
#*** empty log message ***
#
#Revision 1.2  2009/11/17 23:09:51  sinnwell
#updated for ped object
#
#Revision 1.1  2008/07/16 20:23:38  sinnwell
#Initial revision
#

plot.pedigree.shrink <- function(x, bigped=FALSE, title="", ...){

  ##  Plot pedigrees, coloring available subjects according
  ##   to affection status.

  #col.subj <- ifelse(x$avail==TRUE & is.na(x$pedObj$affected==0), 3, 1)
  #col.subj <- ifelse(x$avail==TRUE & x$pedObj$affected==0, 4, col.subj)
  #col.subj <- ifelse(x$avail==TRUE & x$pedObj$affected==1, 2, col.subj)
  #col.subj <- ifelse(x$avail==TRUE & x$pedObj$affected==1, 5, col.subj)
  
  col.subj <- ifelse(x$avail==TRUE & is.na(x$pedObj$affected==0), 3, 1)
  col.subj <- ifelse(x$avail==TRUE & x$pedObj$affected==0, 4, col.subj)
  col.subj <- ifelse(x$avail==TRUE & x$pedObj$affected==1, 2, col.subj)
  col.subj <- ifelse(x$avail==FALSE & x$pedObj$affected==1, 5, col.subj)
  
  if(bigped==FALSE){
    tmp <- plot(x$pedObj, col=col.subj)
  }

  if(bigped==TRUE){
    tmp <- plot.pedigree(x$pedObj, align=FALSE, packed=F,symbolsize=0.5,col=col.subj) #cex=0.25)
  }
#  browser()
#  legend(c(max(tmp$x)*.75, max(tmp$y)*.9), 
#         legend=c("Unavail+Unaff", "Avail+Aff","Avail+Unaff","Avail+Unk","UnAvail+Aff"),
#         pch=c("*","*","*","*", "*"), col=c(1,2,4,3,5), lty=rep(1,5),bty="n")
  
  title(paste(title, "\nbits = ", x$bitSize[length(x$bitSize)]))
}


