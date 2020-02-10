% $Author: sinnwell $ 
% $Date: 2009/11/19 18:51:47 $ 

\name{plot.pedigree.shrink}
\alias{plot.pedigree.shrink}
\title{
  plot pedigree.shrink object that is a shrunk pedigree object
}
\description{
Plot the pedigree object that is the trimmed pedigree.shrink object
along with colors based on availability and affection status.
}
\usage{
\method{plot}{pedigree.shrink}(x, bigped=FALSE, title="", xlegend="topright", \dots)
}
\arguments{
\item{x}{
 A pedigree.shrink object, which contains a pedigree object and
 information about which subject was removed.
}
\item{bigped}{
Logical value indicating whether pedigree should be compacted to fit
in plotting region.  If T, then packed=F is used in pedigree() along
with smaller symbol sizes.
}
\item{title}{
  Optional plot title
}
\item{xlegend}{
  The x argument for the legend command, which allows coordinates or,
  more conveniently, options such as "topright", "right", "left", "bottomleft",
  etc., which is useful for pedigrees that cover most of the plot region.
}
\item{\dots}{
Optional arguments to plot method
}
}
\seealso{
  \code{\link{pedigree.shrink}},
}
\examples{
data(sample.ped)

fam2 <- sample.ped[sample.ped$ped==2,]
ped2 <- pedigree(fam2$id, fam2$father, fam2$mother, fam2$sex,
                 fam2$affected, fam2$avail)

shrink2 <- pedigree.shrink(ped2,avail=fam2$avail)
shrink2

## to plot:
#plot(ped2)
#plot.pedigree.shrink(shrink2, title="Sample Pedigree 2")




}
% docclass is function
% Converted by Sd2Rd version 43268.
