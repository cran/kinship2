\name{plot.pedigree}
\alias{plot.pedigree}
\title{
  plot pedigrees
}
\description{
plot objects created with the pedigree function
}
\usage{
\method{plot}{pedigree}(x, id = x$id, status = x$status, affected =
                 x$affected, cex = 1, col = 1, symbolsize = 1, branch =
                 0.6, packed = TRUE, align = c(1, 2), width = 8,
                 density = c(-1, 35, 55, 25), mar = c(4.1, 1, 4.1, 1),
                 angle = c(90, 65, 40, 0), keep.par = FALSE, subregion,
                 ...)
}
\arguments{
\item{x}{
object created by the function pedigree.
}
\item{id}{
id variable - used for labeling.  
}
\item{status}{
can be missing.  If it exists, 0=alive/missing and 1=death.
}
\item{affected}{
vector, or matrix with up to 4 columns for affected
indicators. Subject's symbol is divided into sections for each
status, shaded if indicator is 1, not-shaded for 0, and symbol "?"
if missing (NA)
}
\item{cex}{
controls text size.  Default=1.
}
\item{col}{
color for each id.  Default assigns the same color to everyone.
}
\item{symbolsize}{
controls symbolsize. Default=1.
}
\item{branch}{
defines how much angle is used to connect various levels of nuclear families.
}
\item{packed}{
default=T.  If T, uniform distance between all individuals at a given level.
}
\item{align}{
default=T.  Indicates extra effort should be spent trying to align parents
and children.  Set to F to speed up plotting.
}
\item{width}{
default=8.  For a packed pedigree, the minimum width allowed in the
realignment of pedigrees.
}
\item{density}{
defines density used in the symbols.  Takes up to 4 different values.
}
\item{mar}{margin parmeters, as in the \code{par} function}
\item{angle}{
defines angle used in the symbols.  Takes up to 4 different values.
}
\item{keep.par}{
Default = F, allows user to keep the parameter settings the same as they
were for plotting (useful for adding extras to the plot)
}
\item{subregion}{4-element vector for (min x, max x, min depth, max depth),
  used to edit away portions of the plot coordinates returned by
  align.pedigree
  }
\item{\dots}{
Extra options that feed into the plot function.
}
}
\value{
returns points for each plot plus original pedigree.
}
\section{Side Effects}{
creates plot on current plotting device.
}
\seealso{
\code{\link{pedigree}}
}
\examples{
data(sample.ped)

pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, 
       sample.ped$sex,  #affected=sample.ped$affected,
       affected=cbind(sample.ped$affected, sample.ped$avail), 
       famid=sample.ped$ped)

ped2 <- pedAll['2']

print(ped2)


## plot(ped2)

}

\keyword{hplot, genetics}
