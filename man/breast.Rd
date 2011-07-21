 
\name{breast}
\alias{breast}
\docType{data}
\title{ Breast Cancer Pedigrees }
\description{
  Pedigrees from a breast cancer study
}
\usage{data(breast)}
\format{
  A data frame with 17 families with the following 7 variables.
  \describe{

    \item{\code{id}}{ subject id, unique to each ped}
    \item{\code{dadid}}{id of the subject's father}
    \item{\code{momid}}{id of the subject's mother}
    \item{\code{sex}}{ M=male, F=female}
    \item{\code{birth}}{year of birth}
    \item{\code{startage}}{age when first on study}
    \item{\code{endage}}{age at last follow-up}
    \item{\code{cancer}}{breast cancer status; 0=not-present, 1=present, NA=unknown}
    \item{\code{parity}}{indicator for mother having multiple children}
    \item{\code{family}}{family id}
    \item{\code{proband}}{proband status}    
  }
}

\examples{
data(breast)
breast[1:20,]

}
\keyword{datasets}
