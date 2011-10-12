\name{NEWS}
\title{NEWS file for the kinship2 package}
\section{Changes in version 1.3.0}{

\itemize{
  \item Added a Minnesota Breast Cancer dataset

  \item Add missid as a code for the parents of founder subjects if the code 
  is not one of the defaults (0 or NA for numeric ids, "" or NA for 
  character)

  \item Fixed some findex/mindex problems in findUnavailable and 
  excludeUnavailFounders, both used within pedigree.shrink.

  \item Updated plot.pedigree.shrink to plot available subjects as red, and 
  included a simple legend to indicate availability coloring. A new 
  parameter, xlegend, can now control the placing of the legend.

  \item Fixed a minor indexing bug in "[.pedigree"

  \item Fixed typing and grammatical problems in error messages in pedigree()

  \item Fixed row/column names in kinship function (Shannon/Terry, please 
  give more details)

  \item Allow pedigree.trim to trim all subjects and return an empty pedigree 
  object

  \item pedigree.legend to show which portions of a subject's plot
  symbol indicate which affection status

  \item aligning of pedigrees now more robust to handle subjects in
  multiple marriages
  
}
}
\section{Release Notes for kinship2, version 1.2.1}{

\itemize{

\item Summary: kinship2 is a branch from the original kinship package with 
some key updates for the pedigree and kinship functions, with some additional
functions that work with the pedigree object.

\item pedigree and pedigreeList objects implemented with S3 methods 
   print and "[,"
\item pedigree plotting routine re-written, now more robust
\item kinship matrices now implemented with Matrix package classes
\item utilities to trim the pedigree object with various criteria

}}
