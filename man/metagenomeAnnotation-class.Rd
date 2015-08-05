\name{metagenomeAnnotation}
\Rdversion{1.0}
\docType{class}
\alias{metagenomeAnnotation-class}
\alias{[,metagenomeAnnotation,ANY,ANY,ANY-method}
\alias{[,metagenomeAnnotation-method}
\alias{split_by,metagenomeAnnotation-method}

\title{Class "metagenomeAnnotation" -- Metagenomic marker gene sequence set with taxonomic annotations.}
\description{An object with metagenome marker gene sequences, DNAStringSet Object, and taxonomic annotations, AnnotatedDataFrame.}
\section{Objects from the Class}{
Objects should be created with calls to \code{\link{MgDb\$annotate}}. Where MgDb is a mgDb reference class object such as gg13.5MgDb in the greengenes13.5MgDb package.
}
\section{Extends}{
Class \code{DNAStringSet} (package 'Biostrings'), directly. # not sure how this applies to this class
Class \code{AnnotatedDataFrame} (package 'Biobase'), directly.
}
\note{ 
Note: This is a summary for reference. For an explanation of the actual usage, see the vignette.

}

\section{Methods}{
    Class-specific methods.
 \describe{
    \item{\code{[}}{}
  }
}

\examples{
# See vignette
}