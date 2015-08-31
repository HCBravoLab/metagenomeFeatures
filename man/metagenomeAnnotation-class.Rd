\name{metagenomeAnnotation}
\Rdversion{1.0}
\docType{class}
\alias{metagenomeAnnotation-class}
\alias{[,metagenomeAnnotation,ANY,ANY,ANY-method}
\alias{[,metagenomeAnnotation-method}
\alias{split_by,metagenomeAnnotation-method}

\title{Class "metagenomeAnnotation" -- Metagenomic marker gene sequence set with taxonomic annotations.}
\description{An object with metagenome marker gene sequences (\code{DNAStringSet} Object), and taxonomic annotations(\code{AnnotatedDataFrame}).}
\section{Objects from the Class}{
Objects should be created with calls to \code{annotate}. Where MgDb is a \code{mgDb} reference class object such as \code{gg13.5MgDb} in the greengenes13.5MgDb package.
}
\section{Extends}{
Class \code{DNAStringSet} (package 'Biostrings'), directly. % not sure how this applies to this class
Class \code{AnnotatedDataFrame} (package 'Biobase'), directly.
}
\note{
\code{metagenomeAnnotation} is a class which combines sequence data from a metagenome experiment with taxonomic annotations for that data for use in exploring the taxonomic composition of the sequenced sample.
}

\section{Methods}{
    Class-specific methods.
 \describe{
    \item{\code{split_by}}{Subseting operation.  Splits the \code{metagenomeAnnotation} object into a list of \code{metagenomeAnnotation} objects by a specified taxonomic level.
  }
}


\examples{
For an explanation of usage, see the vignette \code{Example_16S_Annotation_Workflow}.
}
