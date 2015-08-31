\name{mgDb-class}
\alias{class:mgDb}
\alias{mgDb}

\title{mgDb objects}

\description{
    The \code{mgDb} class is a data object for storing and accessing metagenome marker gene database sequences and their taxonomic annotations.  \code{gg13.5MgDb} in the greengenes13.5MgDb package is an example \code{mgDb} object.
}


\section{Methods}{
  In the code snippets below, \code{x} is a mgDb object.

  \describe{
    \item{}{
        \code{show(x)}:
            Prints a summary of \code{x}.
    }
    \item{}{
        \code{taxa_keytypes(x)}:
            Returns a vector of \code{x}'s keytypes. Keytypes are the taxonomic classification levels, e.g. Kingdom, Phylum, Class, included in the database.
    }
    \item{}{
        \code{taxa_columns(x)}:
            Same as \code{taxa_keytypes}.
    }
    \item{}{
        \code{taxa_keys(x, keytype)}:
            Returns the taxonomic groups for a given taxonomic classification level, defined by the keytype. Use \code{taxa_keytypes(x)} for valid keytypes.
    }

    \item{}{
        \code{annotate(x)}:
            Generates a \code{?\link{metagenomeAnnotation}} object for a
            given set of sequence data.
    }
}

\author{Nathan Olson}


\examples{
# See vignettes
}

\keyword{methods}
\keyword{classes}
