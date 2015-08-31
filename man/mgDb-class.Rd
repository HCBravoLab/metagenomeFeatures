\name{MgDb-class}
\alias{class:MgDb}
\alias{MgDb}

\title{MgDb objects}

\description{
    The \code{MgDb} class is a data object for storing and accessing metagenome marker gene database sequences and their taxonomic annotations.  \code{gg13.5MgDb} in the greengenes13.5MgDb package is an example \code{MgDb} object.
}


\section{Methods}{
  In the code snippets below, \code{x} is a MgDb object.

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
        \code{select(x, ...)}:
            Subsets \code{x}, sequence and/or taxonomic databases based on user defined keytypes, and keys. See \code{?\link{MgDb-select}} for additional documentation.
    }

    \item{}{
        \code{annotate(x)}:
            Generates a \code{?\link{metagenomeAnnotation-class}} object for a
            given set of sequence data.
    }
}

\author{Nathan Olson}


\examples{
# See vignettes
}

\keyword{methods}
\keyword{classes}
