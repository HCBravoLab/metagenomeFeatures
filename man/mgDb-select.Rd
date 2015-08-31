\name{MgDb-select}
\alias{select}

\title{
MgDb-select
}
\description{
Select method for \code{?\link{MgDb-class}} objects.  Subsets the \code{MgDb} object based on defined taxonomic level and values.  Can subset the database taxaonomic, sequence data or both.
}

\usage{
    select(x, type, keys, keytype)
    \S4method{select}{MgDb}(x)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{
    \code{?\link{MgDb-class}} object
  }
  \item{type}{
    either "taxa", "seq", or "both" depending on the desired output
  }

  \item{keys}{
    desired taxonomic groups of level \code{keytypes}
  }
  \item{keytypes}{
    \code{keys} taxonomic classification level
  }
}

\value{
    The returned object depends on the \code{type} argument. "seq" returns a DNAStringSet, and "taxa" a dataframe. "both" returns a list with:
    \item{taxa}{dataframe with taxonomic information}
    \item{seq}{DNAStringSet}
}

\author{Nathan Olson}

\examples{
# For obtianing entries of the genus Vibrio and Salmonella.

# Taxonomic data only
testMgDb$select(type = "taxa",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")

# Sequence data only
testMgDb$select(type = "seq",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")

# Both sequence and taxonomic data
testMgDb$select(type = "both",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ select }
