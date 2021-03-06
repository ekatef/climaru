\name{restore_st_ids}
\alias{restore_st_ids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to automatically restore meta data for a selected meteorological archive.
}

\description{
Extract station identifiers contained in the records archive.
}

\usage{
restore_st_ids(dir_name, zip_name)
}

\arguments{
  \item{dir_name}{
The \code{dir_name} is the directory name where the original meteorological archive is stored. May be \code{NULL} if the \code{zip_name} contains the full path to the archive.
}
  \item{zip_name}{
The \code{zip_name} is a single name of a specific zip-archive to be processed. May contain a full path to a zip archive to be processed.
}
}

\value{
A character vector containing stations identifiers corresponding to the observations stored in the given zip arcive.
}

\details{
The function is intended for use with the zip archive containing meteorological data available via meteo.ru.

May take quite long in case a single records file containing data for many stations.

The \code{dir_name} will be set to `NULL` automatically with a warning in case the supplied \code{zip_name} is a folder path.

}

\examples{
meteo_zip <- system.file("extdata", "wr56089.zip", package = "climaru")
data_chunk <- restore_st_ids(dir_name = NULL, zip_name = meteo_zip)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip } 
\keyword{ meteorologic records } 
