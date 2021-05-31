\name{restore_meta}
\alias{restore_meta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to automatically restore meta data for a selected meteorological archive.
}

\description{
Translate raw meta information in plain English.
}

\usage{
restore_meta(dir_name, zip_name)
}

\arguments{
  \item{dir_name}{
The \code{dir_name} is the directory name where the original meteorological archive is stored. May be \code{NULL} if the \code{zip_name} contains the full path to the archive.
}
  \item{zip_name}{
The \code{zip_name} is a single name of a specific zip-archive to be processed.
}
}

\value{
A character vector containing the names of the data columns which are stored in the given zip arcive.
}

\details{
The function is intended for use with the zip archive containing meteorological data available via meteo.ru.
}

\examples{
meteo_zip <- system.file("extdata", "wr56089.zip", package = "climaru")
data_chunk <- restore_meta(dir_name = NULL, zip_name = meteo_zip)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip } 
\keyword{ meteorologic records } 
