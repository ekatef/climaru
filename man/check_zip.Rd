\name{check_zip}
\alias{check_zip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Does correctness check on the data file paths.
}

\description{
Extracts data for a given meteorological station from the raw zip-archive loaded from meteo.ru into a regular dataframe. The dataframe columns names are being attempted to be automatically translated using the metadata.
}
\usage{
check_zip(dir_name, zip_name, st_id)
}

\arguments{
  \item{dir_name}{
The \code{dir_name} is the directory name where the original meteorological archive is stored. May be \code{NULL} if the \code{zip_name} contains the full path to the archive.
}
  \item{zip_name}{
The \code{zip_name} is the name of a specific zip-archive to be processed.
}
}

\details{
An original structure of the data archive is kept and respected.
}

\value{
Data path is returned.
}

\note{
The raw meteorological records are supposed to be loaded via meteo.ru data access service with original zip archive names left.
}

% \examples{
% # full path to the example zip file
% meteo_zip <- system.file("extdata", "wr56089.zip", package = "climaru")
% data_chunk <- check_zip(dir_name = NULL, zip_name = meteo_zip, 
%     st_id = 20069)
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% \keyword{ ~kwd1 } manip
% \keyword{ ~kwd2 } meteorologic records
