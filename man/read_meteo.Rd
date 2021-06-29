\name{read_meteo}
\alias{read_meteo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract meteorological records from the raw meteorological archive
}

\description{
Extracts data for a given meteorological station from the raw zip-archive loaded from meteo.ru into a regular dataframe. The dataframe columns names are being attempted to be automatically translated using the metadata.
}
\usage{
read_meteo(dir_name, zip_name, st_id, sep_smb, bulk_file)
}

\arguments{
  \item{dir_name}{
The \code{dir_name} is the directory name where the original meteorological archive is stored. May be \code{NULL} if the \code{zip_name} contains the full path to the archive.
}
  \item{zip_name}{
The \code{zip_name} is the name of a specific zip-archive to be processed.
}
  \item{st_id}{
The \code{st_id} is the name of the meteorological station to be read from the whole zip archive.
}
  \item{sep_smb}{
The \code{sep_smb} is a supposed data separator. Is checked automatically and corrected if necessary.
}
  \item{bulk_file}{
The \code{bulk_file} is a logical file indicating the observations for different stations were put into a single file.
}
}

\details{
The format of the \code{st_id} is the five-digits WMO sstation identifier like "12345". The list of stations may be found via meteo.ru/english/climate/katalog2.html
}

\value{
A dataframe of the observed meteorological time series corresponding to the requested station. The columns names are set according to the meta-data file.
}

\note{
The raw meteorological records are supposed to be loaded via meteo.ru data access service with original zip archive names left untouched. Yes, "wr56089a1.zip" is perfectly alright.
}

% \examples{
% # full path to the example zip file
% meteo_zip <- system.file("extdata", "wr56089.zip", package = "climaru")
% data_chunk <- read_meteo(dir_name = NULL, zip_name = meteo_zip, 
%     st_id = 20069)
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% \keyword{ ~kwd1 } manip
% \keyword{ ~kwd2 } meteorologic records
