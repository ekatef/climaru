\name{show_stations}
\alias{show_stations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to automatically restore meta data for a selected meteorological archive.
}

\description{
Extract station identifiers contained in the records archive.
}

\usage{
show_stations(st_ids, main_txt, sub_txt, x_range, y_range)
}

\arguments{
  \item{st_ids}{
The \code{st_ids} is a vector of meteorological stations identifiers to be shown on the map.
}
  \item{main_txt}{
The \code{main_txt} is a text string to annotate the produced map.
}
  \item{sub_txt}{
The \code{sub_txt} is a text string to put additional information to the produced map.
}
  \item{x_range}{
The \code{x_range} is a latitude range to be shown.
}
  \item{y_range}{
The \code{y_range} is a lontitude range to be shown.
}
}

\value{
A plot
}

\details{
The function is plot the requested stations.

}

% \examples{
% show_stations(st_ids = 20069)
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip } 
\keyword{ meteorologic records } 