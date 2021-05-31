\name{climaru-package}
\alias{climaru-package}
\alias{climaru}
\docType{package}
\title{
Preprocessing raw meteorological records provided by the World Data Center in Russia
}

\description{

Helper package for preprocessing meteorological records provided by the All-Russian Research Institute of Hydrometeorological Information - World Data Center (RIHMI WDC). The data base is a unique source of the original observations data on the weather and climate (ambient air temperature, precipitation amount, atmospheric pressure, humidity, soil temperature etc) across a large part of Northern Eurasia. The data are available via meteo.ru/data after a simple registration procedure (aisori-m.meteo.ru/waisori/registration.xhtml). The package is intended to facilitate loading, extraction and meta-data management.
}

\details{
Package is intended for use with the raw meteorological archives loaded via meteo.ru. Following options are available. 

1. Restore meta information of the data containing in the archive. These meta are terms in Russian transliterated by the Latin according to some quite arbitrary set of the rules.

2. Attempt to translate meta information into English using a set of keys. This may fail because of some non-standard data names; the raw Russian-Latin meta data would then be returned.

3. Read the data records for a given meteorological station. The station identifier should be known for this procedure.
}


\references{
S. Broennimann et al. "Observations for Reanalyses". Bulletin of the American Meteorological Society 99.9 (2018): 1851-1866. https://doi.org/10.1175/BAMS-D-17-0229.1
}
