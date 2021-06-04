\name{translate_meta}
\alias{translate_meta}
\title{
Translate Russian columns names
}

\description{
Translate somewhat enigmatic meta information containing in the RIHMI WDC database to plain English.
}

\usage{
translate_meta(colnames_raw)
}

\arguments{
  \item{colnames_raw}{
The character vector containing raw columns names of the data containing in the meteorologic zip archive.
}
}

\value{
The character vector of the data columns names translated in English if available. Raw column names are kept if the translation failed.
}

\details{
The function is intended for use on the zip archive loaded via meteo.ru. The original transliterated observations meta-information is transformed into a set of short notations. These notations are aimed to be used further as proper variables names or columns names and are thus shortened. Notations meaning is as follows:


\bold{"st_id"} is an identifier of the meteorological station according to the World Meteorological Organization (WMO Station Number);

\bold{"year"} is the year of the observation;

\bold{"month"} is the month of the observation;

\bold{"day"} is the day of the observation;

\bold{"t_quality_flag"} is the quality flag of the temperature measurement;

\bold{"t_min"} is the minimum temperature value;

\bold{"t_avr"} is the average temperature value;

\bold{"t_max"} is the maximum temperature value;

\bold{"pre"} is the precipitation amount;

\bold{"h_snow"};

\bold{"snow_cover"} is the snow cover defined as a percentage of an area covered by snow;

\bold{"h_snow_details"};

\bold{"h_snow_quality"};

\bold{"snow_tas_details};

\bold{"st_id_synopt"};

\bold{"year_Greenwich"};

\bold{"month_Greenwich"};

\bold{"day_Greenwich"};

\bold{"hour_Greenwich"};

\bold{"year_local"};

\bold{"month_local"};

\bold{"day_local"};

\bold{"hour_local"};

\bold{"n_hour_in_day"};

\bold{"time_local"};

\bold{"i_time_zone"};

\bold{"day_start"};

\bold{"i_atm_phen"};

\bold{"code_atm_phen"};

\bold{"intensity_atm_phen"};

\bold{"begin_atm_phen"};

\bold{"end_atm_phen"};

\bold{"st_n_coordin"};

\bold{"tas_Xcm_onX"};

\bold{"track_type"};

\bold{"snow_coverage_station};

\bold{"snow_coverage_track"};

\bold{"ice_coverage_track"};

\bold{"h_mean_snow_track_cm"};

\bold{"h_max_snow_track_cm"};

\bold{"h_min_snow_track_cm"};

\bold{"snow_density"};

\bold{"h_ice"};

\bold{"h_humide_snow_mm"};

\bold{"h_water_mm"};

\bold{"water_in_snow_mm"};

\bold{"overall_water_mm"};

\bold{"snow_coverage_type"};

\bold{"snow_type"}.          

The list is currently under development to include more meteorological variables could be assessed via the RIHMI WDC data base. The \code{"col_[[:digit:]]+"} (\emph{"col_1"}, \emph{"col_2"} etc) is used if the term is not available yet as a replacement of the initial notation.

}

\examples{
translate_meta(c("Indeks VMO", "Mesyats", "God"))

meteo_zip <- system.file("extdata", "wr56089.zip", package = "climaru")
meta_info_raw <- restore_meta(dir_name = NULL, zip_name = meteo_zip)
meta_info_en <- translate_meta(meta_info_raw)
}
\keyword{ manip } 
\keyword{ meteorologic records } 

\references{
	WMO stations list via UN data base http://data.un.org/Data.aspx?d=CLINO&f=ElementCode%3A15}

	Weather Reporting (2012) Volume A. Observing stations. Available via https://library.wmo.int/doc_num.php?explnum_id=9896
}
