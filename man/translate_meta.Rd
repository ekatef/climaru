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

\bold{"h_snow"} is the snow height;

\bold{"snow_cover"} is the snow cover defined as a percentage of an area covered by snow;

\bold{"h_snow_details"} is an additional information related to the snow height;

\bold{"h_snow_quality"} is the quality flag of the snow height measurment;

\bold{"snow_tas_details} is an additional information related to snow and the temperature conditions;

\bold{"year_Greenwich"} is an observation year according to UTC;

\bold{"month_Greenwich"} is an observation month according to UTC;

\bold{"day_Greenwich"} is an observation day according to UTC;

\bold{"hour_Greenwich"} is an observation hour according to UTC;

\bold{"year_local"} is an observation year according to the local time;

\bold{"month_local"} is an observation month according to the local time;

\bold{"day_local"} is an observation day according to the local time;

\bold{"hour_local"} is an observation hour according to the local time;

\bold{"i_obs_in_day"} is number of the observation counted inside the day;

\bold{"time_local"} is the local time of the observation;

\bold{"i_time_zone"} a time zone number;

\bold{"day_start"} a start of the meteorological day;

\bold{"i_atm_phen"};

\bold{"code_atm_phen"};

\bold{"intensity_atm_phen"};

\bold{"begin_atm_phen"};

\bold{"end_atm_phen"};

\bold{"wind_direction"};

\bold{"srfw"} is the average surface wind speed;

\bold{"srfw_max"} is the maximum surface wind speed;

\bold{"press_station"} is the pressure on the station level;

\bold{"press_sl"} is the sea level pressure;

\bold{"bar_tendency_character"} is a baric tendency type;

\bold{"bar_tendency_value"} is the baric tendency value;

\bold{"st_n_coordin"} is a coordinate station number;

\bold{"tas_Xcm_onY"} is the soil temperature on the X cm multiplied on Y;

\bold{"track_type"} is the track type;

\bold{"snow_coverage_station} is the snow cover value observed at the station;

\bold{"snow_coverage_track"} is the snow cover value observed along the track;

\bold{"ice_coverage_track"} is the ice layer height observed along the track;

\bold{"h_mean_snow_track_cm"} is the mean snow level along the track in centimeters;

\bold{"h_max_snow_track_cm"} is the maximun snow level along the track in centimeters;

\bold{"h_min_snow_track_cm"} is the minimum snow level along the track [cm];

\bold{"snow_density"} is the mean snow density;

\bold{"h_ice"} is the mean thickness of the ice layer;

\bold{"h_humide_snow_mm"} is the thickness of the snow saturated with water;

\bold{"h_water_mm"} is the water level [mm];

\bold{"water_in_snow_mm"} is the water amount in the snow [mm];

\bold{"overall_water_mm"} is the overall water amount [mm];

\bold{"snow_cover_type"} is the snow cover type;

\bold{"snow_type"} is the snow type.          

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
