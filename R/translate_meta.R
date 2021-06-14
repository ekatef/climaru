translate_meta <-
function(colnames_raw){

    colnames_transl_df <- data.frame(
     
        # tas-pre data base
        ru = c("Indeks VMO", "God", "Mesyats", "Den", 

            # tas-pre parameters
            "Obshchiy priznak kachestva temperatur", 
            "Minimalnaya temperatura vozdukha", "Srednyaya temperatura vozdukha", "Maksimalnaya temperatura vozdukha", 
            "Kolichestvo osadkov",

            # snow observations
            "Vysota snezhnogo pokrova",    
            "Stepen pokrova okrestnosti stantsii snegom",
            "Dopolnitelnaya informatsiya o vysote snezhnogo pokrova",
            "Priznak kachestva po vysote snezhnogo pokrova",
            "Dopolnitelnaya informatsiya s uchetom temperatury vozdukha",

            # atmospheric phenomena
            # TODO Is there any difference between "Indeks VMO" and "Sinopticheskiy indeks stantsii"?
            "Sinopticheskiy indeks stantsii",
            "God po Grinvichu",
            "Mesyats po Grinvichu",
            "Den po Grinvichu",
            "Srok po Grinvichu",
            "God istochnika (mestnyy)",
            "Mesyats istochnika (mestnyy)",
            "Den istochnika (mestnyy)",
            "Srok istochnika",
            "Nomer sroka v sutkakh po PDZV",
            "Vremya mestnoye",
            "Nomer chasovogo poyasa",
            "Nachalo meteorologicheskikh sutok po PDZV",
            "Nomer atmosfernogo yavleniya",
            "Shifr atmosfernogo yavleniya",
            "Intensivnost atmosfernogo yavleniya",
            "Vremya nachala AYA (nat.znacheniye chasy min)",
            "Vremya okonchaniya AYA (nat.znacheniye chasy min)",

            # wind observations
            "Napravleniye vetra",
            "Srednyaya skorost vetra",
            "Maksimalnaya skorost vetra",
            "Atmosfernoye davleniye na urovne stantsii",
            "Atmosfernoye davleniye na urovne morya",
            "Kharakteristika baricheskoy tendentsii",
            "Velichina baricheskoy tendentsii",

            # soil temperature observations
            "Koordinatnyy nomer stantsii",
            "Temperatura na glubine   sm uvelichennaya v raz",

            # snow tracking
            "Tip marshruta",
            "Stepen pokrytiya okrestnosti stantsii snegom",
            "Stepen pokrytiya marshruta snegom",
            "Stepen pokrytiya marshruta ledyanoy korkoy",
            "Srednyaya vysota snezhnogo pokrova na marshrute (sm)",
            "Naibolshaya vysota snezhnogo pokrova na marshrute (sm)",
            "Naimenshaya vysota snezhnogo pokrova na marshrute (sm)",
            "Srednyaya plotnost snega",
            "Srednyaya tolshchina ledyanoy korki",
            "Tolshchina sloya snega nasyshchennogo vodoy (mm)",
            "Tolshchina sloya chistoy vody (mm)",
            "Zapas vody v snege (mm)",
            "Zapas vody obshchiy (mm)",
            "Kharakter zaleganiya snezhnogo pokrova",
            "Kharakter snezhnogo pokrova" 

            ),


        en = c("st_id",      "year",  "month", "day",
            
            # tas-pre parameters
            "t_quality_flag",
            "t_min", "t_avr", "t_max",
            "pre",

            # snow observations
            "h_snow",
            "snow_cover",
            "h_snow_details",
            "h_snow_quality",
            "snow_tas_details",

            # atmospheric phenomena
            "st_id", #"st_id_synopt",
            "year_Greenwich",
            "month_Greenwich",
            "day_Greenwich",
            "hour_Greenwich",
            "year_local",
            "month_local",
            "day_local",
            "hour_local",
            "n_hour_in_day",
            "time_local",
            "i_time_zone",
            "day_start",
            "i_atm_phen",
            "code_atm_phen",
            "intensity_atm_phen",
            "begin_atm_phen",
            "end_atm_phen",

            # wind observations
            "wind_direction", 
            "srfw", 
            "srfw_max", 
            "press_station", 
            "press_sl", 
            "bar_tendency_character", 
            "bar_tendency_value", 

            # soil temperature observations
            "st_n_coordin",
            "tas_Xcm_onX",

            # snow tracking
            "track_type",
            "snow_cover_station",
            "snow_cover_track",
            "ice_cover_track",
            "h_mean_snow_track_cm",
            "h_max_snow_track_cm",
            "h_min_snow_track_cm",
            "snow_density",
            "h_ice",
            "h_humide_snow_mm",
            "h_water_mm",
            "water_in_snow_mm",
            "overall_water_mm",
            "snow_cover_type",
            "snow_type"

            ),
     
        stringsAsFactors = FALSE)
     
    colnames_en <- paste("col", seq(along.with = colnames_raw), sep = "_")

    # TODO trim whitespaces in `colnames_raw`
     
    for ( i in seq(along.with = colnames_raw) ) {
        ru_en_corresp <- colnames_transl_df$ru %in% colnames_raw[i]
         if ( any(ru_en_corresp) ){
             colnames_en[i] <- colnames_transl_df$en[which(ru_en_corresp)]
         }
    }

    return(colnames_en)

}
