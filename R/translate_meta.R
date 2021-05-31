translate_meta <-
function(colnames_raw){

    colnames_transl_df <- data.frame(
     
        # tas-pre data base
        ru = c("Indeks VMO", "God", "Mesyats", "Den", 
            "Obshchiy priznak kachestva temperatur", 
            "Minimalnaya temperatura vozdukha", "Srednyaya temperatura vozdukha", "Maksimalnaya temperatura vozdukha", 
            "Kolichestvo osadkov"),

        en = c("st_id",      "year",  "month", "day",
            "t_quality_flag",
            "t_min", "t_avr", "t_max",
            "pre"),
     
        stringsAsFactors = FALSE)
     
    colnames_en <- paste("col", seq(along.with = colnames_raw), sep = "_")
     
    for ( i in seq(along.with = colnames_raw) ) {
        ru_en_corresp <- colnames_transl_df$ru %in% colnames_raw[i]
         if ( any(ru_en_corresp) ){
             colnames_en[i] <- colnames_transl_df$en[which(ru_en_corresp)]
         }
    }

    return(colnames_en)

}
