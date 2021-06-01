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
            "Dopolnitelnaya informatsiya s uchetom temperatury vozdukha"
            ),

#  [1] "Sinopticheskiy indeks stantsii"                   
#  [2] "God po Grinvichu"                                 
#  [3] "Mesyats po Grinvichu"                             
#  [4] "Den po Grinvichu"                                 
#  [5] "Srok po Grinvichu"                                
#  [6] "God istochnika (mestnyy)"                         
#  [7] "Mesyats istochnika (mestnyy)"                     
#  [8] "Den istochnika (mestnyy)"                         
#  [9] "Srok istochnika"                                  
# [10] "Nomer sroka v sutkakh po PDZV"                    
# [11] "Vremya mestnoye"                                  
# [12] "Nomer chasovogo poyasa"                           
# [13] "Nachalo meteorologicheskikh sutok po PDZV"        
# [14] "Nomer atmosfernogo yavleniya"                     
# [15] "Shifr atmosfernogo yavleniya"                     
# [16] "Intensivnost atmosfernogo yavleniya"              
# [17] "Vremya nachala AYA (nat.znacheniye chasy min)"    
# [18] "Vremya okonchaniya AYA (nat.znacheniye chasy min)"

# [1] "Koordinatnyy nomer stantsii"                     
# [2] "God"                                             
# [3] "Mesyats"                                         
# [4] "Den"                                             
# [5] "Temperatura na glubine   sm uvelichennaya v  raz"
# [6] "Temperatura na glubine  sm uvelichennaya v  raz" 
# [7] "Temperatura na glubine  sm uvelichennaya v  raz" 
# [8] "Temperatura na glubine  sm uvelichennaya v  raz"

#  [4] "Tip marshruta"                                         
#  [5] "Den"                                                   
#  [6] "Stepen pokrytiya okrestnosti stantsii snegom"          
#  [7] "Stepen pokrytiya marshruta snegom"                     
#  [8] "Stepen pokrytiya marshruta ledyanoy korkoy"            
#  [9] "Srednyaya vysota snezhnogo pokrova na marshrute (sm)"  
# [10] "Naibolshaya vysota snezhnogo pokrova na marshrute (sm)"
# [11] "Naimenshaya vysota snezhnogo pokrova na marshrute (sm)"
# [12] "Srednyaya plotnost snega"                              
# [13] "Srednyaya tolshchina ledyanoy korki"                   
# [14] "Tolshchina sloya snega nasyshchennogo vodoy (mm)"      
# [15] "Tolshchina sloya chistoy vody (mm)"                    
# [16] "Zapas vody v snege (mm)"                               
# [17] "Zapas vody obshchiy (mm)"                              
# [18] "Kharakter zaleganiya snezhnogo pokrova"                
# [19] "Kharakter snezhnogo pokrova" 


        en = c("st_id",      "year",  "month", "day",
            
            # tas-pre
            "t_quality_flag",
            "t_min", "t_avr", "t_max",
            "pre",

            # snow
            "h_snow",
            "share_snow",
            "h_snow_details",
            "h_snow_quality",
            "snow_tas_details"

            ),
     
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
