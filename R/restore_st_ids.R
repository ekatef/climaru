# TODO Fix an "unused connestion" issue appearing when bulk data files are processed
# TODO A database update allows for the zip archive names like "Tttr.zip" (with separate files for each station)

# TODO Does unarchiving affect the performance? 
# It could make sense in case of considerable improvements

# unzip + iotools() provide a perfect option to increase performance
# data.table::fread() is unstable for the purpose due to a possible columns loss

restore_st_ids <-
function(dir_name = NULL, zip_name){

    # preliminary input checks -----------------------------

    zip_check_details <- check_zip(dir_name = dir_name, zip_name = zip_name)


    im_a_separate_station_file <- grepl(x = fls_in_zip_df$Name, pattern = "^\\d{5,6}\\.txt$")

    # in case all stations records are put into a single file
    # the standard data file name is the same as the archive name 
    if ( any(im_a_separate_station_file) ) {

        st_fls_names <- fls_in_zip_df[im_a_separate_station_file, "Name"]
        st_ids <- as.numeric(gsub(pattern = ".txt", replacement = "", 
            x = st_fls_names) )       

    } else {

        im_a_bulk_station_file <- grepl(x = fls_in_zip_df$Name, pattern = "^wr\\d{5,6}.*txt$")

        st_fls_names <- ifelse(is.na(zip_id), 
            fls_in_zip_df[im_a_bulk_station_file, "Name"], 
            gsub(x = zip_id, pattern = ".zip", replacement = ".txt"))

        if ( length(unique(st_fls_names)) > 1 ) {
            warning(paste0("There are more than one bulk-data files: ", 
                    paste(st_fls_names, collapse = ", "), 
                    ". The first one only will be processed."))
            st_fls_names <- st_fls_names[1]
        }

        key_fl_name <- gsub(st_fls_names, pattern = "wr", replacement = "fld")

        # the name may include "a0" with no apparent reasons...
        # if ( !(file.exists(unz(file.path(data_path, key_fl_name)))) ) {
        if ( !(any(fls_in_zip_df$Name %in% key_fl_name )) ) {    
            key_fl_name <- gsub(key_fl_name, pattern = ".txt", 
                replacement = "a0.txt")
        }

        keys_df <- dstrfw(
            x = readAsRaw(unz(data_path, key_fl_name)), 
            col_types = c("integer", "integer", "character", NULL), 
            widths = c(2, 3, 8, NULL), 
            nsep = NA, strict=TRUE, skip=0L, nrows=-1L)
        
        # would a regexp work better?
        data_field_widths <- floor(as.numeric(gsub(keys_df[, 3], 
            pattern = ",", replacement = ".")))
        meta_names <- translate_meta(restore_meta(dir_name = dir_name, 
            zip_name = zip_name))
    
        if ( !any(meta_names %in% "st_id") ) {
            warning(paste0("No st_id column in meta data. Available columns: ", 
                meta_names))
        }

        # TODO How to skip all non-st_ids columns?
        i_st_id <- which(meta_names %in% "st_id")
        data_col_types <- rep("double", times = length(meta_names))
        data_col_types[i_st_id] <- "integer"

        data_df <- dstrfw(
            x = readAsRaw(unz(data_path, st_fls_names)), 
            col_types = data_col_types, 
            widths = data_field_widths, 
            nsep = NA, strict=TRUE, skip=0L, nrows=-1L)

        colnames(data_df) <- meta_names  
        st_ids <- as.numeric(unique(data_df[, "st_id"]))


    }

    return(st_ids)
    
}    