read_meteo <-
function(dir_name, zip_name, st_id){

    # preliminary input checks -----------------------------

    zip_check_details <- check_zip(dir_name = dir_name, zip_name = zip_name)


    # TODO Actually, that is perfectly possible to process many st ids
    # The only question is to handle different availability cases
    if ( length(st_id) > 1 ) {
        warning(
            paste0("More than one station is requested namely ",
                   paste(st_id, collapse = ", "), "\n\r",
                   "First one only will be processed.")
        )
        st_id <- st_id[1]
    }
         
    # assess files names -------------------------------------------------------

    key_fl_name <- fls_in_zip_df$Name[grep(fls_in_zip_df$Name, 
        pattern = "^fld")]

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

    # st_id parsing fails in case it's type set as integer and a whitespace presents
    # i_st_id <- which(meta_names %in% "st_id")
    data_col_types <- rep("double", times = length(meta_names))
    # data_col_types[i_st_id] <- "integer"

    # touch data files ---------------------------------------------------------

    im_a_separate_station_file <- grepl(x = fls_in_zip_df$Name, 
        pattern = "^\\d{5,6}\\.txt$")

    # in case all stations records are put into a single file
    # the standard data file name is the same as the archive name 
    if ( any(im_a_separate_station_file) ) {

        st_fls_names <- fls_in_zip_df[im_a_separate_station_file, "Name"]
        st_ids <- gsub(pattern = ".txt", replacement = "", x = st_fls_names)        

    } else {

        im_a_bulk_station_file <- grepl(x = fls_in_zip_df$Name, 
            pattern = "^wr\\d{5,6}.*txt$")

        st_fls_names <- ifelse(is.na(zip_id), 
            fls_in_zip_df[im_a_bulk_station_file, "Name"], 
            gsub(x = zip_id, pattern = ".zip", replacement = ".txt"))

        if ( length(unique(st_fls_names)) > 1 ) {
            warning(paste0("There are more than one bulk-data files: ", 
                    paste(st_fls_names, collapse = ", "), 
                    ". The first one only will be processed."))
            st_fls_names <- st_fls_names[1]
        }

    }      

    if ( any(im_a_separate_station_file) ) {

        is_a_requested_df <- (st_fls_names %in% paste0(st_id, ".txt"))

        # TODO Would it make sense to account for a mix of separate and bulk files?
        if ( !(any(is_a_requested_df)) ){
            stop(paste0("No data for a requested station ", 
                st_id, " is available"))
        }

        data_file <- st_fls_names[is_a_requested_df]         

    } else {

        data_file <- fls_in_zip_df$Name[im_a_bulk_station_file]

    }

    test_sep <- read.csv.raw(unz(data_path, data_file), 
        header = FALSE, sep = NA, 
        skip = 0L, fileEncoding = "",
        nrows = 1L, nsep = NA, strict = TRUE,
        nrowsClasses = 25L, quote = "'\"")

    # there are only two possible separator options: ";" or ""
    if ( grepl(test_sep, pattern = ";") ) {

        data_df <- read.csv.raw(unz(data_path, data_file), 
            header = FALSE, sep = ";", 
            skip = 0L, fileEncoding="",
            nrows = -1L, nsep = NA, strict = TRUE,
            nrowsClasses = 25L, quote="'\"")

    } else {

        # there is a whitespace separator by default influencing of the column width
        add_widths <- c(seq(from = 1, to = 1, along.with = data_field_widths[-1]), 
            0)

        data_df <- dstrfw(
            x = readAsRaw(unz(data_path, data_file)), 
            col_types = data_col_types, 
            # there is a whitespace separator
            widths = data_field_widths + add_widths, 
            nsep = NA, strict=TRUE, skip=0L, nrows=-1L) 

    }

    colnames(data_df) <- meta_names

    return(data_df)


}
