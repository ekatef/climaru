read_meteo <-
function(dir_name, zip_name, st_id, sep_smb = ";", bulk_file = FALSE){

    # preliminary input checks -----------------------------

    path_separator <- unlist(.Platform["file.sep"])

    if ( any(grep(x = zip_name, pattern = paste0("\\", path_separator))) ) {
        warning(
            paste0("The `zip name` seems to be a folder path: ", zip_name, ".\n\r",
                "Setting dir_name to NULL.")
        )
        dir_name <- NULL
    }

    if ( !any(grep(x = zip_name, pattern = ".zip$")) ) {
        stop(
            paste0("The `zip name` seems to be not a zip name: ", zip_name)
        )
    }

    ## TODO Fix file-extists checking   
    # if ( !file.exists(file.path(dir_name, zip_name)) ) {
    #     stop(paste0("The requested data file ", st_fls_names,
    #         " seems to do not exist in the archive ", ," being processed."))
    # }

    # zip archive can be renamed ------
    # the folder path can contain anything => a pure zip archive id shoud be extracted
    if ( is.null(dir_name) ) {
        # courtesy of SO stackoverflow.com/a/31774103/8465924
        zip_id <- sapply(strsplit(zip_name, path_separator), tail, 1L)
    } else {
        zip_id <- zip_name
    }   
    # TODO stronger regexp condition is needed in case the zip name is modified only slightly
    if ( !any(grep(x = zip_id, pattern = ".*wr|.zip.$")) ){
        warning(
            paste0("The `zip name` seems to changed as compared with the database format: ", zip_name, ".\n\r",
                "The data file will be guessed.")
        )
        zip_id <- NA  
    }

    if ( length(st_id) > 1 ) {
        warning(
            paste0("More than one station is requested namely ",
                paste(collapse = st_id,  ", "), "\n\r",
                "First one only will be processed.")
        )
        st_id <- st_id[1]
    }
         
    # ------------------------------------------------------

    if ( is.null(dir_name) ) {
        data_path <- zip_name
    # in case the zip_name does not contain a full path 
    } else {
        data_path <- file.path(dir_name, zip_name)
    }

    fls_in_zip_df <- unzip(zipfile = data_path, list = TRUE)

    if ( nrow(fls_in_zip_df) == 0 ) {      
        stop(paste0("The assessed zip file" , zip_name, " seems to be empty"))
    } 

    # assess files names -------------------------------------------------------

    key_fl_name <- fls_in_zip_df$Name[grep(fls_in_zip_df$Name, pattern = "^fld")]

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

    i_st_id <- which(meta_names %in% "st_id")
    data_col_types <- rep("double", times = length(meta_names))
    data_col_types[i_st_id] <- "integer"

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

        data_df <- dstrfw(
            x = readAsRaw(unz(data_path, data_file)), 
            col_types = data_col_types, 
            widths = data_field_widths, 
            nsep = NA, strict=TRUE, skip=0L, nrows=-1L)

        colnames(data_df) <- meta_names 

        res <- data_df           

    } else {

        data_file <- fls_in_zip_df$Name[im_a_bulk_station_file]

        # return(data_file)

        data_df <- dstrfw(
            x = readAsRaw(unz(data_path, data_file)), 
            col_types = data_col_types, 
            widths = data_field_widths, 
            nsep = NA, strict=TRUE, skip=0L, nrows=-1L) 

        colnames(data_df) <- meta_names

        res <- data_df[data_df$st_id %in% st_id, ]

    }

    return(res)


}
