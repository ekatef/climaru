read_daily <-
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

    # ------------------------------------------------------

    st_fl_name <- paste0(st_id, ".txt")

    # in case all the stations are put into a single file
    # it's name is the same as one of the whole zip file
    if ( bulk_file ) {
        st_fl_name <- gsub(pattern = ".zip", replacement = ".txt", x = zip_name)
    }

    if ( is.null(dir_name) ) {
        res <- read.csv(
            unz(zip_name, st_fl_name),
            # whitespace is possible, as well (and is even default) 
            sep = sep_smb,
            stringsAsFactors = FALSE, header = FALSE)
    # in case the zip_name does not contain a full path 
    } else {
        res <- read.csv(
            unz(file.path(dir_name, zip_name), st_fl_name),
            # whitespace is possible, as well (and is even default) 
            sep = sep_smb,
            stringsAsFactors = FALSE, header = FALSE)
    }

    colnames_ru <- restore_meta(dir_name, zip_name)
    clean_colnames <- translate_meta(colnames_ru)

    # it's easy to mix separator specification
    if ( length(clean_colnames) != ncol(res) ) {
        # fortunately, there are currently only "" and ";" options
        alt_sep_smb <- ifelse(sep_smb == ";", "", ";")
    }    

    if ( is.null(dir_name) ) {
        res <- read.csv(
            unz(zip_name, st_fl_name),
                sep = alt_sep_smb,
                stringsAsFactors = FALSE, header = FALSE)
    # in case the zip_name does not contain a full path 
    } else {
        res <- read.csv(
            unz(file.path(dir_name, zip_name), st_fl_name),
                sep = alt_sep_smb,
                stringsAsFactors = FALSE, header = FALSE)
    }

    colnames(res) <- clean_colnames

    # distinguish between a requested station and all available ones
    if ( bulk_file ) {
        res <- res[res$st_id %in% st_id, ]
    }

    # TODO not all columns may be present
    # are contained within a supplied file
    # colnames(res) <- c("st_id", "year", "month", "day", "qual_flag",
    #     "t_min", "t_avr", "t_max", "pre")
    
    return(res)
}
