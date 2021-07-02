check_zip <-
function(dir_name = NULL, zip_name){
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

    return(data_path)
}    
