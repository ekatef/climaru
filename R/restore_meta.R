# TODO Allow to skip variables names when calling a function

# What if the warnings will be optional?

restore_meta <-
function(dir_name = NULL, zip_name){

	# "wr55408a3.zip"
	# fld55408.txt with columns names
	# statlist55408a3.txt with stations meta

	# gsub(".*<i>|</i>.*", "", x)
	# x <- "wr55408a3.zip"
	# gsub(".*wr|a.*", "", x)

    # preliminary input checks -----------------------------

    zip_check_details <- check_zip(dir_name = dir_name, zip_name = zip_name)

    data_path <- zip_check_details[["data_path"]]
    fls_in_zip_df <- zip_check_details[["fls_in_zip_df"]]
    zip_id <- zip_check_details[["zip_id"]]

    # names assesment

    # the archive name may be a meaningful original or beautifully changed
    if ( !(is.na(zip_id)) ) {    # some crypto-notaions are hardcoded
        if ( length(grep("a", zip_id)) > 0 ) {
            dataset_id <- gsub(".*wr|a.*", "", zip_id)
            dataset_post_id <- gsub(".*a|.zip.*", "", zip_id)
        } else {
            dataset_id <- gsub(".*wr|.zip.*", "", zip_id)
            dataset_post_id <- ""
        }
    
        # it may be or may be not "a3" at the end of the metadata file
        possible_meta_names <- c(paste0("fld", dataset_id,                      ".txt"), 
                                 paste0("fld", dataset_id, "a", dataset_post_id, ".txt"), 
                                 paste0("fld", dataset_id,      dataset_post_id, ".txt"), 
                                 paste0("fld", dataset_id, "a",                  ".txt"),
                                 paste0("fld", dataset_id, "a", "0",             ".txt"))
    
        meta_fl_name <- unique(possible_meta_names[possible_meta_names %in% fls_in_zip_df$Name])
    } else {
        meta_fl_name <- fls_in_zip_df$Name[grep(x = fls_in_zip_df$Name, pattern = (".*fld|.txt.$"))]
    }

    if ( is.null(dir_name) ) {

    	meta_txt <- readr::read_file(unz(description = zip_name, 
    		filename = meta_fl_name))
	# in case the zip_name does not contain a full path	
    } else {

    	meta_txt <- readr::read_file(unz(description = file.path(dir_name, zip_name), 
    		filename = meta_fl_name))
    }
	
    meta_utf8 <- stringi::stri_encode(meta_txt, from = "windows-1251", to = "UTF-8", to_raw = FALSE)
    meta_for_humans <- stringi::stri_trans_general(meta_utf8,"Russian-Latin/BGN")
    
    meta_clean <- gsub("\u02B9", "", meta_for_humans)
    
    # TO DO digits between letters could be kept ("Temperatura na glubine  sm")
    meta_ids <- gsub(",", "", gsub("[[:digit:]]", "", unlist(strsplit(meta_clean, "\r\n"))))
    meta_colnames <- trimws(gsub("\\s+", " ", meta_ids))
	
	return(meta_colnames)
}
