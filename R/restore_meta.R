restore_meta <-
function(dir_name = NULL, zip_name){

	# "wr55408a3.zip"
	# fld55408.txt with columns names
	# statlist55408a3.txt with stations meta

	# gsub(".*<i>|</i>.*", "", x)
	# x <- "wr55408a3.zip"
	# gsub(".*wr|a.*", "", x)


    path_separator <- unlist(.Platform["file.sep"])
    if ( is.null(dir_name) ) {
    	names_in_zip <- unzip(zipfile = zip_name, list = TRUE)$Name
	# in case the zip_name does not contain a full path	
    } else {
    	names_in_zip <- unzip(zipfile = file.path(dir_name, zip_name), list = TRUE)$Name
    }

    # the folder path can contain anything => a pure zip archive id shoud be extracted
    if ( is.null(dir_name) ) {
        # courtesy of SO stackoverflow.com/a/31774103/8465924
        zip_id <- sapply(strsplit(zip_name, path_separator), tail, 1L)
    } else {
        zip_id <- zip_name
    }

    # some crypto-notaions are hardcoded
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
                             paste0("fld", dataset_id, "a",                  ".txt"))

    meta_fl_name <- unique(possible_meta_names[possible_meta_names %in% names_in_zip])

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
    meta_colnames <- trimws(meta_ids)

	# test <- read.csv(
	# 	unz(file.path(dir_name, zip_name), meta_fl_name),
 #        # whitespace is possible, as well (and is even default) 
	# 	sep = sep_smb,
	# 	stringsAsFactors = FALSE, header = FALSE)

	# test <- read.fwf(unz(file.path(dir_name, zip_name), meta_fl_name),
	# 				widths = c(4, 6, 5, 100),
	# 				fileEncoding = "windows-1251",
	# 				stringsAsFactors = FALSE) 

	# # works better
	# test_tsv <- read_tsv(unz(file.path(test_data_dir23, zip_data_fl23), meta_fl_name23))
	
	return(meta_colnames)
}
