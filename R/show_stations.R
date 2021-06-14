show_stations <- 
function(st_ids, main_txt = "", sub_txt = "", 
	x_range = c(0, 180), y_range = c(35, 80)){

# obninsk_path <- system.file("extdata", "Obninsk_St_info.csv", package = "climaru")
# test <- read.csv(obninsk_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)

obninsk_path2 <- system.file("extdata", "st_info.csv", package = "climaru")
st_info_df <- read.csv(obninsk_path2, sep = ";", header = TRUE, stringsAsFactors = FALSE)

selected_st_info <- st_info_df[st_info_df$St_Code %in% st_ids, ]

# draw maps ------------------------------------------------

dev.new()
maps::map(xlim = x_range, ylim = y_range, main = "test")
points(x = selected_st_info$Long_dec, y = selected_st_info$Lat_dec, pch = 21,
		bg = "cornflowerblue", col = "darkblue", cex = 1)
title(main = main_txt, sub = sub_txt)	
box()
return(NULL)	

}