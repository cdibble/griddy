# ===================================================================================================
# Setup
# ===================================================================================================
	rm(list=ls())
	require(dplyr)
	require(ggplot2)
	require(reshape2)
	require(scales)
	require(ggrepel) # geom_text_repel function
		# Map data
			# load(file  = "/Users/Connor/Documents/Graduate School/Dibble_Research/FLAH_2016/Analysis/flah_map_data.R") # local BB area
			load(file = '/Users/connor/Documents/Graduate School/Dibble_Research/Krill/Data/Wseaboard_df')
		# utilities
		theme_tsg <- theme(axis.text.x = element_text(angle= 90),
						panel.grid.major = element_line(color = "grey55", size = 0.5), panel.grid.minor = element_line(color = "grey90"))
		scale_x_datetime_tsg <- function(...){
			structure(list(
				scale_x_datetime(expand = c(0,0), breaks = date_breaks(width = "1 day"), minor_breaks = date_breaks(width = "1 hour"), date_labels = "%m-%d", ...)
				))
		}
		se <- function(x, ...) sqrt(var(x, na.rm = TRUE)/length(x[!is.na(x)]))
		dname <- function(df, old, new){
			new.names <- names(df)
			new.names[which(new.names == old)] <- new
			return(new.names)
		}
		'%nin%' <- Negate('%in%')
# ===================================================================================================
# Setup
# ===================================================================================================
# function to search the indexes and find the right Dataset.ID to query.
i.search <- function(index.i, terms = c("Wind"), my.pick = NULL, ignore.Case = TRUE, Cache = TRUE){
	# Check to see if the index was already cached in the global environment; if not, ping server for index of datasets
	if(exists("index.i", env = .GlobalEnv) == FALSE){
		index.i <- read.csv("https://coastwatch.pfeg.noaa.gov/erddap/info/index.csv")
		if(Cache == TRUE){assign("index.i", index.i, envir = .GlobalEnv)} # cache to global envi if you want; saves download time
	}else{index.i <- get("index.i", envir = .GlobalEnv)} # if it's cached, pull it into this environment;
	if(length(terms) > 1){ # setup the search term regular expression; collapse user search terms and use forward-look ('?=') to make it agnostic to the ordering
		grep.terms <- paste0("^(?=.*\\b", paste0(terms, collapse = "\\b)(?=.*\\b"), "\\b)")
		# grep("^(?=.*\\bWind\\b)(?=.*\\bRAMA\\b)", index.i[["Title"]], value = TRUE,)
	}else{
		grep.terms <- paste0("^(?=.*\\b", terms, "\\b)")  # "/^(?=.*Tim)(?=.*stupid).+/"
	}
	match.i <- grep(grep.terms, index.i[["Title"]], value = TRUE, perl = TRUE, ignore.case = TRUE) # search data products using user search terms
	if(length(match.i) > 1 & is.null(my.pick)){ 	# if multiple options, prompt user for choice; return the dataset ID
		cat("Pick one: \n")
		print(match.i)
		my.pick <- as.numeric(readline(prompt = "Enter a number: "))
		your.Dataset.ID <- index[which(index$Title == match.i[my.pick]), "Dataset.ID"]
	}else{
		if(is.null(my.pick)){
			your.Dataset.ID <- index[which(index$Title == match.i) , "Dataset.ID"]
		}else{
			your.Dataset.ID <- index[which(index$Title == match.i[my.pick]), "Dataset.ID"]
		}
	}
	return(your.Dataset.ID)
}
# search for the meta data to figure out which variables are available
var.search <- function(DID){
	# create url string from user input; query server for meta data
	string <- paste0('https://coastwatch.pfeg.noaa.gov/erddap/info/', DID,'/index.csv')
	meta.data <- read.csv((string))	# meta.data <- readr::read_csv((string))	# meta.data <- download.file(string)
	# Prompt user with options for variables if there is more than one; pull the one they chose out and find its query parameters and return all that info;
	if(nrow(meta.data[which(meta.data$Attribute.Name == "standard_name"),]) > 1){
		cat("Pick one: \n")
		print(meta.data[which(meta.data$Attribute.Name == "standard_name"),] %>% mutate(Option_Number = 1:nrow(.)))
		my.pick <- as.numeric(readline(prompt = "Enter a number: "))
		your.standard_name <- meta.data[which(meta.data$Attribute.Name ==  "standard_name")[my.pick], "Variable.Name"]
		your.st.params <- meta.data[which(meta.data$Variable.Name ==  your.standard_name & meta.data$Row.Type == "variable"), "Value"]
	}else{
		your.standard_name <- meta.data[which(meta.data$Attribute.Name == "standard_name") , "Variable.Name"]
		your.st.params <- meta.data[which(meta.data$Variable.Name ==  your.standard_name & meta.data$Row.Type == "variable"), "Value"]
	}
	return(list(DID = DID, standard_name = your.standard_name, params = unlist(strsplit(as.character(your.st.params), split = ', '))))
}
# Function to setup query and send it.
# erddap.q <- function(DID, st.name, start.time = as.POSXIct("2016-01-01 00:00:00"), stop.time =  as.POSXIct("2016-02-01 00:00:00"), time.span = 1, s.limit = 36, n.limit = 38, lat.span = 0.25, w.limit = -124, e.limit = -122, lon.span = 0.25){
# erddap.q <- function(DID, st.name, params, start.time, stop.time, time.span = 1, s.limit, n.limit, lat.span = 0.25, w.limit, e.limit, lon.span = 0.25, ...){
# 	# future extnsion: allow multiple st.names; for each, append the query string and then slap all of them on the final string.
# 	stopifnot(class(start.time) == "POSIXct" & class(stop.time) == "POSIXct")
# 	start.t <- paste0("(", format(start.time, "%Y-%m-%d"), "T", format(start.time, "%H:%M:%S"), "Z)")
# 	stop.t <- paste0("(", format(stop.time, "%Y-%m-%d"), "T", format(stop.time, "%H:%M:%S"), "Z)")
# 	string <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/", DID, ".csvp?", st.name, "[",
# 		start.t, ":", as.character(time.span), ":", stop.t, "][(", n.limit, "):", lat.span, ":(", s.limit, ")][(",
# 			w.limit, "):", lon.span, ":(", e.limit, ")]")
# 	string <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/", DID, ".csvp?", st.name)
# 	# [(2015-03-21T00:00:00Z):1:(2015-03-21T00:00:00Z)] [(89.5):1:(-89.5)] [(-179.5):1:(179.5)]
# 	my.data <- read.csv(string)	
# }
# erddap.q <- function(DID, st.name, params, start.time, stop.time, time.span = 1, s.limit, n.limit, lat.span = 0.5, w.limit, e.limit, lon.span = 0.5, ...){
erddap.q <- function(DID, st.name, params, start.time = as.POSIXct("2016-01-01 00:00:00"), stop.time =  as.POSIXct("2016-02-01 00:00:00"), time.span = 1,
	s.limit = 36, n.limit = 38, lat.span = 0.5, w.limit = -124, e.limit = -122, lon.span = 0.5, ...){
	# future extnsion: allow multiple st.names; for each, append the query string and then slap all of them on the final string.
	vars <- list(...) # extra user vars; 
	stopifnot("POSIXct" %in% class(start.time) & "POSIXct" %in% class(stop.time)) # make sure time is formatted correctly
	# assemble url strings from user input
	time <- paste0("[(", format(start.time, "%Y-%m-%d"), "T", format(start.time, "%H:%M:%S"), "Z):", time.span, ":(", format(stop.time, "%Y-%m-%d"), "T", format(stop.time, "%H:%M:%S"), "Z)]")
	latitude <- paste0("[(", s.limit, "):", lat.span, ":(", n.limit, ")]")
	longitude <- paste0("[(", 360 - abs(w.limit), "):", lat.span, ":(", 360 - abs(e.limit), ")]")
		# other variables: loop through missing variables; check if they were given by the user (as ... now in vars); if not, prompt user; if so, pull out of vars and convert to string.
	for(i in 1:length(params[which(params %nin% c("time", "latitude", "longitude"))])){
		if(params[which(params %nin% c("time", "latitude", "longitude"))][i] %nin% names(vars)){
			assign(params[which(params %nin% c("time", "latitude", "longitude"))][i],
				paste0("[(", readline(prompt = paste0("This parameter is required: ",
				params[which(params %nin% c("time", "latitude", "longitude"))][i])), ")]"), pos = environment() )
		}else{assign(params[which(params %nin% c("time", "latitude", "longitude"))][i],
			paste0("[(", vars[[params[which(params %nin% c("time", "latitude", "longitude"))][i]]] , ")]"))
		}
	}
	# print(lapply(params, get, pos = environment())) # debugger
	# assemble full string; download data; wrangle the names and classes, convert longitude back to hemisphere system
	string <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/", DID, ".csvp?", st.name,
		paste0(lapply(params, get, pos = environment()), collapse = "") ) # [(2015-03-21T00:00:00Z):1:(2015-03-21T00:00:00Z)] [(89.5):1:(-89.5)] [(-179.5):1:(179.5)]
	my.data <- read.csv(string)
	names(my.data)[which(names(my.data) == grep('latitude', names(my.data), value = TRUE))] <- "latitude"
	names(my.data)[which(names(my.data) == grep('longitude', names(my.data), value = TRUE))] <- "longitude"
	my.data$longitude <- ifelse(my.data$longitude >= 180, -(360 - my.data$longitude), my.data$longitude )
	names(my.data)[which(names(my.data) == grep('time', names(my.data), value = TRUE))] <- "time"
	my.data$time <- as.POSIXct(as.character(my.data$time), format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
	my.data <- left_join(my.data,
			my.data %>% dplyr::filter(time == unique(time)[1]) %>% mutate(id = 1:nrow(.)) %>% dplyr::select(latitude, longitude, id),
			by = c("latitude", "longitude") )
	return(my.data)
}
xx <- erddap.q(DID = p$DID, st.name = p$standard_name, params = p$params, height_above_ground = 10)

# Function to match up a dataset by nearest points
# The data outputs from erddap.q are going to need some wrangling- not sure how consistent they are yet though.
grid.data <- xx %>% dplyr::filter(time == unique(time)[1])

grid.match <- function(mdf, grid.data){
	require(rgdal); require(maptools); require(rgeos); require(sp); require(geosphere); require(raster);
	# mdf contains krill or other data; move to spatial df
	sp.mdf <- mdf[!is.na(mdf$latitude),]
	coordinates(sp.mdf) <- ~longitude + latitude
	crs(sp.mdf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	# gridded data contains environmental data from erddap to spatial df
	sp.grid.data <- grid.data
	coordinates(sp.grid.data) <- ~longitude + latitude
	crs(sp.grid.data) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	# compute pairwise distances; minimize distances for each row of mdf; assign nearest station index to mdf
	# d <- gDistance(sp.mdf, sp.grid.data, byid=T) # distances in degrees
	d <- distm(sp.mdf, sp.grid.data)/1000 # use geosphere::distm() for distance in km.
	minds <- apply(d, 1, min)	
	minds.index <- apply(d, 1, function(x){which(x == min(x))})
	sp.mdf$nearest.grid <- minds.index # index station; now you can left_join the mdf and the grid.data using this index (for space; don't forget about time).
	sp.mdf$nearest.grid.distance <- minds # kilometers

	return(as.data.frame(sp.mdf))
}

sp.mdf <- grid.match(krill, grid.data)

ggplot(grid.data) +
	geom_text(aes(x = longitude, y = latitude, label = id )) +
	geom_text(data = as.data.frame(sp.mdf), aes(x = longitude, y = latitude, label = nearest.grid), color = 'blue') +
	coord_equal() + theme_bw()





