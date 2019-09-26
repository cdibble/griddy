# CMIP5 data can be obtained from: https://esgf-node.llnl.gov/projects/esgf-llnl/
	# Some command line skills are need to DL data
	# List of variables available as output: http://www.clivar.org/sites/default/files/documents/137_WGOMD_ModelOutput.pdf
	# List of modeling groups: https://pcmdi.llnl.gov/mips/cmip5/docs/CMIP5_modeling_groups.pdf?id=37
# ======================================================================================================================================================
# Setup
# ======================================================================================================================================================
	rm(list=ls())
	library(ncdf4)
	require(reshape2)
	require(dplyr)
	require(ggplot2)
	require(scales)
	require(viridis)
# ======================================================================================================================================================
# NetCDF Wrangling Functions
# ======================================================================================================================================================
	nc_build <- function(file.name, var.name, lat.name = 'lat', long.name = 'lon'){
		ncin <- ncdf4::nc_open(file.name)
		lon <- ncdf4::ncvar_get(ncin, long.name)
		lon[which(lon > 180)] <- lon[which(lon > 180)] - 360
		lat <- ncdf4::ncvar_get(ncin, lat.name)
		time <- ncdf4::ncvar_get(ncin, "time")
		timeunits <- ncdf4::ncatt_get(ncin, "time", "units")	
		timeunit <- substr(timeunits$value, regexpr('[a-z]+', timeunits$value)[1], regexpr('[a-z]+', timeunits$value)[1] + attr(regexpr('[a-z]+', timeunits$value), 'match.length') - 1)
		start.time <- as.POSIXct(substr(timeunits$value, regexpr('\\s[0-9]+-[0-9]+-[0-9]+', timeunits$value)[1] + 1, regexpr('\\s[0-9]+-[0-9]+-[0-9]+', timeunits$value)[1] + attr(regexpr('\\s[0-9]+-[0-9]+-[0-9]+', timeunits$value), 'match.length')),
						tz = "GMT")
		if(timeunit == 'days'){time <- as.POSIXct(time * 24 * 60 * 60, origin = start.time)}else{cat("\nDon't know how to deal with time units other than days. Make me smarter. \n")}
		# print(c(dim(lon), dim(lat)))
		# print(range(time))
		# print(timeunits)
		dat.array <- ncdf4::ncvar_get(ncin, var.name)
		dlname <- ncdf4::ncatt_get(ncin, var.name, "long_name")
		dunits <- ncdf4::ncatt_get(ncin, var.name, "units")
		fillvalue <- ncdf4::ncatt_get(ncin, var.name, "_FillValue")

		dat.array[which(dat.array == fillvalue$value)] <- NA # convert from native fill value to NA in R.
		dimnames(dat.array) <- list(lon, lat, time) # provide lat, lon, and time values as attributes of the array (dimname)

		meta.dat <- data.frame(
			title = ncdf4::ncatt_get(ncin, 0, "title")$value,
			institution = ncdf4::ncatt_get(ncin, 0, "institution")$value,
			datasource = ncdf4::ncatt_get(ncin, 0, "source")$value,
			references = ncdf4::ncatt_get(ncin, 0, "references")$value,
			history = ncdf4::ncatt_get(ncin, 0, "history")$value,
			conventions = ncdf4::ncatt_get(ncin, 0, "Conventions")$value,
			dlname = dlname$value,
			dunits = dunits$value,
			timeunits = timeunits$value
			)
		
		ncdf4::nc_close(ncin)

		return(list(dat.array = dat.array, meta.dat = meta.dat))
	}

	nc_cat <- function(directory.path, var.name, ...){
		var.files <- list.files(path = directory.path, pattern = paste0("^" , var.name, "_"))
		for(i in 1:length(var.files)){
			nc.i <- nc_build(file.name = paste0(directory.path, var.files[i]), var.name = var.name, ...)
			if(i == 1){
				meta.data <- nc.i[['meta.dat']]
				data.array <- nc.i[['dat.array']]
			}else{
				meta.data <- rbind(meta.data, nc.i[['meta.dat']])
				data.temp <- nc.i[['dat.array']]
				if(dim(data.temp)[1] == dim(data.array)[1] & dim(data.temp)[2] == dim(data.array)[2]){
					data.array <- abind::abind(data.array, data.temp)
				}
			}
		}
		return(list(dat.array = data.array, meta.dat = meta.data))
	}

	nc_plot <- function(nc.df, time.range, funn = 'mean'){
		require(viridis); require(ggplot2);require(reshape2)
		times <- as.POSIXct(as.numeric(dimnames(nc.df)[3][[1]]), origin = '1970-01-01')	
		if(length(time.range) == 2){ # average over time range
			begin <- min(which(times >= time.range[1]), na.rm = TRUE)
			ending <- max(which(times <= time.range[2]), na.rm = TRUE)
			cat('\n'); print(times[begin]); cat('\n'); print(times[ending])
			nc.plot <- nc.df[,, begin:ending ] / length(begin:ending)
			# nc.plot <- apply(nc.df[,, begin:ending ], c(1,2), mean, na.rm = TRUE)
			nc.plot <- apply(nc.df[,, begin:ending ], c(1,2), funn, na.rm = TRUE)
		}
		if(length(time.range) == 1){
			nc.plot <- nc.df[,, min(which(times >= time.range), na.rm = TRUE) ]
		}
		nc.plot <- as.data.frame(nc.plot)
		nc.plot$lon <- as.numeric(rownames(nc.plot))

		nc.melt <- melt(nc.plot, id.vars = 'lon')
		names(nc.melt) <- c("lon", "lat", "var")
		nc.melt$lat <- round(as.numeric(as.character(nc.melt$lat)))

		plot.out <- ggplot(nc.melt) + geom_raster(aes(x = lon, y = lat, fill = var)) +
			theme_bw() + scale_fill_viridis()
		return(list(nc.df.melted = nc.melt, nc.plot = plot.out))
	}

	nc_box <- function(nc.df.melted, bound.box, funn = 'mean'){
		nc.df.m <- dplyr::filter(nc.df.melted, lat <= bound.box$max.lat, lat >= bound.box$min.lat, lon <= bound.box$max.lon, lon >= bound.box$min.lon)
		# val.out <- mean(nc.df.m$var, na.rm = TRUE)
		val.out <- do.call(funn, list(nc.df.m$var, na.rm = TRUE))
		return(val.out)
	}
	# dplyr::filter(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range)[['nc.df.melted']], lat <= bound.box$max.lat, lat >= bound.box$min.lat, lon <= bound.box$max.lon, lon >= bound.box$min.lon)
	# dplyr::filter(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range)[['nc.df.melted']], lat >= bound.box$min.lat, lat <= bound.box$max.lat, lon >= bound.box$min.lon,  lon <= bound.box$max.lon)
	nc_diff <- function(nc.df, t.range1, t.range2){
		d1 <- nc_plot(nc.df, time.range = t.range1)[['nc.df.melted']] # present
		d2 <- nc_plot(nc.df, time.range = t.range2)[['nc.df.melted']] # present
		d1 <- left_join(d1, d2, by = c("lat", "lon"), suffix = c(".present", ".future"))
		d1$diff <- d1$var.future - d1$var.present
		return(d1)
	}
# ======================================================================================================================================================
# Dispersal in a Changing Ocean
# ======================================================================================================================================================
	'sos' # sea surface salinity
	'tos' # sea surface temperature
	'ph' # surface pH
	'o2' # dissolved oxygen at surface
	directory.path.ts = '/Users/Connor/Documents/Graduate School/Dibble_Research/Dispersal_ClimateChange_Paper/CMIP/wget-20190807152516-NCDFfiles/' # temp/salin
	# aa <- nc_cat(directory.path.ts, var.name = 'sos') # salinity
	# aa <- nc_cat(directory.path.ts, var.name = 'tos') # temperature
	directory.path.bg = '/Users/Connor/Documents/Graduate School/Dibble_Research/Dispersal_ClimateChange_Paper/CMIP/wget-20190807202515-NCDFfiles/' # biogeochem
	# 	var.files <- list.files(path = directory.path.bg, pattern = 'ph_')
	# 	var.files <- list.files(path = directory.path.bg, pattern = '^o2_')
	aa <- nc_cat(directory.path.bg, var.name = 'o2') # ph
	# aa <- nc_cat(directory.path.bg, var.name = 'o2') # o2

	time.range <- c("2090-01-01", "2100-12-31")
	# time.range <- c("2100-01-01")
	# time.range <- c("2050-01-01")
	# dimnames(aa[['dat.array']])[2] <- round(dimnames(aa[['dat.array']])[2] )
	time.range.1 <- c("2018-06-01", "2019-06-01")
	time.range.2 <- c("2095-06-01", "2096-06-01")
	aa <- nc_cat(directory.path.bg, var.name = 'ph') # ph
	range(as.POSIXct(as.numeric(dimnames(aa[['dat.array']])[[3]]), origin = "1970-01-01"))

	diff.df <- nc_diff(nc.df = aa[['dat.array']], t.range1 = time.range.1, t.range2 = time.range.2)
		ggplot(diff.df) + geom_raster(aes(x = lon, y = lat, fill = diff)) + scale_fill_gradient2()

	p1 <- nc_plot(aa[['dat.array']], time.range = time.range)
	p1[['nc.plot']]

	# Temperature - is in kelvin.
	p1.m <- p1[["nc.df.melted"]]
	p1.m$var <- p1.m$var - 271
	ggplot(p1.m) + geom_raster(aes(x = lon, y = lat, fill = var)) + scale_fill_viridis() + theme_bw()

	# Tropical Pacific Boundary Box.test
	bound.box = data.frame(min.lat = 0, max.lat = 20, min.lon = -150, max.lon = -100)
	nc_box(p1[['nc.df.melted']], bound.box)

	# Tropical IndoPacific: lizard island
	bound.box = data.frame(min.lat = -15, max.lat = -13, min.lon = 145, max.lon = 147)
	# time.range <- c("2100-01-01", "2100-12-31")
	# time.range <- c("2096-01-01", "2096-12-31") # FUTURE
	time.range <- c("2016-01-01", "2016-12-31") # PRESENT
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'tos')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'tos')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	# time.range <- c("2096-01-01", "2096-12-31")
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'ph')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'ph')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'o2')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'o2')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)

	# Temperate Pacific - San Diego
	bound.box = data.frame(min.lat = 31.5, max.lat = 33.5, min.lon = -119.2, max.lon = -117.2)
	# time.range <- c("2100-01-01", "2100-12-31")
	# time.range <- c("2096-01-01", "2096-12-31")
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'tos')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'tos')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	# time.range <- c("2096-01-01", "2096-12-31")
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'ph')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'ph')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'o2')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'o2')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)

	# Temperate Pacific - Monterey
	# bound.box = data.frame(min.lat = 35, max.lat = 37, min.lon = -123.5, max.lon = -121.5)
	bound.box = data.frame(min.lat = 35, max.lat = 37, min.lon = -124, max.lon = -122)
	# time.range <- c("2100-01-01", "2100-12-31")
	# time.range <- c("2096-01-01", "2096-12-31")
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'sos')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'tos')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.ts, var.name = 'tos')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	# time.range <- c("2096-01-01", "2096-12-31")
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'ph')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'ph')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'o2')[['dat.array']], time.range = time.range)[['nc.df.melted']], bound.box)
	nc_box(nc_plot(nc_cat(directory.path.bg, var.name = 'o2')[['dat.array']], time.range = time.range, funn = 'sd')[['nc.df.melted']], bound.box)
