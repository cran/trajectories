### R code from vignette source 'tracks.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tracks.Rnw:27-31
###################################################
# Wrap R commands. Use together with Sweave option "keep.source=false".
# options(width = 60)
# Load library "rgl" to allow for using "rgl" graphics with Sweave.
library("rgl")


###################################################
### code chunk number 2: tracks.Rnw:46-49
###################################################
library("spacetime")
library("trajectories")
example("Track")


###################################################
### code chunk number 3: tracks.Rnw:64-65
###################################################
Track(stidf)


###################################################
### code chunk number 4: tracks.Rnw:76-77
###################################################
Tracks(list(A1 = A1, A2 = A2))


###################################################
### code chunk number 5: tracks.Rnw:88-89
###################################################
TracksCollection(list(A = A, B = B))


###################################################
### code chunk number 6: tracks.Rnw:112-114
###################################################
dim(Tr)
summary(Tr)


###################################################
### code chunk number 7: tracks.Rnw:119-123
###################################################
proj4string(B)
coordinates(A1)
coordnames(Tr)
bbox(A)


###################################################
### code chunk number 8: tracks.Rnw:128-129
###################################################
bbox(Tr)


###################################################
### code chunk number 9: tracks.Rnw:134-135
###################################################
stbox(Tr)


###################################################
### code chunk number 10: tracks.Rnw:146-148
###################################################
class(Tr[1:2])
dim(Tr[1:2])


###################################################
### code chunk number 11: tracks.Rnw:152-154
###################################################
class(Tr[2])
dim(Tr[2])


###################################################
### code chunk number 12: tracks.Rnw:158-160
###################################################
class(Tr[2][1])
dim(Tr[2][1])


###################################################
### code chunk number 13: tracks.Rnw:165-167
###################################################
class(Tr[list(1:2, 2)])
dim(Tr[list(1:2, 2)])


###################################################
### code chunk number 14: tracks.Rnw:171-172 (eval = FALSE)
###################################################
## Tr[Muenster]


###################################################
### code chunk number 15: tracks.Rnw:175-177
###################################################
class(Tr[["co2"]])
length(Tr[["co2"]])


###################################################
### code chunk number 16: tracks.Rnw:180-182
###################################################
class(Tr$co2)
length(Tr$co2)


###################################################
### code chunk number 17: tracks.Rnw:185-186
###################################################
Tr[["distance"]] = Tr[["distance"]] * 1000


###################################################
### code chunk number 18: tracks.Rnw:189-190
###################################################
Tr$distance = Tr$distance * 1000


###################################################
### code chunk number 19: tracks.Rnw:229-230
###################################################
plot(Tr, col = 2, axes = TRUE)


###################################################
### code chunk number 20: tracks.Rnw:238-239
###################################################
stplot(Tr, attr = "co2", arrows = TRUE, lwd = 3, by = "IDs")


###################################################
### code chunk number 21: tracks.Rnw:251-265 (eval = FALSE)
###################################################
## # Generalise a track into 5 minute intervals. Use max() as the
## # aggregation function.
## generalize(B, max, timeInterval = "2 min")
## # Generalise a track into 200 distance units (usually metres).
## generalize(A2, distance = 200)
## # Generalise a track into n segments with each segment consisting of
## # two points.
## generalize(Tr, min, n = 2)
## # Simplify the given geometries using the Douglas–Peucker algorithm
## # with tolerance value 2.
## generalize(A, timeInterval = "3 min", tol = 2)
## # Keep the middle point of each segment rather than generalising to
## # objects of class "SpatialLines".
## generalize(A1, n = 3, toPoints = TRUE)


###################################################
### code chunk number 22: tracks.Rnw:274-276 (eval = FALSE)
###################################################
## demo("Track")
## demo("stcube")


###################################################
### code chunk number 23: tracks.Rnw:281-314
###################################################
# Import enviroCar track.
importEnviroCar = function(trackID, url = "https://envirocar.org/api/stable/tracks/") {
	require(RCurl)
	require(rgdal)
	require(rjson)
	url = getURL(paste(url, trackID, sep = ""), 
		.opts = list(ssl.verifypeer = FALSE)) # .opts needed for Windows
	# Read data into spatial object.
	spdf = readOGR(dsn = url, layer = "OGRGeoJSON", verbose = FALSE)
	# Convert time from factor to POSIXct.
	time = as.POSIXct(spdf$time, format = "%Y-%m-%dT%H:%M:%SZ")
	# Convert phenomena from JSON to data frame.
	phenomena = lapply(as.character(spdf$phenomenons), fromJSON)
	values = lapply(phenomena, function(x) as.data.frame(lapply(x, function(y) y$value)))
	# Get a list of all phenomena for which values exist.
	names = vector()
	for(i in values)
		names = union(names, names(i))
	# Make sure that each data frame has the same number of columns.
	values = lapply(values, function(x) {
		xNames = names(x)
		# Get the symmetric difference.
		diff = setdiff(union(names, xNames), intersect(names, xNames))
		if(length(diff) > 0)
			x[diff] = NA
		x
	})
	# Bind values together.
	data = do.call(rbind, values)
	sp = SpatialPoints(coords = coordinates(spdf), proj4string = CRS("+proj=longlat +ellps=WGS84"))
	stidf = STIDF(sp = sp, time = time, data = data)
	Track(track = stidf)
}


###################################################
### code chunk number 24: tracks.Rnw:318-320 (eval = FALSE)
###################################################
## A3 = importEnviroCar("528cf1a3e4b0a727145df093")
## stcube(A3, showMap = TRUE, col = "red")


###################################################
### code chunk number 25: tracks.Rnw:323-325 (eval = FALSE)
###################################################
## data(A3)
## stcube(A3, showMap = TRUE, col = "red")


