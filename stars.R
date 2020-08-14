## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----eval=FALSE---------------------------------------------------------------
## install.packages("remotes") # if not already installed
## remotes::install_github("r-spatial/stars")


## ----eval=FALSE---------------------------------------------------------------
## install.packages("starsdata", repos = "http://gis-bigdata.uni-muenster.de/pebesma", type = "source")


## -----------------------------------------------------------------------------
library(stars)


## -----------------------------------------------------------------------------
library(ggplot2)
data(Produc, package = "plm")
head(Produc)
ggplot(Produc) + geom_raster(aes(y = state, x = year, fill = pcap))


## -----------------------------------------------------------------------------
(s = st_as_stars(Produc, y_decreasing = FALSE))
s = st_apply(s, 1, function(x) x/mean(x)) %>%
  st_set_dimensions(names = c("year", "state"))
s
pr = as.data.frame(s)
head(pr)
ggplot(pr) + geom_raster(aes(y = state, x = year, fill = pcap))


## -----------------------------------------------------------------------------
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
nc.df = st_set_geometry(nc, NULL) # m is a regular, non-spatial data.frame
head(nc.df)


## -----------------------------------------------------------------------------
mat = as.matrix(nc.df[c("BIR74", "SID74", "NWBIR74", "BIR79", "SID79", "NWBIR79")])
dim(mat) = c(county = 100, var = 3, year = 2) # make it a 3-dimensional array
# set dimension values to the array:
dimnames(mat) = list(county = nc$NAME, var = c("BIR", "SID", "NWBIR"), year = c(1974, 1979))
# convert array into a stars object
(nc.st = st_as_stars(pop = mat))


## -----------------------------------------------------------------------------
(nc.geom <- st_set_dimensions(nc.st, 1, st_geometry(nc)))


## -----------------------------------------------------------------------------
plot(st_apply(nc.geom, c(1,2), sum), key.pos = 4) # sum population over year


## -----------------------------------------------------------------------------
# split out BIR, SID, NWBIR over attributes:
split(nc.geom, 2) 
# sum by category:
(nc.sum = sapply(split(nc.geom, 2), sum)) 
# denominator: mean incidence ratio, averaged over county & years:
(IR = nc.sum[2]/nc.sum[1]) 
# standardise each year/counte value by dividing over IR:
nc.SIR = st_apply(nc.geom, c(1,3), function(x) (x[2]/x[1])/IR)
plot(nc.SIR, breaks = c(0,.25,.5,.75,.9,1.1,1.5,2.5,3.5,5), 
  pal = rev(RColorBrewer::brewer.pal(9, "RdBu")))


## -----------------------------------------------------------------------------
#library(mapview)
#mapview(breweries)


## -----------------------------------------------------------------------------
(g = read_stars(system.file("external/test.grd", package = "raster")))
plot(g, col = viridis::viridis(11), breaks = "equal")


## -----------------------------------------------------------------------------
(g.sf = st_as_sf(g, na.rm = FALSE))
plot(g.sf, border = 'grey', pal = viridis::viridis(9), nbreaks = 10)


## -----------------------------------------------------------------------------
(r = read_stars(system.file("pictures/Rlogo.jpg", package = "rgdal"))) # the old one
plot(r, breaks = "equal")


## -----------------------------------------------------------------------------
(r.rgb = st_rgb(r))
r.rgb[[1]][1:3,1]


## -----------------------------------------------------------------------------
L7file = system.file("tif/L7_ETMs.tif", package = "stars")
(L7 = read_stars(L7file))


## -----------------------------------------------------------------------------
plot(L7)


## -----------------------------------------------------------------------------
plot(L7, join_zlim = FALSE)


## -----------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(L7, rgb = c(3,2,1), reset = FALSE, main = "RGB")    # rgb
plot(L7, rgb = c(4,3,2), main = "False color (NIR-R-G)") # false color


## -----------------------------------------------------------------------------
bb = st_bbox(c(xmin = -10, xmax = 20, ymin = 40, ymax = 60), crs = 4326)
(x = st_as_stars(bb, dx = 1, dy = 1))


## -----------------------------------------------------------------------------
sf_use_s2(FALSE)
library(rnaturalearth)
ne = ne_countries(returnclass = "sf", continent = 'europe')
ne$pop_dens = units::set_units(ne$pop_est / st_area(ne), 1/(km^2))
plot(ne["pop_dens"], reset = FALSE, extent = bb)
pop = st_rasterize(ne["pop_dens"], x)
plot(st_as_sf(pop, na.rm = FALSE), add = TRUE, border = 'grey')


## -----------------------------------------------------------------------------
(pop.3035 = st_transform(pop, 3035))
ne.3035 = st_transform(ne, 3035)
plot(pop.3035, border = 'grey', reset = FALSE)
plot(st_geometry(ne.3035), add = TRUE, border = 'yellow')


## -----------------------------------------------------------------------------
target_grid = st_as_stars(st_bbox(pop.3035)) # add dimensions/cell sizes etc here
(w = st_warp(pop, target_grid)) # or give only a target crs
plot(w, border = 'grey', reset = FALSE)
plot(st_geometry(ne.3035), add = TRUE, border = 'yellow')


## -----------------------------------------------------------------------------
(w <- system.file("nc/bcsd_obs_1999.nc", package = "stars") %>%
    read_stars("data/full_data_daily_2013.nc"))
plot(w)


## -----------------------------------------------------------------------------
x = c(
"avhrr-only-v2.19810901.nc",
"avhrr-only-v2.19810902.nc",
"avhrr-only-v2.19810903.nc",
"avhrr-only-v2.19810904.nc",
"avhrr-only-v2.19810905.nc",
"avhrr-only-v2.19810906.nc",
"avhrr-only-v2.19810907.nc",
"avhrr-only-v2.19810908.nc",
"avhrr-only-v2.19810909.nc"
)
# see the second vignette:
# install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")
file_list = system.file(paste0("netcdf/", x), package = "starsdata")
(y = read_stars(file_list, quiet = TRUE))


## -----------------------------------------------------------------------------
(z = adrop(y))


## -----------------------------------------------------------------------------
library(ggplot2)
library(viridis)
## Loading required package: viridisLite
library(ggthemes)
ggplot() +
  geom_stars(data = z[1], alpha = 0.8, downsample = c(10, 10, 1)) +
  facet_wrap("time") +
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


## -----------------------------------------------------------------------------
y[2] # second attribute
y[,1:10,1:12] # x/y region
y[,,,1] # zlev
y[,,,,3:5] # time


## ----eval=FALSE---------------------------------------------------------------
## x = st_as_stars() # global, 1 degree grid
## plot([ne])


## -----------------------------------------------------------------------------
granule = system.file("sentinel/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.zip", 
  package = "starsdata")
s2 = paste0("SENTINEL2_L1C:/vsizip/", granule, 
  "/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.SAFE/MTD_MSIL1C.xml:10m:EPSG_32632")
(p = read_stars(s2))


## -----------------------------------------------------------------------------
ndvi = function(x) (x[4]-x[1])/(x[4]+x[1])
(p.ndvi = st_apply(p, c("x", "y"), ndvi)) # doesn't actually do anything, yet
plot(p.ndvi)


## ----eval = TRUE--------------------------------------------------------------
dirs = list.dirs(".")
dirs = dirs[grepl("./LC08229064", dirs)] # those starting with ./LC08229064 : an L8 scene
f = list.files(dirs, pattern = "*band[1-7].tif", full.names = TRUE)

# to save memory, we continuously overwrite an object called "l":
l = lapply(f, function(x) read_stars(x, proxy=FALSE))
# the scenes don't line up 100%, so we need to warp them to the geometry of the first:
for (i in 2:length(l))
	l[[i]] = st_warp(l[[i]], l[[1]])
l = do.call(c, l) # every band is an attribute, with ugly names:
names(l)[1:3]

# get dates from scene name:
d = as.Date(substr(names(l), 18, 25), format = "%Y%m%d")
d[1:3]
# get band from scene name:
band = substr(names(l), 45, 49)
band[1:7]

# throw all bands in a dimension:
(l = merge(l))

di =  st_dimensions(l)
di[3] = NULL # remove the third attribute
# the order in which we add these dimensions matters; band cycles fastest, so will be added last:
di$date = stars:::create_dimension(values = unique(d))
di$band = stars:::create_dimension(values = unique(band))

(l = st_redimension(l, di))

# select a time slice and see all bands:
plot(adrop(l[,,,1,]))
# select a band and see a time series of that band
plot(l[,,,,1])
# reduce dimension "band" to an index (ndvi) and show its time series:
# ndvi = function(x) ifelse(x[5] < 1 || x[4] < 1, NA, (x[4]-x[3])/(x[4]+x[3]))
# plot(st_apply(l, 1:3, ndvi), breaks = "equal")

