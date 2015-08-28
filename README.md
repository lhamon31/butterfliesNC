# raster-script
library(raster)

## Reading in raster data
# long-term minimum temperature data downloaded from WorldClim.org

# Read in single raster layer from january
tmin_jan = raster('./tmin/tmin1.bil') # note scale off by factor of 10
plot(tmin_jan)

# Read in stack of layers from all 12 months
files = paste('./tmin/tmin',1:12,'.bil', sep='')
tmin = stack(files)
plot(tmin)

# Find minimum across all months
lowT = calc(tmin, min)
plot(lowT, col=mycol)

# Convert to actual temp
lowT = lowT/10

## Manipulating raster data: cropping, re-sampling, re-projecting

# Crop raster to extent of AK
myext = extent(c(-181, -126, 48,72))
lowT_ak = crop(lowT, myext)
plot(lowT_ak, col=mycol)

# Re-project raster to crs of vector data
lowT_ak = projectRaster(lowT_ak, crs=CRS(ak_proj))
plot(lowT_ak, col=mycol)

# Calculate mean across ecoregions and add to akreg1 dataframe
eco_temp = extract(lowT_ak, akreg1, fun=mean, na.rm=T)

akreg1$lowT = eco_temp
spplot(akreg1, 'lowT', col.regions=colorRampPalette(mycol)(7), cuts=6)
