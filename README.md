# raster-script
setwd("~/1990")

library(raster)

## Reading in raster data
# long-term mean temperature data downloaded from WorldClim.org

# Read in single raster layer from january
tmean_jan = raster('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_199001_bil.bil')
plot(tmean_jan)

# Read in stack of layers from all 12 months
files=paste('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_19900',3:6,'_bil.bil',sep='')
tmean = stack(files)
plot(tmean)
