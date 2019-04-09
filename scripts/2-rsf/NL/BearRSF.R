### Bear RSF ----
# Authors: Michel Laforge, Alec Robitaille

# Copyright: ./LICENSE.md

#TODO: it's spring and not summer right?
### Packages ----
libs <- c('data.table', 'sp', 'adehabitatHR', 'raster',
          'ewc')
# libs <- c('adehabitatHR', 'rgeos', 'spatstat', 'raster',
#           'ewc', 
#           'data.table', 'piecewiseSEM', 'rgdal')
lapply(libs, require, character.only = TRUE)


### Set variables ----
source('scripts/0-variables/variables.R')


### Input data ----
bear <- readRDS('output/1-data-prep/bear.Rds')

# Covariates
lsCovers <- gsub(".tif|100m|prep", "", 
               dir('output/1-data-prep/covariates/NL', '.tif$'))

lsPaths <- dir('output/1-data-prep/covariates/NL', 
             '.tif$', full.names = TRUE)
names(lsPaths) <- lsCovers


### Processing ----
points <- SpatialPoints(bear[, .(EASTING, NORTHING)],
                        proj4string = CRS(utmNL))

mcps <- mcp(points, percent = 100)


# Create Regular Grid
regPts <- generate_grid(mcps, 90, crs = utmNL)
setnames(regPts, c('EASTING', 'NORTHING'))

# Combine observed and regular grid points
regPts[, observed := 0]
bear[, observed := 1]

# Add fake season to regular grid 'grid'
regPts[, season := 'grid']

samplePts <- rbindlist(list(regPts, bear), 
                       use.names = TRUE, fill = TRUE)


### Sampling ----
# Drop columns leaving only needed
cols <- c('id','EASTING', 'NORTHING', 'season', 'observed')
samplePts <- samplePts[, ..cols]

# Add row ID
samplePts[, rowID := .I]

# Sample rasters
lsRasters <- lapply(lsPaths, raster)

samplePts[, (lsCovers) := lapply(
  lsRasters,
  FUN = function(r) {
    extract(r, matrix(c(EASTING, NORTHING), ncol = 2))
  }
)]


### RSF ----
# Winter RSF
winterPts <- samplePts[season == "winter" | season == 'grid']
winterPts[season == 'grid', season := "winter"]

winterRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                 family = 'binomial',data = winterPts)

summary(winterRSF)
vif(winterRSF)
rsquared(winterRSF)

# Pull out the coefficients, dropping the intercept
winterCoefs <- coef(winterRSF)[-1]


# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterRSF)[1]

if (all(names(winterCoefs) == names(lsRasters))) {
  winterRaster <-
    exp(intercept + Reduce('+', Map('*', winterCoefs, lsRasters)))
} else {
  stop('names dont match, check coef and rasters')
}

# Spring RSF
springwolf <- samplePts[season == "spring" | season == 'grid']
springwolf[observed == 0, season := "spring"]

springwolfRSF <- glm(reformulate(lsCovers, response = 'observed'), 
                     family = 'binomial',data = springwolf)

summary(springwolfRSF)
vif(springwolfRSF)
rsquared(springwolfRSF)

# Pull out the coefficients, dropping the intercept
springCoefs <- coef(springwolfRSF)[-1]

# Create the raster matching the first raster layer with the first fixed effect
intercept <- coef(winterwolfRSF)[1]

if (all(names(winterCoefs) == names(lsRasters))) {
  springRaster <-
    exp(intercept + Reduce('+', Map('*', springCoefs, lsRasters)))
} else {
  stop('names dont match, check coef and rasters')
}

bears <- readRDS('output/1-data-prep/bear.Rds')
nrow(bears)
plot(bears$EASTING, bears$NORTHING)

head(bears)


plot(nlBounds)
points(bears$EASTING, bears$NORTHING)


BearPoints <-
  SpatialPoints(data.frame(bears$EASTING, bears$NORTHING), proj4string = CRS(utm))

BearAvail <- mcp(BearPoints, percent = 100)
shapefile(BearAvail, "output/bearsMCP.shp")

### Buffer by mean step length
BearAvailBuf <- gBuffer(BearAvail, width = mean(bears$stepLength))

Bearclipped <- gIntersection(nlBounds, BearAvailBuf)
#clipped2<-as.owin.SpatialPolygons(clipped)

# Create Regular Grid

regPts <- generate_grid(Bearclipped, 90, crs = utm)



BearAntUsed <- extract(Ant, BearPoints)
BearBroUsed <- extract(Bro, BearPoints)
BearConUsed <- extract(Con, BearPoints)
BearLicUsed <- extract(Lic, BearPoints)
BearMixUsed <- extract(Mix, BearPoints)
BearRocUsed <- extract(Roc, BearPoints)
BearScrUsed <- extract(Scr, BearPoints)
BearWatUsed <- extract(Wat, BearPoints)
BearWetUsed <- extract(Wet, BearPoints)
BearRugUsed <- extract(Rug, BearPoints)
BearWaDUsed <- extract(WaD, BearPoints)
BearLinUsed <- extract(Lin, BearPoints)

BearRandPoints <-
  SpatialPoints(data.frame(regPts[, 1], regPts[, 2]), proj4string = CRS(utm))


BearAntAvail <- extract(Ant, BearRandPoints)
BearBroAvail <- extract(Bro, BearRandPoints)
BearConAvail <- extract(Con, BearRandPoints)
BearLicAvail <- extract(Lic, BearRandPoints)
BearMixAvail <- extract(Mix, BearRandPoints)
BearRocAvail <- extract(Roc, BearRandPoints)
BearScrAvail <- extract(Scr, BearRandPoints)
BearWatAvail <- extract(Wat, BearRandPoints)
BearWetAvail <- extract(Wet, BearRandPoints)
BearRugAvail <- extract(Rug, BearRandPoints)
BearWaDAvail <- extract(WaD, BearRandPoints)
BearLinAvail <- extract(Lin, BearRandPoints)

CNames <-
  c(
    "use",
    "Ant",
    "Bro",
    "Con",
    "Lic",
    "Mix",
    "Roc",
    "Scr",
    "Wat",
    "Wet",
    "Rug",
    "WaD",
    "Lin",
    "x",
    "y",
    "Season"
  )

BearUsedData <-
  data.frame(
    1,
    BearAntUsed,
    BearBroUsed,
    BearConUsed,
    BearLicUsed,
    BearMixUsed,
    BearRocUsed,
    BearScrUsed,
    BearWatUsed,
    BearWetUsed,
    BearRugUsed,
    BearWaDUsed,
    BearLinUsed,
    bears$EASTING,
    bears$NORTHING,
    bears$season
  )

colnames(BearUsedData) <- CNames

BearAvailData <-
  data.frame(
    0,
    BearAntAvail,
    BearBroAvail,
    BearConAvail,
    BearLicAvail,
    BearMixAvail,
    BearRocAvail,
    BearScrAvail,
    BearWatAvail,
    BearWetAvail,
    BearRugAvail,
    BearWaDAvail,
    BearLinAvail,
    regPts[, 1],
    regPts[, 2],
    "Avail"
  )

colnames(BearAvailData) <- CNames

bearsRSF <- rbind(BearUsedData, BearAvailData)


write.csv(bearsRSF, "output/bearsRSFdata.csv")

bearsRSF <- read.csv("output/bearsRSFdata.csv")

bearsRSF$X <- NULL
head(bearsRSF)




## Remove all points with 50% NA data
bearsRSF$rs <- rowSums(bearsRSF[2:10])
coyRSF2 <- subset(bearsRSF, rs > 0.5)
summary(coyRSF2$rs)

coyRSF2[, 2] <- coyRSF2[, 2] / coyRSF2$rs
coyRSF2[, 3] <- coyRSF2[, 3] / coyRSF2$rs
coyRSF2[, 4] <- coyRSF2[, 4] / coyRSF2$rs
coyRSF2[, 5] <- coyRSF2[, 5] / coyRSF2$rs
coyRSF2[, 6] <- coyRSF2[, 6] / coyRSF2$rs
coyRSF2[, 7] <- coyRSF2[, 7] / coyRSF2$rs
coyRSF2[, 8] <- coyRSF2[, 8] / coyRSF2$rs
coyRSF2[, 9] <- coyRSF2[, 9] / coyRSF2$rs
coyRSF2[, 10] <- coyRSF2[, 10] / coyRSF2$rs

## Check to make sure it worked
coyRSF2$rs2 <- rowSums(coyRSF2[2:10])
head(coyRSF2)


### Wetland is the reference
BearSummer <- subset(coyRSF2, Season == "spring" | Season == "Avail")


RSFbearsSum <-
  glm(use ~ Ant + Bro + Con + Lic + Mix + Roc + Scr + WaD + Lin + Rug,
      data = BearSummer,
      family = 'binomial')


summary(RSFbearsSum)
rsquared(RSFbearsSum)

SumRSF <-
  exp(
    coef(RSFbearsSum)[1] + AntR * coef(RSFbearsSum)[2] + BroR * coef(RSFbearsSum)[3] +
      ConR * coef(RSFbearsSum)[4] + LicR * coef(RSFbearsSum)[5] +
      MixR * coef(RSFbearsSum)[6] + RocR * coef(RSFbearsSum)[7] +
      ScrR * coef(RSFbearsSum)[8] + WaDR * coef(RSFbearsSum)[9] +
      LinR * coef(RSFbearsSum)[10] + RugR * coef(RSFbearsSum)[11]
  )

cellStats(SumRSF, max)

SumRSFsc <-
  (SumRSF - (cellStats(SumRSF, min))) / (cellStats(SumRSF, max) - cellStats(SumRSF, min))

plot(SumRSFsc)

writeRaster(SumRSFsc, "output/PredRSFNL/bearSummer.tif", overwrite = T)

saveRDS(RSFbearsSum, "output/PredRSFNL/BearsSummerRSF.RDS")
