# Script to define HMSC models for fish species distribution and abundance
source("./0-Load libraries.R")

localDir <- "." # Set your local directory here
ModelDir <- file.path(localDir, "Models")   
DataDir <- file.path(localDir, "Inputs_HMSC")

load(file = file.path(DataDir, "allData.RData")) #S,X,Y,P
load(file = file.path(DataDir, "Traits.RData")) # Traits

#source("Keep_species_more_than_1percent_occ.R")

# Excluding rare taxa based on previous script
load(file = file.path(DataDir, "selected_sp.RData"))
Y <- Y[, colnames(Y) %in% selected_sp]

# Remove hauls that do not contain the selected species in all tables (S, X, Y)
S_rows <- rowSums(Y)
hauls_to_be_removed <- names(S_rows[S_rows == 0])
Y <- Y[!(rownames(Y) %in% hauls_to_be_removed), ]
S <- S[!(S$id %in% hauls_to_be_removed), ]
X <- X[!(rownames(X) %in% hauls_to_be_removed), ]

# Transform variables as factors
S$Year <- as.factor(S$Year)
S$id <- as.factor(S$id)

# Define regression model for environmental and anthropogenic covariates
XFormula <- ~ poly(Depth, degree = 2, raw = TRUE) + poly(SBT, degree = 2, raw= TRUE) + poly(SST, degree = 2, raw= TRUE) + poly(SSS, degree = 2, raw= TRUE) + poly(Chla, degree = 2, raw= TRUE) + Fishing + Gravity
print(XFormula)

# Define regression model for traits
# Remove Caudal shape trait
Tr <- Tr[, -which(names(Tr) %in% c('Caudal_shape'))]
Tr_tmp <- Tr
# Select traits for species that will be kept
Tr_tmp <- Tr_tmp[rownames(Tr_tmp) %in% colnames(Y),]
TrFormula <- as.formula(paste("~",paste(c("Trophic_level", "Min_size", "Max_size", "Growth_coefficient", "Trophic_guild", "Body_shape", "Water_column_position"), collapse="+")))
print(TrFormula)

# Set up study design with spatial random effect
sum(duplicated(S[, c(2,3)])) # 3 and 2 are latitudes and longitudes columns
    # coordinates of every sample id cannot be repeated /unless all sampling units are all constantly repeated by a time (year)
    # add a small value in the duplicated coordinates to make them different
row.names(S) <- seq(1,nrow(S),1)

dupl <- which(duplicated(S[,c('Long','Lat')]))
for (i in 1:length(dupl)) {
  ds <- S[which(S$Long==S$Long[dupl[i]] & S$Lat==S$Lat[dupl[i]]), ]
  ds <- ds[-1,]
  lon <- ds$Long[1]
  lat <- ds$Lat[1]
  for (n in 1:nrow(ds)){
    lon <- lon +0.000001
    lat <- lat +0.000001
    S[dupl[i],"Long"] <- lon
    S[dupl[i],"Lat"] <- lat
  }
}

# Define model study design 
studyDesign <- data.frame(sample = as.factor(S$id), 
                         year = as.factor(S$Year),
                         grid.cell=as.factor(S$grid.id),
                         grid.lon=as.factor(round(S$grid.lon,5)),
                         grid.lat=as.factor(round(S$grid.lat,5)))

# Define random effects
# Temporal
rL.year <- HmscRandomLevel(units = levels(studyDesign$year))# add year as random effect
rL.year <- setPriors(rL.year, nfMin = 2, nfMax = 5) # set prior for number of latent factors to be estimated for this random effect

# Spatial
xy <- data.frame(S[match(unique(S$grid.id), S$grid.id), c("grid.lon", "grid.lat")]) # Coordinates of unique grid cells
rownames(xy) <- unique(S$grid.id) # Cells of those coordinates

rL.location <- HmscRandomLevel(sData = xy, longlat = TRUE, sMethod = "NNGP", nNeighbours = 10)
rL.location <- setPriors(rL.location, nfMin = 2, nfMax = 5)

# 2 tables: pres/abs and log abundance
# Define presence absence model
Ypa <- Y
Ypa[is.na(Ypa)] <- 0
Ypa[Ypa > 0] <- 1
mpa <- Hmsc(Y = Ypa,
         XData = X,
         XFormula = XFormula,
         TrFormula = TrFormula,
         TrData = Tr_tmp,
         phyloTree = P,
         studyDesign = studyDesign,
         YScale = FALSE,
         XScale = TRUE,
         TrScale = TRUE,
         ranLevels = list(year = rL.year, grid.cell = rL.location),
         distr = "probit")

# Define abundance model
Yabu <- Y
Yabu[Yabu == 0] <- NA
Yabu <- log(Yabu)
mabu <- Hmsc(Y = Yabu,
           XData = X,
           XFormula = XFormula,
           TrFormula = TrFormula,
           TrData = Tr_tmp,
           phyloTree = P,
           studyDesign = studyDesign,
           YScale = TRUE,
           XScale = TRUE,
           TrScale = TRUE,
           ranLevels = list(year = rL.year, grid.cell = rL.location),
           distr = "normal")

# Save
models <- list(mpa, mabu)
modelnames <- c("probit", "normal")

# save(models, modelnames, file = file.path(ModelDir, "unfitted_models"))