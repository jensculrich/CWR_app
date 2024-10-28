rData <- readRDS(paste0("./data/apples/fus_pred_mod_ssp245_30.Rdata"))
plot(rData)
str(rData)

r <- raster::raster(rData)

r[r[] < 1 ] = NA 

plot(r)


a <- "Malus coronaria"

if(a == "Malus coronaria"){
  stringy1 = "cor_pred"
} else {
  stringy1 = "fus_pred"
}

b <- "reduced (ssp245)"

if(b == "reduced (ssp245)"){
  stringy2 = "ssp245"
} else {
  stringy2 = "ssp585"
}

c <- "2030"

if(c == "2030"){
  stringy3 = "30"
} else if(c == "2050") {
  stringy3 = "50"
} else{
  stringy3 = "70"
}

d <- "high"

if(d == "high"){
  stringy4 = "high"
} else if(c == "moderate") {
  stringy4 = "mod"
} else{
  stringy4 = "low"
}

stringy_cat <- paste0(stringy1, "_", stringy4, "_", stringy2, "_", stringy3)
r <- terra::rast(readRDS(paste0("./data/apples/", stringy_cat, ".Rdata")))

plot(r)

"ssp245: middle of the road projection, high climate adaptation, low climate mitigation"
"ssp585: low regard for enviromental sustainability, increased fossil fuel reliance, this is the current tracking projection
"