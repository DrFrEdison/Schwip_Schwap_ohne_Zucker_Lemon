#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###                         ###
###  ######       ########  ###
###  ##    ##     ########  ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##    ##        ##     ###
###  ######          ##     ###
###                         ###
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###                         ###
### By Markus Kurtz         ###
### For Dausch Technologies ###
### 2022                    ###
###                         ###
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###
###
### Script is a template to analyse spectra from LG systems, data exported from Backup
###
###
### required functions and packages are loaded from source_read.R and source_pls.R

# load all required functions and packages ####
library(r4dt)

# set working directory ####
setwd(wd$csvtemp)

# list and read files ####
bev <- list()
bev$wd <- getwd()
bev$para$customer = "CCEP"
bev$para$beverage <- "Coca_Cola"

setwd(paste0(bev$wd.model <- paste0(wd$fe[[ grep(bev$para$customer, names(wd$fe)) ]]$Mastermodelle, bev$para$beverage)))
setwd( bev$wd <- wd$csvtemp )
bev$wd.git <- paste(wd$git, bev$para$customer, bev$para$beverage, sep = "/")

bev$para$date <- date()
bev$para$wl1 <- c(190)
bev$para$wl2 <- c(598)
bev$para$wl[[1]] <- seq(bev$para$wl1, bev$para$wl2, 1)

# Unit ####
bev$para$substance <- c("Coffein", "GS2")
bev$para$unit <- c( bquote("%"),  bquote("%"))
bev$para$ylab <- c(bquote("Coffein in %"), bquote("GS2 in %"))

# Rezept und SOLL-Werte ####
setwd( paste0( bev$wd.model, "/", "/Rezept") )
bev$rez <- read.xlsx(grep(".xlsx", dir( paste0( bev$wd.model, "/", "/Rezept")), value = T)[ length(grep(".xlsx", dir( paste0( bev$wd.model, "/", "/Rezept")), value = F))])
bev$rez[ grep("Messparameter", bev$rez[ , 3]): nrow(bev$rez) , ]
bev$para$SOLL <- c(100, 100)
bev$para$eingriff <- data.frame( Coffein = c(100 - 1.1/57*100, 100 + 1.1/57*100)
                                 , GS = c(100 - 2/100*100, 100 + 2/10*100))

bev$para$sperr <- data.frame( Coffein = c(100 - 1.7/57*100, 100 + 1.7/57*100)
                              , GS = c(100 - 4/100*100, 100 + 4/10*100))

# List files ####
setwd(bev$wd)
bev$para$files$ref <- grep("ref", dir(pattern = "ref.csv$")[grep(bev$para$beverage, dir(pattern = "ref.csv$"))], value = T) # Background spc
bev$para$files$drk <- grep("rk", dir(pattern = "rk.csv$")[grep(bev$para$beverage, dir(pattern = "rk.csv$"))], value = T) # Dark spc
bev$para$files$spc <- grep("spc", dir(pattern = "spc.csv$")[grep(bev$para$beverage, dir(pattern = "spc.csv$"))], value = T) # Production spc

# get file info ####
bev$para$txt <- lapply(bev$para$files, txt.file)

# read files ####
bev$raw$ref <- lapply(bev$para$files$ref, function(x) freadr4dt(x, dec = ",", sep = ";")) # Background spc
bev$raw$drk <- lapply(bev$para$files$drk, function(x) freadr4dt(x, dec = ",", sep = ";")) # Dark spc
bev$raw$spc <- lapply(bev$para$files$spc, function(x) freadr4dt(x, dec = ",", sep = ";")) # Production spc

# set names ####
names(bev$raw$ref) <- bev$para$txt$ref$loc.line
names(bev$raw$drk) <- bev$para$txt$drk$loc.line
names(bev$raw$spc) <- bev$para$txt$spc$loc.line

# read wavelength columns ####
bev$ppp <- lapply(bev$raw, function(x) lapply(x, transfer_csv.num.col))

# transfer_csv ####
bev$trs <- lapply(bev$raw, function(x) lapply(x, transfer_csv))

# prediction
# model_parameter(bev$para$customer, bev$para$beverage, LG = "3")
bev$pred <- lapply(bev$para$substance, function( para ) lapply(bev$trs$spc, function( spc )
  use_model_on_device(customer = bev$para$customer
                      , beverage = bev$para$beverage
                      , LG = "3"
                      , parameter = para
                      , csv_transfered = spc)
)
)
names(bev$pred) <- bev$para$substance

# Plot ####
setwd(bev$wd)
dir.create("plot", showWarnings = F)
setwd("./plot")

par(mfrow = c(1, length(bev$trs$spc)))
for(i in 1 : length(bev$pred) ){
  plot(bev$raw$spc[[ i ]]$GS2, ylim = c(90, 110)
       , main = paste("GS2 in", bev$para$txt$spc$loc.line[ i ])
       , axes = F, xlab = "KW", ylab = "GS2 in %")
  xaxisdate(bev$raw$spc[[ i ]]$datetime)
}

bev$para$toplot <- "GS2"



png(paste0(.date(),"_plot.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1, length(bev$trs$spc)))
for(i in 1 : length(bev$trs$spc )){
  plot(bev$pred[[ grep( bev$para$toplot, names(bev$pred)) ]][[ i ]], ylim = c(90, 110)
       , main = paste("GS2 in", bev$para$txt$spc$loc.line[ i ])
       , axes = F, xlab = "KW", ylab = "GS2 in %")
  xaxisdate(bev$raw$spc[[ i ]]$datetime)
}
dev.off()
