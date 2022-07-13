# beverage parameter ####
setwd(this.path::this.dir())
source.file <- print("Rsource_Max_Lemon_mtx_V01.R")
source( paste0(getwd(), "/", source.file) )

# spectra ####
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd("./Produktionsdaten")

dt$para$files <- dir(pattern = ".csv$")
dt$para$txt <- txt.file(dt$para$files)

dt$raw <- lapply(dt$para$files, \(x) freadr4dt(x, sep = ";", dec = ","))
names(dt$raw) <- paste0(dt$para$txt$type, "_", dt$para$txt$loc.line)

dt$para$trs <- lapply(dt$raw, transfer_csv.num.col)

dt$raw  <- mapply(function(x , y) y[ , c(1 : (min(x$numcol) - 1), x$numcol[ x$wl %in% dt$para$wl[[1]] ]), with = F]
                  , x = dt$para$trs
                  , y = dt$raw)

dt$para$trs <- lapply(dt$raw, transfer_csv.num.col)

# validate drk ####
par( mfrow = c(1, length( grep( "drk", names(dt$para$trs)) )))
for(i in grep( "drk", names(dt$para$trs)))
    matplot(dt$para$trs[[ i ]]$wl
            , t(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F])
            , lty = 1, type = "l", xlab = lambda, ylab = "Counts", main = dt$para$txt$loc.line[ i ])

for(i in grep( "drk", names(dt$para$trs))){

dt$val$drk[[ i ]] <- apply(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F], 1, spectra.validation.drk)
print( unique(dt$val$drk[[ i ]]) )
dt$raw$spc <- dt$raw$spc[ spectra.validation.range(valid.vector = dt$val$drk[[ i ]]
                                                   , drkref.datetime = dt$raw[[ i ]]$datetime
                                                   , spc.datetime = dt$raw$spc$datetime
                                                   , pattern = "invalid") , ]
dt$val$drk[[ i ]] <- apply(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F], 1, spectra.validation.drk)
dt$raw$spc <- dt$raw$spc[ spectra.validation.range(valid.vector = dt$val$drk[[ i ]]
                                                   , drkref.datetime = dt$raw[[ i ]]$datetime
                                                   , spc.datetime = dt$raw$spc$datetime
                                                   , pattern = "empty") , ]
}

# validate ref ####
par( mfrow = c(1, length( grep( "ref", names(dt$para$trs)) )))
for(i in grep( "ref", names(dt$para$trs)))
  matplot(dt$para$trs[[ i ]]$wl
          , t(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F])
          , lty = 1, type = "l", xlab = lambda, ylab = "Counts", main = dt$para$txt$loc.line[ i ])

# validate spc ####
par( mfrow = c(1, length( grep( "spc", names(dt$para$trs)) )))
for(i in grep( "spc", names(dt$para$trs))){ boxplot( dt$raw[[ i ]]$X220) }
grep( "spc", names(dt$para$trs)) 
i = 3; dt$raw[[ i ]] <-  dt$raw[[ i ]][  dt$raw[[ i ]]$X220 > .535 , ]
i = 6; dt$raw[[ i ]] <-  dt$raw[[ i ]][  dt$raw[[ i ]]$X220 < .94 , ]

for(i in grep( "spc", names(dt$para$trs))){ boxplot( dt$raw[[ i ]]$X320) }
i = 3; dt$raw[[ i ]] <-  dt$raw[[ i ]][  dt$raw[[ i ]]$X320 > -.2 , ]

for(i in grep( "spc", names(dt$para$trs))){ boxplot( dt$raw[[ i ]]$X420) }
  
matplot(dt$para$trs$spc$wl
        , t(dt$raw$spc[ , dt$para$trs$spc$numcol, with = F])
        , lty = 1, type = "l")

plot(dt$raw$spc$X279)
dt$raw$spc <- dt$raw$spc[ dt$raw$spc$X279 > .395 , ]
dt$raw$spc <- dt$raw$spc[ dt$raw$spc$X279 < .42 , ]

par( mfrow = c(1, length( grep( "spc", names(dt$para$trs)) )))
for(i in grep( "spc", names(dt$para$trs)))
  matplot(dt$para$trs[[ i ]]$wl
          , t(dt$raw[[ i ]][ , dt$para$trs[[ i ]]$numcol, with = F])
          , lty = 1, type = "l", xlab = lambda, ylab = "AU", main = dt$para$txt$loc.line[ i ])

# export clean spc csv ####
for(i in grep( "spc", names(dt$para$trs)))
fwrite(dt$raw[[ i ]]
       , gsub("_spc.csv", "_spc_validated.csv", dt$para$files[ i ])
       , sep = ";", dec = ",")



