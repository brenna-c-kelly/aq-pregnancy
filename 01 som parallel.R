
library(Rsomoclu)

sub <- poll_cl[c(1:100000), ]

input_data <- as.matrix(poll_cl[, c("mean_pm", "max_pm",
                                    "mean_no2", "max_no2",
                                    "mean_o3", "max_o3")])


nSomX <- 90
nSomY <- 100
nEpoch <- 10
radius0 <- 0
radiusN <- 0
radiusCooling <- "linear"
scale0 <- 0
scaleN <- 0.01
scaleCooling <- "linear"
kernelType <- 0
mapType <- "planar"
gridType <- "hexagonal"
compactSupport <- FALSE
codebook <- NULL
neighborhood <- "gaussian"
stdCoeff <- 0.5

system.time(
  res <- Rsomoclu.train(input_data, nEpoch, nSomX, nSomY,
                        radius0, radiusN,
                        radiusCooling, scale0, scaleN,
                        scaleCooling,
                        kernelType, mapType, gridType, compactSupport,
                        neighborhood, stdCoeff, codebook)
)


summary(res)

res$codebook
res$globalBmus
res$uMatrix

res_k <- Rsomoclu.kohonen(input_data, res, n.hood = "circular", toroidal = FALSE)

#plot(res_k, type = "changes")
plot(res_k, type = "counts", shape = "straight", border = NA, heatkeywidth = 0.75)
plot(res_k, type = "codes", shape = "straight")
plot(res_k, type = "dist", shape = "straight")
summary(res_k)

str(res_k$codes[[1]][, 2])
par(mfrow = c(2, 3))
plot(res_k, type = "property", property = res_k$codes[[1]][, 1],#[,"max_pm"],
     main = "mean_pm", shape = "straight", palette.name = turbo, heatkey = TRUE, heatkeywidth = 0.75,
     zlim = c(0, 100), border = NA)
plot(res_k, type = "property", property = res_k$codes[[1]][, 2],#[,"max_pm"],
     main = "max_pm", shape = "straight", palette.name = turbo, heatkey = FALSE,#, heatkeywidth = 0.5,
     zlim = c(0, 100), border = NA)
plot(res_k, type = "property", property = res_k$codes[[1]][, 3],#[,"max_pm"],
     main = "mean_no2", shape = "straight", palette.name = turbo, heatkey = FALSE,#, heatkeywidth = 0.5,
     zlim = c(0, 100), border = NA)
plot(res_k, type = "property", property = res_k$codes[[1]][, 4],#[,"max_pm"],
     main = "max_no2", shape = "straight", palette.name = turbo, heatkey = FALSE,#, heatkeywidth = 0.5,
     zlim = c(0, 100), border = NA)
plot(res_k, type = "property", property = res_k$codes[[1]][, 5],#[,"max_pm"],
     main = "mean_o3", shape = "straight", palette.name = turbo, heatkey = FALSE,#, heatkeywidth = 0.5,
     zlim = c(0, 100), border = NA)
plot(res_k, type = "property", property = res_k$codes[[1]][, 6],#[,"max_pm"],
     main = "max_o3", shape = "straight", palette.name = turbo, heatkey = FALSE,#, heatkeywidth = 0.5,
     zlim = c(0, 100), border = NA)
dev.off()
