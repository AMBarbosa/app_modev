library(terra)
library(geodata)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

countries <- geodata::world(path = "data")
ib <- subset(countries, countries$NAME_0 %in% c("Portugal", "Spain"))
plot(ib)
ib <- disagg(ib)
ib <- ib[order(expanse(ib), decreasing = TRUE)[1:2], ]
plot(ib)

wclim <- geodata::worldclim_global(var = "bio", res = 10, path = "data")  # less disk space than wclim_country, where 'res' is not an option
plot(wclim)
wclim <- crop(wclim, ib, snap = "out", mask = TRUE)
plot(wclim[[1]])
res(wclim)
wclim <- aggregate(wclim, 10, na.rm = TRUE)
plot(wclim[[1]])
plot(ib, add = TRUE)

names(wclim)
names(wclim) <- gsub("wc2.1_10m_", "", names(wclim))
temperature <- as.character(round(values(wclim[["bio_1"]]), 0))
temperature <- sub("NaN", "", temperature)
precipitation <- as.character(round(values(wclim[["bio_12"]]), 0))
precipitation <- sub("NaN", "", precipitation)


par(mfrow = c(1, 2))

plot(wclim[[1]], axes = FALSE, mar = c(1, 1, 2, 4), main = "Temperature")
plot(ib, border = "brown", lwd = 2, add = TRUE)
text(wclim[[1]], temperature, cex = 0.7, halo = TRUE)

plot(wclim[[2]], axes = FALSE, mar = c(1, 1, 2, 4), main = "Precipitation")
plot(ib, border = "brown", lwd = 2, add = TRUE)
text(wclim[[1]], precipitation, cex = 0.7, halo = TRUE)

occ_inds <- c(1:7, 9:10, 17:18)
occs <- crds(wclim)[occ_inds, ]
points(occs, pch = 20, col = "black", cex = 2)

par(mfrow = c(1, 1))
plot(wclim[[1]] * 0, col = "lightgrey", axes = FALSE, legend = FALSE, main = "Species presences")
plot(ib, border = "brown", lwd = 2, add = TRUE)
points(occs, pch = 20, col = "black", cex = 9)
# points(occs, pch = 15, col = "black", cex = 7)

par(mar = c(5, 4, 2, 1), mfrow = c(1, 1))
plot(precipitation, temperature, pch = 20, col = "turquoise", cex = 2)
points(precipitation[occ_inds], temperature[occ_inds], pch = 20, col = "black", cex = 1)


temperature <- as.numeric(temperature)
precipitation <- as.numeric(precipitation)
presence <- rep(0, length(temperature))
presence[occ_inds] <- 1

mod <- glm(presence ~ temperature + precipitation, family = binomial)
pred <- predict(wclim, mod, type = "response")
plot(pred, main = "Predicted probability", axes = FALSE)
plot(ib, border = "brown", lwd = 2, add = TRUE)

hist(pred)

# pred <- modEvA::quantReclass(pred)
set.seed(8)
values(pred) <- jitter(values(pred), amount = 0.15)
pred[pred < 0] <- 0
pred[pred > 1] <- 1
hist(pred)
plot(pred, main = "Predicted probability", axes = FALSE)
plot(ib, border = "brown", lwd = 2, add = TRUE)

pred_vals <- as.character(round(values(pred), 3))
pred_vals <- gsub("NaN", "", pred_vals)

text(pred, pred_vals, cex = 0.9, halo = TRUE)


d <- data.frame(pres = as.numeric(presence), pred = as.numeric(pred_vals))
d
d[order(d$pred, decreasing = TRUE), ]
head(d)
d[5:15, ]

# plot(d$pres, d$pred, pch = 20, col = adjustcolor("black", 0.5))
plot(d$pres, d$pred, pch = 1, xlab = "observed", ylab = "predicted", axes = FALSE, lwd = 1.5)
box()
axis(1, at = c(0, 1))
axis(2)

linmod <- lm(pred ~ pres, data = d)
abline(linmod, col = "royalblue", lwd = 2)

linmod$coefficients[2]

cor(d$pres, d$pred, use = "pairwise")


terra::writeRaster(pred, "data/pred.tif", overwrite = TRUE)
terra::writeVector(ib, "data/ib.gpkg", overwrite = TRUE)
write.csv(occs, "data/occ.csv", row.names = FALSE)
