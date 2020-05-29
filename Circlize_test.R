library(circlize)

set.seed(999)
n = 1000
df = data.frame(factors = sample(letters[1:8], n, replace = TRUE),
                x = rnorm(n), y = runif(n))


circos.par("track.height" = 0.1)
circos.initialize(factors = df$factors, x = df$x)

circos.track(factors = df$factors, y = df$y)
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(df$factors, df$x, df$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)
