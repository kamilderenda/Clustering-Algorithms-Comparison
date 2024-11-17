library(sf)
library(dbscan)
library(dplyr)


# Wczytywanie danych
points <- st_read("C:\\Users\\kamil\\Desktop\\3rok\\5sem\\Analiza_Danych_Przestrzennych\\cw5\\zestaw2_XYTableToPoi_ProjectX.shp")
osiedle <- st_read("C:\\Users\\kamil\\Desktop\\3rok\\5sem\\Analiza_Danych_Przestrzennych\\cw5\\osiedla.shp")

# Wyodrębnienie współrzędnych z punktów
points_coords <- st_coordinates(points)
k_distances <- dbscan::kNNdist(points_coords, k = 7)
# Create a custom plot with adjusted axis limits
plot(
  sort(k_distances), 
  type = "l", 
  xlab = "Points (sorted)", 
  ylab = "kNN Distance", 
  xlim = c(0, 500),    # Adjust x-axis limits
  ylim = c(0, 10)       # Adjust y-axis limits
)
#par(mfrow=c(1,1))
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5)
title("Cracow")
axis(1)
axis(2)

# Wykonanie algorytmu DBSCAN 4:190
db <- dbscan(points_coords, eps=190, minPts=4)

# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(db$cluster == 0, 0, db$cluster+1))
title("Cracow")
axis(1)
axis(2)

# Wykonanie algorytmu DBSCAN 6:240
db <- dbscan(points_coords, eps=240, minPts=6)

# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(db$cluster == 0, 0, db$cluster+1))
title("Cracow")
axis(1)
axis(2)

# Wykonanie algorytmu DBSCAN 10:340
db <- dbscan(points_coords, eps=340, minPts=10)

# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(db$cluster == 0, 0, db$cluster+1))
title("Cracow")
axis(1)
axis(2)


# Wyodrębnienie współrzędnych z punktów
# Liczba sąsiadów (np. min_samples w HDBSCAN)
k <- 10

# Obliczenie odległości k-najbliższych sąsiadów
nn_distances <- get.knn(points_coords, k = k)$nn.dist
k_distances <- sort(nn_distances[, 7])  # Odległość do k-tego sąsiada

qplot(1:length(k_distances), k_distances, geom = "line") +
  labs(
    x = "Posortowane punkty",
    y = paste("Odległość do", k, "najbliższego sąsiada"),
    title = "Wykres odległości k-najbliższych sąsiadów"
  )

# Wykonanie algorytmu HDBSCAN 4
par(mfrow=c(4,2))
hdb <- hdbscan(points_coords, minPts = 4)
plot(hdb, show_flat = TRUE)


# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(hdb$cluster == 0, 0, hdb$cluster+1))
title("minPts 4")
axis(1)
axis(2)

# Wykonanie algorytmu HDBSCAN 8
hdb <- hdbscan(points_coords, minPts = 8)
plot(hdb, show_flat = TRUE)


# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(hdb$cluster == 0, 0, hdb$cluster+1))
title("minPts 8")
axis(1)
axis(2)

# Wykonanie algorytmu HDBSCAN 20
hdb <- hdbscan(points_coords, minPts = 20)
plot(hdb, show_flat = TRUE)


# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(hdb$cluster == 0, 0, hdb$cluster+1))
title("minPts 20")
axis(1)
axis(2)

# Wykonanie algorytmu HDBSCAN 25
hdb <- hdbscan(points_coords, minPts = 25)
plot(hdb, show_flat = TRUE)


# Przypisanie wyników klasteryzacji
plot(osiedle$geometry, xlab='X', ylab="Y")
plot(points$geometry, add=TRUE, pch=19, cex=0.5, col=ifelse(hdb$cluster == 0, 0, hdb$cluster+1))
title("minPts 25")
axis(1)
axis(2)
par(mfrow=c(1,1))


minPts_values <- 2:10  # Przykład: próbujemy dla minPts od 2 do 10
distances <- numeric(length(minPts_values))  # Przechowa odległości
# Obliczanie średnich odległości do minPts-tego najbliższego sąsiada
for (i in 1:length(minPts_values)) {
  minPts <- minPts_values[i]
  knn_dist <- kNNdist(points_coords, k = minPts)  # Obliczanie odległości k-najbliższych sąsiadów
  distances[i] <- mean(knn_dist)  # Średnia odległość do minPts-tego sąsiada
}

# Tworzenie wykresu
plot(
  minPts_values, 
  distances, 
  type = "o", 
  xlab = "minPts", 
  ylab = "Średnia odległość",
  main = "Wykres zależności minPts i promienia otoczenia"
)


library(dbscan)

# Przygotowanie układu do 3 wykresów w jednej linii
par(mfrow = c(1, 3))  # Układ 1x3 (1 wiersz, 3 kolumny)

# Wykres 1: DBSCAN 4:190
db <- dbscan(points_coords, eps = 190, minPts = 4)
plot(osiedle$geometry, xlab = 'X', ylab = "Y", main = "DBSCAN 4:190")
plot(points$geometry, add = TRUE, pch = 19, cex = 0.5, col = ifelse(db$cluster == 0, 0, db$cluster + 1))
axis(1)
axis(2)

# Wykres 2: DBSCAN 6:240
db <- dbscan(points_coords, eps = 240, minPts = 6)
plot(osiedle$geometry, xlab = 'X', ylab = "Y", main = "DBSCAN 6:240")
plot(points$geometry, add = TRUE, pch = 19, cex = 0.5, col = ifelse(db$cluster == 0, 0, db$cluster + 1))
axis(1)
axis(2)
# Wykres 3: DBSCAN 10:340
db <- dbscan(points_coords, eps = 340, minPts = 10)
plot(osiedle$geometry, xlab = 'X', ylab = "Y", main = "DBSCAN 10:340")
plot(points$geometry, add = TRUE, pch = 19, cex = 0.5, col = ifelse(db$cluster == 0, 0, db$cluster + 1))
axis(1)
axis(2)
# Resetowanie układu graficznego
par(mfrow = c(1, 1))


