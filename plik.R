library(dplyr)

tab1 <- read.table('C:/Users/marci/OneDrive/Pulpit/moje-dane-MD/samojluk_output_0.txt', sep=';', header = TRUE)
filter_tab1 <- dplyr::filter(tab1, Dip_ang > 0 & Dip_ang < 91, X_N > -0.05 & X_N < 0.05, Y_N > -0.05 & Y_N < 0.05)
grupowanie_normals <- kmeans(as.matrix(select(filter_tab1, c("X_N", "Y_N", "Z_N"))), centers = 3, nstart = 40, iter.max = 100000, algorithm = "Lloyd")

filter_tab1$clustering <- grupowanie_normals$cluster


# Podział danych na podstawie klastrów
df1 <- dplyr::filter(filter_tab1, clustering == 1)
df2 <- dplyr::filter(filter_tab1, clustering == 2)
df3 <- dplyr::filter(filter_tab1, clustering == 3)

# Zapisz ramki danych do plików
write.table(df1[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df1.txt", row.names = FALSE, sep = ",")
write.table(df2[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df2.txt", row.names = FALSE, sep = ",")
write.table(df3[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df3.txt", row.names = FALSE, sep = ",")

library(ggplot2)

ggplot(data=filter_tab1, aes(x = Y_C, y = X_C, color = factor(clustering))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red", "green"))

ggplot(data=tab1, aes(x = Y_C, y = X_C, color = Dip_ang )) +
  geom_point()
ggplot(data=filter_tab1, aes(x = Y_C, y = X_C, color = Dip_ang )) +
  geom_point()


write.table(df1[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df1.txt", row.names = FALSE, sep = ",")
write.table(df2[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df2.txt", row.names = FALSE, sep = ",")
write.table(df3[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df3.txt", row.names = FALSE, sep = ",")

calculate_dip_angles <- function(centers) {
  norm_vec <- function(x) sqrt(sum(x^2))
  normalized_centers <- t(apply(centers, 1, function(row) row / norm_vec(row)))
  dip_dir_and_angle <- function(normed_representant) {
    dip_ang <- acos(normed_representant[3]) * (180 / pi)
    if (dip_ang > 90) dip_ang <- dip_ang - 90
    dip_dir <- atan2(normed_representant[2], normed_representant[1]) * (180 / pi)
    if (dip_dir > 360) dip_dir <- dip_dir - 360
    return(c(Dip_ang = dip_ang, Dip_dir = dip_dir))
  }
  result <- t(apply(normalized_centers, 1, dip_dir_and_angle))
  colnames(result) <- c("Dip_ang", "Dip_dir")
  return(result)
}

# Zastosowanie:
orientation_data <- calculate_dip_angles(grupowanie_normals$centers)
orientation_data
write.csv(orientation_data, "C:/Users/marci/OneDrive/Pulpit/MD/skupienia.csv", row.names = FALSE)


##############################################################################3

tab1_2 <- read.table('C:/Users/marci/OneDrive/Pulpit/moje-dane-MD/samojluk_output_0.txt', sep=';', header = TRUE)
filter_tab1_2 <- dplyr::filter(tab1_2, Dip_ang > 0 & Dip_ang < 91, X_N > -0.05 & X_N < 0.05, Y_N > -0.05 & Y_N < 0.05)
grupowanie_normals_2 <- kmeans(as.matrix(select(filter_tab1, c("X_D", "Y_D", "Z_D"))), centers = 3, nstart = 40, iter.max = 100000, algorithm = "Lloyd")

filter_tab1$clustering <- grupowanie_normals_2$cluster


# Podział danych na podstawie klastrów
df1_2 <- dplyr::filter(filter_tab1, clustering == 1)
df2_2 <- dplyr::filter(filter_tab1, clustering == 2)
df3_2 <- dplyr::filter(filter_tab1, clustering == 3)

df1_2

# Zapisz ramki danych do plików
write.table(df1_2[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df1_2.txt", row.names = FALSE, sep = ",")
write.table(df2_2[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df2_2.txt", row.names = FALSE, sep = ",")
write.table(df3_2[, c('Dip_dir', 'Dip_ang')], "C:/Users/marci/OneDrive/Pulpit/MD/df3_2.txt", row.names = FALSE, sep = ",")


library(ggplot2)

ggplot(data=filter_tab1, aes(x = Y_C, y = X_C, color = factor(clustering))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red", "green"))

orientation_data_2 <- calculate_dip_angles(grupowanie_normals_2$centers)
orientation_data_2
write.csv(orientation_data_2, "C:/Users/marci/OneDrive/Pulpit/MD/skupienia2.csv", row.names = FALSE)
