setwd("/Users/andi.ashari/Documents/School/Semester4/DataMining/tourism-data-idn")

#===============================================================================IMPORT PACKAGES AND DATAS

# Load packages for data manipulation
library(readr)
library(dplyr)
library(data.table)
library(tidyr)

# Load packages for data visualization
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(forcats)

# Load packages for modeling
install.packages("tensorflow")
install.packages("keras")
library(tensorflow)
library(keras)


#import data
destination <- read.csv("package_tourism.csv")
View(rating_bandung)
tourism <- read.csv("user.csv")
id_user <- read.csv("tourism_with_id.csv")
rating <- read.csv("tourism_rating.csv")

#===============================================================================EXPLORING DATA

#print data untuk di cek
print(destination)
print(tourism)
print(id_user)
print(rating)


# Menghapus kolom 'Unnamed: 11' dan 'Unnamed: 12'
id_user <- id_user[, !(names(id_user) %in% c("X", "X.1"))]
head(id_user, 2)

# Membuat subset data yang hanya berisi baris dengan nilai 'City' sama dengan 'Bandung'
id_user <- subset(id_user, City == "Bandung")

# Menampilkan dua baris pertama data
head(id_user, 2)

# Menampilkan informasi tentang dataframe
str(id_user)

# Menghapus kolom 'Time_Minutes'
id_user <- id_user[, -which(names(id_user) == "Time_Minutes")]
head(id_user, 2)

View(id_user_bandung)

#cek missing data di setiap dataset
sum(is.na(destination))
sum(is.na(tourism))
sum(is.na(id_user))
sum(is.na(rating))

# menghaus rows yang memiliki missing values dari dataset
id_user_cleaned <- na.omit(id_user)
rating_cleaned <- na.omit(rating)
destination <- na.omit(destination)
tourism <- na.omit(tourism)

#melihat gambaran data rating
head(rating_cleaned,5)

# Menampilkan informasi tentang dataframe
str(rating_cleaned)

# Mendapatkan nilai unik dari kolom "Place_Id" pada dataframe 'id_user_bandung'
id_user_bandung<- unique(id_user_cleaned)

# Menghapus row yang memiliki nilai Prica yang outliner
id_user_bandung <- id_user_bandung %>%
  filter(Price != "375000" & Price != "280000" & Price != "175000" & Price != "150000" )

# Melakukan join antara dataframe 'rating' dan 'id_user_bandung' menggunakan kolom 'Place_Id'
rating_bandung<- merge(rating_cleaned,id_user_bandung, by="Place_Id", all.y=TRUE)

str(rating_bandung, 5)

# Melihat ukuran dataset rating untuk Kota Bandung
dim(rating_bandung)

# Melihat gambaran data user
head(tourism)

# Merubah data user agar hanya berisi user yang pernah megunjungi wisata di Kota Bandung
rate_unique <- unique(rating_bandung[, "User_Id"])
tourism <- merge(tourism, rating_bandung[, "User_Id", drop = FALSE], by = "User_Id", all.y = TRUE) %>%
  distinct() %>%
  arrange(User_Id)
head(tourism)
dim(tourism)

# create a sample dataframe for new prices
new_prices <- data.frame(Place_Id = c(249, 218, 234, 237, 325, 296, 281), Price = c(60000, 55000, 55000, 50000,60000, 60000, 60000))

# join id_user_bandung and new_prices based on Place_Id and update Price column
id_user_bandung <- id_user_bandung %>%
  left_join(new_prices, by = "Place_Id") %>%
  mutate(Price = ifelse(!is.na(Price.y), Price.y, Price.x)) %>%
  select(-Price.x, -Price.y) # remove the old and new Price columns

summary(id_user_bandung)
summary(rating_bandung)
#===============================================================================VISUALIZING DATA

# Membuat dataframe berisi lokasi dengan jumlah rating terbanyak
top_10 <- rating_bandung %>%
  count(Place_Id) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  left_join(filter(id_user_bandung, !is.na(Place_Name)), by = "Place_Id")
top_10 <- na.omit(top_10)
  
# Membuat visualisasi wisata dengan jumlah rating terbanyak
ggplot(top_10, aes(y = reorder(Place_Name, n), x = n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "green", high = "red") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20))) +
  ggtitle("Jumlah Tempat Wisata dengan Rating Terbanyak") + xlab("Jumlah Rating") + ylab("Nama Lokasi")



# Convert Category menjadi factor numeric untuk membuat plot perbandingan jumlah ketegori wisata di kota Bandung
id_user_bandung$Category <- factor(id_user_bandung$Category, levels = unique(id_user_bandung$Category))
ggplot(id_user_bandung, aes(x = Category, fill = Category)) +
  geom_bar() +
  ggtitle("Perbandingan Jumlah Kategori Wisata di Kota Bandung", 
          subtitle = " ") +
  xlab("Kategori") +
  ylab("Jumlah") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

summary(tourism$Location)

# Membuat visualisasi distribusi usia user
boxplot(tourism$Age, horizontal = TRUE, col = "#0072B2",
        main = "Usia Pengunjung", xlab = "Usia", ylab = NULL)

summary(id_user_bandung$Price)


# Membuat visualisasi distribusi harga masuk tempat wisata
boxplot(id_user_bandung$Price, horizontal = TRUE, col = "#0072B2",
        main = "Distribusi Harga Masuk Wisata di Kota Bandung", xlab = "Harga", ylab = NULL)

# Memfilter asal kota dari user
askot <- sapply(tourism$Location, function(x) { strsplit(x, ",")[[1]][1] })

# Visualisasi asal kota dari user
ggplot(data = data.frame(askot), aes(y = reorder(askot, table(askot)[askot]), fill = askot)) + 
  geom_bar() +
  ggtitle("Jumlah Asal Kota pengunjung") +
  scale_fill_manual(values = scales::hue_pal()(length(unique(askot)))) +
  theme(plot.title = element_text(hjust = 0.5))


# Membuat scatter plot untuk melihat hubungan antara variabel 'Price' dan 'Rating' untuk melihat hubungan antara 2 dataframe
ggplot(id_user_bandung, aes(x = Price, y = Rating)) +
  geom_point() +
  ggtitle("Hubungan antara Harga Masuk dan Rating di Kota Bandung", 
          subtitle = " ") +
  xlab("Harga Masuk") +
  ylab("Rating") +
  theme_minimal()


#===============================================================================MODELING DATA

# Membaca dataset untuk dilakukan encoding
df <- rating_bandung
head(df)

# Membuat Fungsi untuk Melakukan Encoding
dict_encoder <- function(col, data = df) {
  
  # Mengubah kolom suatu dataframe menjadi list tanpa nilai yang sama
  unique_val <- unique(data[[col]])
  
  # Melakukan encoding value kolom suatu dataframe ke angka
  val_to_val_encoded <- setNames(as.integer(seq_along(unique_val)), unique_val)
  
  # Melakukan proses encoding angka ke value dari kolom suatu dataframe
  val_encoded_to_val <- setNames(unique_val, seq_along(unique_val))
  
  return(list(val_to_val_encoded, val_encoded_to_val))
}

# Encoding User_Id
user_to_user_encoded <- dict_encoder('User_Id', data = df)[[1]]

# Mapping User_Id ke dataframe
df$user <- user_to_user_encoded[df$User_Id]


# Encoding Place_Id
place_to_place_encoded <- dict_encoder('Place_Id', data = df)[[1]]

# Mapping Place_Id ke dataframe place
df$place <- place_to_place_encoded[df$Place_Id]

# Mendapatkan jumlah user dan place
num_users <- length(user_to_user_encoded)
num_places <- length(place_to_place_encoded)

# Mengubah rating menjadi nilai float
df$Place_Ratings <- as.numeric(as.character(df$Place_Ratings))

# Mendapatkan nilai minimum dan maksimum rating
min_rating <- min(df$Place_Ratings)
max_rating <- max(df$Place_Ratings)
cat(paste("Number of User: ", num_users, ", Number of Place: ", num_places, ", Min Rating: ", min_rating, ", Max Rating: ", max_rating))

# cek dataset
set.seed(42)
df <- df %>%
  sample_frac(1)
head(df, 10)



