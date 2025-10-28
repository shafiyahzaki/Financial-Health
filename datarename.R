# Pasang pakej yang diperlukan (hanya perlu sekali)
# install.packages("dplyr")
# install.packages("psych") 
# install.packages("randomForest") 

# Muat naik pakej
library(dplyr)
library(psych) # Untuk Analisis Kebolehpercayaan (Cronbach's Alpha)
library(randomForest) # Untuk model Random Forest

# Muat naik data dari fail CSV
data<- read.csv(file.choose(),header = TRUE, stringsAsFactors = FALSE)

# Tunjukkan 6 baris pertama data
head(data)

nrow(data)
ncol(data)

sapply(data, class)
names(data)

data<- data %>%
  # Pilih kolum yang berkaitan sahaja, buang kolum kosong di akhir
  select(Jantina:Kesihatan6) %>%
  
  # Tukar pemboleh ubah kategori ke jenis 'factor'
  mutate(across(c(Jantina, Tahun_pengajian, CGPA_terkini, Akaun_Bank:Kesihatan6), as.factor)) %>%
  
  # Tukar kolum umur kepada jenis numerik (jika ada nilai seperti '22\n' dari data mentah)
  mutate(Umur = as.numeric(gsub("[^0-9.]", "", Umur)))

# Semak struktur data selepas pembersihan awal
str(data)

View(data)


#ujian kebolehpercayaan
# Dataframe hanya untuk item skala likert (Numerik)
# Nota: Pastikan kolum ini sudah dalam format numerik. Jika R membaca sebagai factor, sila tukar.
likert_data <- data %>%
  select(Rakan1:Literasi4) %>%
  mutate(across(everything(), as.numeric)) 

# Ujian Cronbach's Alpha
library(psych)
alpha_rakan <- alpha(likert_data %>% select(Rakan1:Rakan6))
alpha_ibubapa <- alpha(likert_data %>% select(IbuBapa1:IbuBapa4))
alpha_literasi <- alpha(likert_data %>% select(Literasi1:Literasi4))

print("Cronbach's Alpha untuk Rakan:")
print(alpha_rakan$total)
print("Cronbach's Alpha untuk IbuBapa:")
print(alpha_ibubapa$total)
print("Cronbach's Alpha untuk Literasi:")
print(alpha_literasi$total)
# Rujukan: Alpha > 0.70 dianggap boleh dipercayai

#Latar belakang responden
install.packages("purrr")
library(purrr)


data <- data %>%
  mutate(Jantina = case_when(
    Jantina %in% c("Male", "Lelaki", "male") ~ "Lelaki",
    Jantina %in% c("Female", "Perempuan", "female") ~ "Perempuan",
    TRUE ~ Jantina  # kalau ada nilai lain, biar kekal
  ))


data %>%
  count(Tahun_pengajian) %>%
  mutate(percent = round((n / sum(n)) * 100, 1))
  

library(dplyr)
library(purrr)

# Senarai pemboleh ubah kategori
vars <- c("Jantina", "Umur", "Tahun_pengajian", "CGPA_terkini", "Kad_Bank", "Akaun_Bank")

# Ulang proses count + percent untuk setiap variable
# Ulang proses count + percent untuk setiap variable
summary_table <- map_df(vars, function(v) {
  data %>%
    mutate(across(all_of(v), as.character)) %>%   # Tukar ke character supaya boleh digabung
    count(!!sym(v)) %>%
    mutate(
      percent = round((n / sum(n)) * 100, 1),
      Variable = v
    ) %>%
    rename(.x = 1) %>%
    select(Variable, .x, n, percent)
})

