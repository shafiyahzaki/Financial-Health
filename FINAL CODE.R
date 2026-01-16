data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
head(data)
sum(is.na(data))

unique(data$A1)
data$A1<-tolower(data$A1)
data$A1<-factor(
  data$A1,
  levels = c("perempuan", "lelaki"),
  labels = c("Perempuan", "Lelaki")
)

unique(data$A2)
data$A2<-as.numeric(data$A2)

unique(data$A3)
data$A3 <- factor(
  data$A3,
  levels = c("Tahun 1", "Tahun 2", "Tahun 3", "Tahun 4"),
  ordered = TRUE
)


unique(data$A4)
data$A4 <- tolower(trimws(data$A4))
data$A4 <- factor(
  data$A4,
  levels = c(
    "1.90 dan ke bawah",
    "2.00 - 2.49",
    "2.50 - 3.74",
    "3.75 - 4.00"
  ),
  ordered = TRUE
)

unique(data$A5)
data$A5 <- trimws(tolower(data$A5))
data$A5 <- factor(
  data$A5,
  levels = c("tidak", "ya"),
  labels = c("Tidak", "Ya")
)

unique(data$A6)
data$A6<- trimws(tolower(data$A6))
data$A6 <- factor(
  data$A6,
  levels = c("tidak", "ya"),
  labels = c("Tidak", "Ya")
)

unique(data$B1)
data$B1<- trimws(tolower(data$B1))
unique(data$B1)
data$B1 <- factor(
  data$B1,
  levels = c("tidak", "ya"),
  labels = c("Tidak", "Ya")
)

unique(data$B2)
data$B2<- trimws(tolower(data$B2))
data$B2 <- factor(
  data$B2,
  levels = c("tidak", "ya"),
  labels = c("Tidak", "Ya")
)

unique(data$B3)
data$B3<- trimws(tolower(data$B3))
data$B3 <- factor(
  data$B3,
  levels = c(
    "kurang dari rm50",
    "rm50 - rm100",
    "rm101 - rm200",
    "rm201 - rm300",
    "lebih dari rm300"
  ),
  labels = c(
    "Kurang dari RM50",
    "RM50 - RM100",
    "RM101 - RM200",
    "RM201 - RM300",
    "Lebih dari RM300"
  ),
  ordered = TRUE
)


unique(data$B4)
data$B4<- trimws(tolower(data$B4))
library(dplyr)
library(stringr)

data$B4 <- data$B4 %>%
  tolower() %>%
  trimws() %>%
  str_replace_all("\\(.*?\\)", "") %>%
  str_replace_all("\\n", " ")


data$B4 <- sapply(data$B4, function(x) {
  
  kategori <- c()
  
  # Akaun bank (termasuk tabung Be U Bank Islam)
  if (grepl("akaun bank|bank account|\\bbank\\b|tabung be u|be u bank islam", x))
    kategori <- c(kategori, "akaun bank")
  
  # E-dompet
  if (grepl("e-wallet|ewallet|e dompet|e-dompet", x))
    kategori <- c(kategori, "e-dompet")
  
  # Tabung (fizikal / piggy bank sahaja)
  if (grepl("piggy bank|piggybank|\\btabung\\b", x) &&
      !grepl("be u|bank islam", x))
    kategori <- c(kategori, "tabung")
  
  # Aplikasi pelaburan
  if (grepl("pelaburan|investment|public gold|crypto|emas|emas fizikal|asnb", x))
    kategori <- c(kategori, "aplikasi pelaburan")
  
  # Simpanan melalui ibu bapa
  if (grepl("ibu bapa|parents|saving through parents", x))
    kategori <- c(kategori, "simpanan melalui ibu bapa")
  
  paste(unique(kategori), collapse = ", ")
})

library(tidyr)

unique(
  data %>%
    separate_rows(B4, sep = ",") %>%
    mutate(B4 = trimws(B4)) %>%
    pull(B4)
)


unique(data$B5)
data$B5 <- factor(
  data$B5,
  levels = c("Kecemasan", "Tabiat", "Masa depan",
             "Pendidikan", "Peribadi", "Hiburan")
)

unique(data$B6)
data$B6<- trimws(tolower(data$B6))
data$B6 <- factor(
  data$B6,
  levels = c(
    "kurang dari 1 tahun",
    "1 - 3 tahun",
    "3 - 5 tahun",
    "lebih dari 5 tahun"
  ),
  ordered = TRUE
)

unique(data$B7)

unique(data$B8)
data$B8 <- factor(
  data$B8,
  levels = c(
    "simpanan lebih rendah daripada perbelanjaan",
    "simpanan sama dengan perbelanjaan",
    "simpanan lebih tinggi daripada perbelanjaan"
  ),
  ordered = TRUE
)

unique(data$D1)
data$D1 <- factor(
  data$D1,
  levels = c("tidak mampu dan sering memerlukan bantuan",
             "ya, tetapi kadangkala memerlukan bantuan",
             "ya, sepenuhnya mampu tanpa bantuan"),
  ordered = TRUE
)

unique(data$D2)
data$D2 <- factor(
  data$D2,
  levels = c("ya, jumlah besar",
             "ya, jumlah kecil",
             "tidak"),
  ordered = TRUE
)

unique(data$D3)
data$D3 <- factor(
  data$D3,
  levels = c("tidak pernah",
             "kadangkala",
             "ya, setiap bulan"),
  ordered = TRUE
)

unique(data$D4)
data$D4 <- factor(
  data$D4,
  levels = c("sangat jarang",
             "kadangkala",
             "hampir setiap bulan"),
  ordered = TRUE
)

unique(data$D5)
data$D5 <- factor(
  data$D5,
  levels = c("tidak bersedia langsung",
             "agak bersedia",
             "sangat bersedia"),
  ordered = TRUE
)

unique(data$D6)
data$D6 <- factor(
  data$D6,
  levels = c("tidak sihat",
             "kurang sihat",
             "sihat"),
  ordered = TRUE
)

### PROFIL RESPONDEN ###

library(janitor)

Jantina <- data %>% 
  tabyl(A1) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
Jantina

unique(data$A2)
data <- data %>%
  mutate(Umur_Kumpulan = case_when(
    A2 <= 20 ~ "20 tahun dan ke bawah",
    A2 >= 21 & A2 <= 22 ~ "21–22 tahun",
    A2 >= 23 ~ "23 tahun dan ke atas"
  ))

Umur <- data %>% 
  tabyl(Umur_Kumpulan) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
Umur

Tahun_Pengajian <- data %>% 
  tabyl(A3) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
Tahun_Pengajian

CGPA <- data %>% 
  tabyl(A4) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
CGPA

### ANALISIS OBJEKTIF 1 ###

#bina dummy variable
data$B4_bank       <- ifelse(grepl("akaun bank", data$B4), 1, 0)
data$B4_edompet    <- ifelse(grepl("e-dompet", data$B4), 1, 0)
data$B4_tabung     <- ifelse(grepl("tabung", data$B4), 1, 0)
data$B4_pelaburan  <- ifelse(grepl("aplikasi pelaburan", data$B4), 1, 0)
data$B4_ibu_bapa   <- ifelse(grepl("simpanan melalui ibu bapa", data$B4), 1, 0)

colSums(data[, c(
  "B4_bank",
  "B4_edompet",
  "B4_tabung",
  "B4_pelaburan",
  "B4_ibu_bapa"
)])

# B1 – Ada simpanan
tbl_simpanan <- data %>%
  tabyl(B1) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

# B2 – Simpan konsisten
tbl_konsisten <- data %>%
  tabyl(B2) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

# B3 – Jumlah simpanan
tbl_jumlah <- data %>%
  tabyl(B3) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

# B4 – Cara simpanan (multiple response)
B4_freq <- colSums(data[, c(
  "B4_bank",
  "B4_edompet",
  "B4_tabung",
  "B4_pelaburan",
  "B4_ibu_bapa"
)])

B4_pct <- round(B4_freq / nrow(data) * 100, 1)

tbl_cara <- tibble(
  Cara_Simpanan = c(
    "Akaun bank",
    "E-dompet",
    "Tabung",
    "Aplikasi pelaburan",
    "Simpanan melalui ibu bapa"
  ),
  Bilangan = as.numeric(B4_freq),
  Peratus = B4_pct
)
tbl_cara


tbl_tujuan <- data %>%
  tabyl(B5) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

# B6 – Tempoh simpanan
tbl_tempoh <- data %>%
  tabyl(B6) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
tbl_tempoh

library(ggplot2)
B1_sum <- data %>%
  count(B1) %>%
  mutate(pct = n / sum(n) * 100)
ggplot(B1_sum, aes(x = B1, y = pct, fill = B1)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Pemilikan Simpanan dalam Kalangan Pelajar",
    x = NULL, y = "Peratus (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


B2_sum <- data %>%
  count(B2) %>%
  mutate(pct = n / sum(n) * 100)
ggplot(B2_sum, aes(x = B2, y = pct, fill = B2)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Konsistensi Amalan Menyimpan Wang",
    x = NULL, y = "Peratus (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

B3_sum <- data %>%
  count(B3) %>%
  mutate(pct = n / sum(n) * 100)
ggplot(B3_sum, aes(x = B3, y = pct)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            vjust = -0.5, size = 3.8) +
  labs(
    title = "Anggaran Jumlah Simpanan Bulanan Pelajar",
    x = "Julat Anggaran Simpanan", y = "Peratus (%)"
  ) +
  theme_minimal()


B4_freq <- colSums(data[, c(
  "B4_bank",
  "B4_edompet",
  "B4_tabung",
  "B4_pelaburan",
  "B4_ibu_bapa"
)], na.rm = TRUE)
B4_df <- tibble(
  Cara = c(
    "Akaun Bank",
    "E-dompet",
    "Tabung",
    "Aplikasi Pelaburan",
    "Melalui Ibu Bapa"
  ),
  Bilangan = as.numeric(B4_freq)
) %>%
  mutate(pct = Bilangan / nrow(data) * 100)
ggplot(B4_df, aes(x = reorder(Cara, pct), y = pct)) +
  geom_col(fill = "#41AE76") +
  coord_flip() +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            hjust = -0.1, size = 3.6) +
  labs(
    title = "Kaedah Simpanan Wang Pelajar",
    x = NULL, y = "Peratus (%)"
  ) +
  theme_minimal()

B5_sum <- data %>%
  count(B5) %>%
  mutate(pct = n / sum(n) * 100)
ggplot(B5_sum, aes(x = reorder(B5, pct), y = pct)) +
  geom_col(fill = "#DD1C77") +
  coord_flip() +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            hjust = -0.1, size = 3.6) +
  labs(
    title = "Tujuan Utama Menyimpan Wang",
    x = NULL, y = "Peratus (%)"
  ) +
  theme_minimal()

B6_sum <- data %>%
  count(B6) %>%
  mutate(pct = n / sum(n) * 100)
ggplot(B6_sum, aes(x = B6, y = pct)) +
  geom_col(fill = "#756BB1", width = 0.7) +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            vjust = -0.5, size = 3.8) +
  labs(
    title = "Tempoh Pelajar Menyimpan Wang",
    x = "Tempoh", y = "Peratus (%)"
  ) +
  theme_minimal()

data$B7_kecantikan <- ifelse(grepl("Penjagaan diri/kecantikan", data$B7), 1, 0)
data$B7_beli       <- ifelse(grepl("Membeli-belah", data$B7), 1, 0)
data$B7_akademik   <- ifelse(grepl("Pengajian/akademik", data$B7), 1, 0)
data$B7_hiburan    <- ifelse(grepl("Hiburan", data$B7), 1, 0)
data$B7_angkutan   <- ifelse(grepl("Pengangkutan", data$B7), 1, 0)
data$B7_telco      <- ifelse(grepl("Tambah nilai telefon/data", data$B7), 1, 0)
data$B7_sukan      <- ifelse(grepl("Sukan", data$B7), 1, 0)

B7_freq <- colSums(data[, c(
  "B7_kecantikan",
  "B7_beli",
  "B7_akademik",
  "B7_hiburan",
  "B7_angkutan",
  "B7_telco",
  "B7_sukan"
)], na.rm = TRUE)

B7_pct <- round(B7_freq / nrow(data) * 100, 1)

tbl_belanja <- tibble(
  Kategori_Perbelanjaan = c(
    "Penjagaan diri / kecantikan",
    "Membeli-belah",
    "Pengajian / akademik",
    "Hiburan",
    "Pengangkutan",
    "Tambah nilai telefon / data",
    "Sukan"
  ),
  Bilangan = as.numeric(B7_freq),
  Peratus = B7_pct
)
tbl_belanja
ggplot(tbl_belanja,
       aes(x = reorder(Kategori_Perbelanjaan, Peratus),
           y = Peratus)) +
  geom_col(fill = "#2CA25F") +
  coord_flip() +
  geom_text(aes(label = paste0(Peratus, "%")),
            hjust = -0.1, size = 3.8) +
  labs(
    title = "Corak Perbelanjaan Pelajar UKM",
    x = NULL,
    y = "Peratus (%)"
  ) +
  theme_minimal()

tbl_status <- data %>%
  tabyl(B8) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
tbl_status

B8_sum <- data %>%
  count(B8) %>%
  mutate(pct = n / sum(n) * 100)
ggplot(B8_sum, aes(x = B8, y = pct)) +
  geom_col(fill = "#3182BD", width = 0.7) +
  geom_text(aes(label = paste0(round(pct,1), "%")),
            vjust = -0.5, size = 3.8) +
  labs(
    title = "Status Simpanan dan Perbelanjaan Pelajar UKM",
    x = NULL,
    y = "Peratus (%)"
  ) +
  theme_minimal()


### ANALISIS OBJEKTIF 2 ###

dv_list<-c("B2","B3","B6","B8")
results_kw<-list()

for (dv in dv_list) {
  
  DV_numeric <- as.numeric(data[[dv]])
  DV_rank    <- rank(DV_numeric, ties.method = "average")
  
  kw_test <- kruskal.test(DV_numeric ~ data$D6)
  
  mean_rank_tbl <- data.frame(
    D6 = levels(data$D6),
    N = as.numeric(table(data$D6)),
    Mean_Rank = tapply(DV_rank, data$D6, mean)
  )
  
  results_kw[[dv]] <- list(
    H        = as.numeric(kw_test$statistic),
    df       = as.numeric(kw_test$parameter),
    p_value  = kw_test$p.value,
    MeanRank = mean_rank_tbl
  )
}

results_kw

#ujian post-hoc

library(dunn.test)
data$B3_num <- as.numeric(data$B3)
dunn.test(
  x = data$B3_num,
  g = data$D6,
  method = "bonferroni"
)

data$B2_num <- as.numeric(data$B2)
data$B6_num <- as.numeric(data$B6)
data$B8_num <- as.numeric(data$B8)

dunn.test(data$B2_num, data$D6, method = "bonferroni")
dunn.test(data$B6_num, data$D6, method = "bonferroni")
dunn.test(data$B8_num, data$D6, method = "bonferroni")

### ANALISIS OBJEKTIF 3 ###

# 1. LOAD LIBRARY
library(tidyverse)
library(janitor)
library(seminr)

  clean_data <- data %>%
  clean_names() %>%
  mutate(
    # --- Bahagian B: Tingkah Laku (Semakin tinggi = Semakin Baik) ---
    tl_b2 = ifelse(trimws(tolower(b2)) == "ya", 1, 0),
    
    tl_b3 = as.numeric(factor(trimws(tolower(b3)), 
                              levels = c("kurang dari rm50", "rm50 - rm100", "rm101 - rm200", "rm201 - rm300", "lebih dari rm300"))),
    
    tl_b6 = as.numeric(factor(trimws(tolower(b6)), 
                              levels = c("kurang dari 1 tahun", "1 - 3 tahun", "3 - 5 tahun", "lebih dari 5 tahun"))),
    
    tl_b8 = as.numeric(factor(trimws(tolower(b8)), 
                              levels = c("simpanan lebih rendah daripada perbelanjaan", "simpanan sama dengan perbelanjaan", "simpanan lebih tinggi daripada perbelanjaan"))),
        across(matches("^c[1-3]_"), as.numeric),
    
    # reverse coding semua item negatif
    # Menukar skor supaya 5 (Sangat Setuju dengan tabiat buruk) menjadi 1 (Sangat Tidak Setuju/Berdisiplin)

    rakan1_rev = 6 - c1_1,
    rakan2_rev = 6 - c1_2,
    rakan3_rev = 6 - c1_3,
    rakan4_rev = 6 - c1_4,
    rakan5_rev = 6 - c1_5,
    rakan6_rev = 6 - c1_6,
    
    # 2. Reverse item Keluarga yang negatif (C2.2 - Bergantung pada duit tambahan)
    ibu2_rev = 6 - c2_2
  )

sem_data <- clean_data %>%
  select(
    rakan1 = rakan1_rev, rakan2 = rakan2_rev, rakan3 = rakan3_rev, 
    rakan4 = rakan4_rev, rakan5 = rakan5_rev, rakan6 = rakan6_rev,
    ibu1 = c2_1, ibu2 = ibu2_rev, ibu3 = c2_3, ibu4 = c2_4,
    lit1 = c3_1, lit2 = c3_2, lit3 = c3_3, lit4 = c3_4,
    tl2 = tl_b2, tl3 = tl_b3, tl6 = tl_b6, tl8 = tl_b8
  )

measurement_model <- constructs(
  composite("RakanSebaya", multi_items("rakan", 1:6)),
  composite("IbuBapa",     multi_items("ibu", 1:4)),
  composite("Literasi",    multi_items("lit", 1:4)),
  composite("TingkahLaku", multi_items("tl", c(2, 3, 6, 8)))
)

structural_model <- relationships(
  paths(from = c("RakanSebaya", "IbuBapa", "Literasi"), 
        to = "TingkahLaku")
)
pls_model <- estimate_pls(
  data = sem_data,
  measurement_model = measurement_model,
  structural_model = structural_model,
  inner_weights = path_weighting 
)
pls_model$outer_loadings


measurement_model_refined <- constructs(
  composite("RakanSebaya", multi_items("rakan", c(2, 5, 6))),
  composite("IbuBapa",     multi_items("ibu",   c(3, 4))),
  composite("Literasi",    multi_items("lit",   c(2, 3, 4))),
  composite("TingkahLaku", multi_items("tl",    c(6, 8)))
)

measurement_model_final <- constructs(
  # Kekalkan rakan 1, 2, 5, 6
  composite("RakanSebaya", multi_items("rakan", c(2, 5, 6))), 
  
  # Kekalkan ibu 3, 4 sahaja
  composite("IbuBapa",     multi_items("ibu",   c(3, 4))),     
  
  # Kekalkan lit 2, 3, 4
  composite("Literasi",    multi_items("lit",   c(2, 3, 4))),  
  
  # Kekalkan tl 6, 8
  composite("TingkahLaku", multi_items("tl",    c(6, 8)))      
)

pls_model_final <- estimate_pls(
  data = sem_data,
  measurement_model = measurement_model_final,
  structural_model = structural_model
)
summary(pls_model_final)
summary_final$validity$htmt

boot_final <- bootstrap_model(pls_model_final, nboot = 5000)

summary(boot_final)
t_values <- c(2.932, 2.249, -0.052)
names(t_values) <- c("RakanSebaya", "IbuBapa", "Literasi")
p_values <- 2 * (1 - pt(abs(t_values), df = Inf))
round(p_values, 4)


plot(pls_model_final)


