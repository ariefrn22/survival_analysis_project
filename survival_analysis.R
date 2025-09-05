# Install dan panggil package yang dibutuhkan
install.packages("readxl")
install.packages("survival")
install.packages("survminer")
install.packages("survey")

library(readxl)
library(survival)
library(survminer)
library(survey)

# Membaca data dari file Excel
data <- read_excel("data_simulasi_survival_kanker_paru_R.xlsx")
View(data)
data
# Menampilkan 6 baris pertama data untuk pemeriksaan awal
head(data)

# Mengonversi variabel kategori menjadi faktor untuk analisis
data$X1_Usia <- factor(data$X1_Usia, levels = c("Dibawah 45", "Diatas 45"))
data$X2_Stadium <- factor(data$X2_Stadium, levels = c("Stadium Dini", "Stadium Lanjut"))
data$X3_Adenokarsinoma <- factor(data$X3_Adenokarsinoma, levels = c("Tidak Adenokarsinoma", "Adenokarsinoma"))
data$X4_Komorbiditas <- factor(data$X4_Komorbiditas, levels = c("Tidak Ada Komorbiditas", "Ada Komorbiditas"))

# Membuat model propensity score menggunakan regresi logistik
propensity_model <- glm(T_event ~ X1_Usia + X2_Stadium + X3_Adenokarsinoma + X4_Komorbiditas, 
                        family = binomial(), data = data)

# Menambahkan kolom propensity score pada data
data$propensity_score <- predict(propensity_model, type = "response")

# Menghitung inverse probability of treatment weights (IPTW)
data$weight <- ifelse(data$T_event == 1, 1 / data$propensity_score, 
                      1 / (1 - data$propensity_score))

# Menyiapkan data untuk analisis Kaplan-Meier dengan pembobotan
surv_object <- Surv(time = data$T_time, event = data$T_event)

# Menggunakan weighted Kaplan-Meier untuk menghitung survival function
km_fit <- survfit(surv_object ~ 1, data = data, weights = data$weight)

# 1. Menampilkan hasil Kaplan-Meier dan plotnya
summary(km_fit)  # Menampilkan hasil statistik Kaplan-Meier
plot(km_fit, main = "Kurva Kaplan-Meier untuk Kelangsungan Hidup Kanker Paru",
     xlab = "Waktu (bulan)", ylab = "Probabilitas Kelangsungan Hidup", col = "blue")

# 2. Analisis per kelompok berdasarkan usia
km_fit_usia <- survfit(surv_object ~ X1_Usia, data = data, weights = data$weight)
plot(km_fit_usia, col = c("red", "green"), 
     main = "Kaplan-Meier Berdasarkan Usia", 
     xlab = "Waktu (bulan)", ylab = "Probabilitas Kelangsungan Hidup")
legend("topright", legend = levels(data$X1_Usia), col = c("red", "green"), lty = 1)

# 3. Analisis per kelompok berdasarkan stadium
km_fit_stadium <- survfit(surv_object ~ X2_Stadium, data = data, weights = data$weight)
plot(km_fit_stadium, col = c("purple", "orange"), 
     main = "Kaplan-Meier Berdasarkan Stadium", 
     xlab = "Waktu (bulan)", ylab = "Probabilitas Kelangsungan Hidup")
legend("topright", legend = levels(data$X2_Stadium), col = c("purple", "orange"), lty = 1)

# 4. Uji log-rank untuk perbandingan survival berdasarkan usia dan stadium
# Uji log-rank untuk usia
logrank_usia <- survdiff(surv_object ~ X1_Usia, data = data)
# Menampilkan Chi-Square dan p-value untuk usia
logrank_usia_chisq <- logrank_usia$chisq
logrank_usia_pvalue <- logrank_usia$pvalue
cat("Uji Log-Rank Usia - Chi-Square:", logrank_usia_chisq, "p-value:", logrank_usia_pvalue, "\n")

# Uji log-rank untuk stadium
logrank_stadium <- survdiff(surv_object ~ X2_Stadium, data = data)
# Menampilkan Chi-Square dan p-value untuk stadium
logrank_stadium_chisq <- logrank_stadium$chisq
logrank_stadium_pvalue <- logrank_stadium$pvalue
cat("Uji Log-Rank Stadium - Chi-Square:", logrank_stadium_chisq, "p-value:", logrank_stadium_pvalue, "\n")

# 5. Menyimpan hasil pembobotan IPTW dan propensity score
head(data$propensity_score)  # Menampilkan beberapa nilai propensity score
head(data$weight)  # Menampilkan beberapa nilai bobot IPTW

# 6. Tabel Kaplan-Meier
# Tabel Kaplan-Meier dengan waktu survival dan estimasi survival di setiap waktu
summary(km_fit)

# 7. Histogram distribusi bobot IPTW
hist(data$weight, main = "Distribusi Bobot IPTW", xlab = "Bobot IPTW", col = "lightblue")

# 8. Boxplot distribusi bobot IPTW
boxplot(data$weight, main = "Boxplot Bobot IPTW", ylab = "Bobot IPTW")

# Menyimpan hasil analisis dalam file CSV
write.csv(data, "data_with_weights.csv")
