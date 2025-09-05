# survival_analysis_project
Proyek analisis survival kanker paru menggunakan metode IPTW dan Kaplan-Meier
# Survival Analysis Project

Ini adalah proyek analisis survival kanker paru menggunakan metode inverse probability of treatment weights (IPTW) dan Kaplan-Meier.

## Struktur File
- `survival_analysis.R`: Sintaks R untuk analisis survival.
- `data_simulasi_survival_kanker_paru_R.xlsx`: Data yang digunakan untuk analisis survival.

## Langkah-langkah Analisis
1. Membaca data dari file Excel.
2. Mengonversi variabel kategori menjadi faktor.
3. Membuat model propensity score dan menghitung bobot IPTW.
4. Menggunakan analisis Kaplan-Meier untuk menghitung fungsi kelangsungan hidup.
