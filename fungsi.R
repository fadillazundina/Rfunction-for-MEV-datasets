# Load Package
library(readxl)
library(dplyr)

# Fungsi untuk import data dari excel dengan memilih sheet
import_data_from_excel <- function() {
  # Untuk input path
  file_path <- readline(prompt = "Masukkan path yang ingin Anda import: ")
  
  # Untuk input sheet
  sheet_name <- readline(prompt = "Masukkan nama sheet yang ingin Anda import: ")
  
  # Input nama variabel
  variable_name <- readline(prompt = "Masukkan nama variabel untuk data ini: ")
  
  # Simpan nilai max.print saat ini
  current_max_print <- options("max.print")
  
  # Set nilai max.print agar mencetak seluruh baris data
  options(max.print = 99999)
  
  # Import data dari path dan sheet yang diinput
  tryCatch({
    data <- read_excel(file_path, sheet = sheet_name)
    
    # Konversi data menjadi time series
    time_series_data <- ts(data)  # Ubah indeks waktu sesuai dengan data Anda
    
    # Simpan time series data dalam variabel yang diinput oleh pengguna
    assign(variable_name, time_series_data, envir = .GlobalEnv)
    
    print(paste("Data dari sheet", sheet_name, "berhasil diimpor sebagai time series:", variable_name))
    print(get(variable_name))  # Menampilkan semua data
  }, error = function(e) {
    print(paste("Terjadi kesalahan:", e$message))
  })
}

# Memanggil fungsi
import_data_from_excel()

# Fungsi untuk transformasi
choose_transformation <- function(data) {
  transformation <- readline(prompt = "Pilih transformasi (1: Transformasi ke bentuk ln, 
                             2: Transformasi ke bentuk C, 
                             3: Transformasi ke bentuk diff, 
                             4: Tidak dilakukan transformasi): ")
  lag <- readline(prompt = "Pilih lag (1: lag1, 
                  2: lag2, 
                  3: lag3, 
                  4: lag4, 
                  5: Tidak dilakukan lag): ")
  
  if (transformation == "1") {
    data <- log(data)
  } else if (transformation == "2") {
    data <- (data - lag(data, 12)) / lag(data, 12)
  } else if (transformation == "3") {
    data <- diff(data, differences = 6)
  }
  
  if (lag == "1") {
    data <- data.frame(stats::lag(data, 3))
  } else if (lag == "2") {
    data <- data.frame(stats::lag(data, 6))
  } else if (lag == "3") {
    data <- data.frame(stats::lag(data, 9))
  } else if (lag == "4") {
    data <- data.frame(stats::lag(data, 12))
  }
  
  return(data)
}

# Fungsi untuk import, transformasi, dan display hasil transformasi
import_transform_and_display <- function() {
  # Input nama data yang akan ditransformasi
  data_variable_name <- readline(prompt = "Masukkan nama data yang akan ditransformasi: ")
  
  # Mendapatkan data dari data yang diinputkan
  data <- get(data_variable_name)
  
  # Menampilkan transformasi dan memanggil fungsi choose_transformation()
  transformed_data <- choose_transformation(data)
  print("Transformed Data:")
  print(transformed_data)
}

# Memanggil fungsi 
import_transform_and_display()


