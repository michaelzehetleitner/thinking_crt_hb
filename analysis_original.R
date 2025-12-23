# 📌 1. Bibliotheken laden
library(dplyr)

# 📌 2. Datensatz laden
dummy <- read.csv("Datensatz_Qualtrics.csv", stringsAsFactors = FALSE)

# 📌 3. Erste zwei Zeilen entfernen
dummy <- dummy[-c(1, 2), ]

# 📌 4. Spaltennamen bereinigen (entfernt "." am Ende)
colnames(dummy) <- gsub("\\.$", "", colnames(dummy))

# 📌 5. Sicherstellen, dass EW, Check und Finished numerisch sind
dummy$EW <- as.numeric(dummy$EW)
dummy$Check <- as.numeric(dummy$Check)
dummy$Finished <- as.numeric(dummy$Finished)
# 📌 Alter als sicher numerisch umwandeln
dummy$Alter <- as.numeric(dummy$Alter)

# 📌 Nur Personen behalten, die älter als 18 sind
dummy <- dummy %>%
  filter(is.na(Alter) | Alter >= 18)

# 📌 6. Zeilen entfernen, die bestimmte Bedingungen nicht erfüllen
dummy <- dummy %>%
  filter(EW != 0, Check != 0, Finished == 1)

# 📌 7. Liste der Kategorien mit Anzahl der Items
categories <- list(
  AOT = list(num_items = 6, suffix = ""), 
  PET = list(num_items = 6, suffix = ""), 
  PIT = list(num_items = 6, suffix = ""),
  CMT = list(num_items = 6, suffix = ""),
  PB  = list(num_items = 6, suffix = ""),
  EMP = list(num_items = 14, suffix = ""),
  VA  = list(num_items = 5, suffix = ""),
  HB  = list(num_items = 3, suffix = "")
)

# 📌 8. Sichere numerische Umwandlung
safe_as_numeric <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub(",", ".", x)
  x <- gsub("[^0-9.-]", "", x)
  as.numeric(x)
}

# 📌 9. Berechnung der _Sum und _Mean Spalten für jede Kategorie
for (cat in names(categories)) {
  num_items <- categories[[cat]]$num_items
  suffix <- categories[[cat]]$suffix
  
  relevant_columns <- paste0(cat, "_", 1:num_items, suffix)
  relevant_columns <- relevant_columns[relevant_columns %in% colnames(dummy)]
  
  if (length(relevant_columns) == 0) {
    message(paste("⚠️ Warnung: Keine passenden Spalten für", cat, "gefunden!"))
    next
  }
  
  dummy[, relevant_columns] <- lapply(dummy[, relevant_columns], safe_as_numeric)
  
  dummy[[paste0(cat, "_Sum")]] <- rowSums(dummy[, relevant_columns], na.rm = TRUE)
  dummy[[paste0(cat, "_Mean")]] <- dummy[[paste0(cat, "_Sum")]] / num_items
}

# 📌 10. Gewichtete Mittelwertsberechnung für RB_Mean
rb_columns <- c("RB_1", "RB_2", "RB_3", "RB_4", "RB_5")
rb_weights <- c(5, 5, 5, 6, 7)

existing_rb_columns <- rb_columns[rb_columns %in% colnames(dummy)]

if (length(existing_rb_columns) > 0) {
  dummy[, existing_rb_columns] <- lapply(dummy[, existing_rb_columns], as.numeric)
  
  for (i in seq_along(existing_rb_columns)) {
    col <- existing_rb_columns[i]
    dummy[[col]] <- dummy[[col]] / rb_weights[i]
  }
  
  dummy$RB_Mean <- rowMeans(dummy[, existing_rb_columns], na.rm = TRUE)
}

# 📌 11. CRT-Kodierung mit separaten numerischen Spalten
crt_columns <- c("CRT_1", "CRT_2", "CRT_3", "CRT_4", "CRT_5", "CRT_6", "CRT_7")

existing_crt_columns <- crt_columns[crt_columns %in% colnames(dummy)]

if (length(existing_crt_columns) > 0) {
  dummy[, existing_crt_columns] <- lapply(dummy[, existing_crt_columns], as.character)
  
  dummy$CRT_1_num <- ifelse(dummy$CRT_1 == "4", 1, 0)
  dummy$CRT_2_num <- ifelse(dummy$CRT_2 == "10", 1, 0)
  dummy$CRT_3_num <- ifelse(dummy$CRT_3 == "39", 1, 0)
  dummy$CRT_4_num <- ifelse(dummy$CRT_4 == "2", 1, 0)
  dummy$CRT_5_num <- ifelse(dummy$CRT_5 == "8", 1, 0)
  dummy$CRT_6_num <- ifelse(tolower(dummy$CRT_6) == "emily", 1, 0)
  
  dummy$CRT_7 <- as.numeric(dummy$CRT_7)
  
  crt_numeric_columns <- c("CRT_1_num", "CRT_2_num", "CRT_3_num", "CRT_4_num", "CRT_5_num", "CRT_6_num")
  dummy$CRT_Sum <- rowSums(dummy[, crt_numeric_columns], na.rm = TRUE)
  dummy$CRT_Mean <- dummy$CRT_Sum / length(crt_numeric_columns)
  dummy$CRT_Einsch <- dummy$CRT_Sum - dummy$CRT_7
}

# 📌 12. Berechnung der MV-Werte
mv_columns <- c("MV_1_Falsch", "MV_2_Falsch", "MV_1_Ekel", "MV_2_Ekel")

existing_mv_columns <- mv_columns[mv_columns %in% colnames(dummy)]

if (length(existing_mv_columns) > 0) {
  dummy[, existing_mv_columns] <- lapply(dummy[, existing_mv_columns], safe_as_numeric)
  
  if (all(c("MV_1_Falsch", "MV_2_Falsch") %in% colnames(dummy))) {
    dummy$MV_F_Mean <- rowMeans(dummy[, c("MV_1_Falsch", "MV_2_Falsch")], na.rm = TRUE)
  }
  
  if (all(c("MV_1_Ekel", "MV_2_Ekel") %in% colnames(dummy))) {
    dummy$MV_E_Mean <- rowMeans(dummy[, c("MV_1_Ekel", "MV_2_Ekel")], na.rm = TRUE)
  }
  
  if ("MV_F_Mean" %in% colnames(dummy) & "MV_E_Mean" %in% colnames(dummy)) {
    dummy$MV_Dif <- dummy$MV_F_Mean - dummy$MV_E_Mean
  }
}

# 📌 13. Z-Transformation aller _Mean-Spalten + CRT_Einsch
mean_columns <- grep("_Mean$", colnames(dummy), value = TRUE)

if (length(mean_columns) > 0) {
  for (col in mean_columns) {
    mean_value <- mean(dummy[[col]], na.rm = TRUE)
    sd_value <- sd(dummy[[col]], na.rm = TRUE)
    
    if (sd_value > 0) { 
      dummy[[paste0(col, "_z")]] <- (dummy[[col]] - mean_value) / sd_value
    } else {
      dummy[[paste0(col, "_z")]] <- NA  
      message(paste("⚠️ Achtung: Keine Varianz für", col, "- Z-Transformation nicht möglich"))
    }
  }
}

# 📌 Z-Transformation für CRT_Einsch
mean_value <- mean(dummy$CRT_Einsch, na.rm = TRUE)
sd_value <- sd(dummy$CRT_Einsch, na.rm = TRUE)

if (sd_value > 0) {
  dummy$CRT_Einsch_z <- (dummy$CRT_Einsch - mean_value) / sd_value
} else {
  dummy$CRT_Einsch_z <- NA
  message("⚠️ Achtung: Keine Varianz für CRT_Einsch - Z-Transformation nicht möglich")
}



# 📌 14. Bereinigten Datensatz speichern
write.csv(dummy, "Datensatz_Updated.csv", row.names = FALSE)

# 📌 15. Überprüfung der Änderungen
str(dummy)  
head(dummy)  

