### Importação, tratamento e análise de dados da regeneração natural
### pos queima no PARNA do Araguaia

# Desenvolvido com assistência de Inteligência Artificial
# Modelo: DeepSeek (deepseek.com)
# Data: 2025-10-15

# version: alfa 1.1

### packages

if (!require("purrr"))
  install.packages("purrr")
library("purrr")

if (!require("readxl"))
  install.packages("readxl")
library("readxl")

if (!require("data.table"))
  install.packages("data.table")
library("data.table")

if (!require("tidyverse"))
  install.packages("tidyverse")
library("tidyverse")

if (!require("stringr"))
  install.packages("stringr")
library("stringr")

if (!require("terra"))
  install.packages("terra")
library(terra)

if (!require("sf"))
  install.packages("sf")
library(sf)

### working directory to source file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### list and extract zipfiles

zipfiles <-
  list.files(
    path = setwd(dirname(
      rstudioapi::getActiveDocumentContext()$path
    )),
    pattern = "*.zip",
    recursive = T,
    full.names = TRUE
  )

purrr::map(.x = zipfiles, .f = unzip, exdir = "extracted")

rm(zipfiles)

# Load necessary library
library(readxl)

# List all .xlsx files in the directory
xlsx_files <- list.files(pattern = "\\.xlsx$", recursive = T, full.names = TRUE)

# Loop through each .xlsx file
for (xlsx_file in xlsx_files) {
  # Create the corresponding .csv filename
  csv_file <- sub("\\.xlsx$", ".csv", xlsx_file)
  
  # Check if the .csv file already exists
  if (!file.exists(csv_file)) {
    # Read the .xlsx file into a data frame
    data <- read_excel(xlsx_file)
    
    # Write the data frame to a .csv file
    write.csv(data, file = csv_file, row.names = FALSE)
    cat("Converted:", xlsx_file, "to", csv_file, "\n")
  } else {
    cat("CSV file already exists:", csv_file, "\n")
  }
}

rm(data, csv_file, xlsx_file, xlsx_files)

### Read and concatenate csv files

csvfiles <-
  list.files(
    path = setwd(dirname(
      rstudioapi::getActiveDocumentContext()$path
    )),
    pattern = "*.csv",
    recursive = T,
    full.names = TRUE
  ) %>%
  
  stringr::str_subset(., "reg_filtered.csv", negate = TRUE) %>%
  stringr::str_subset(., "reg_stat_joined_raster_classes", negate = TRUE)

  reg <-
  data.table::rbindlist(
    lapply(csvfiles, data.table::fread, colClasses = "character"),
    idcol = TRUE,
    fill = TRUE,
    use.names = TRUE
  )

# Check if '.id' exists in the "reg" data.table

if (!".id" %in% colnames(reg)) {
  # If '.id' doesn't exist, create it and assign NA
  reg[, .id := NA]
}

reg[, .id := factor(.id, labels = basename(csvfiles))]

data.table::setnames(reg, make.unique(names(reg)))

rm(csvfiles)

## rename attributes
setnames(reg, "queimado/queimado", "tratamento_queima")

## filter campaign period
reg[, `data_hora/data` := as.Date(`data_hora/data`)]
reg_filtered <- reg[`data_hora/data` %between% c(as.Date("2025-10-10"), as.Date("2025-10-15"))]

## reg_stat

reg_stat <- reg_filtered %>%
  group_by(
    .id,
    uc,
    `data_hora/data`,
    ua,
    `amostragem/ponto_inicio_transecto`,
    `amostragem/ponto_fim_transecto`,
    tratamento_queima,
    `queimado/observacoes_gerais`
  ) %>%
  separate_rows(`amostragem/registro/tipo_forma_vida`, sep = "\\s+") %>%
  filter(nzchar(`amostragem/registro/tipo_forma_vida`)) %>%
  dplyr::count(ua, `amostragem/registro/tipo_forma_vida`) %>%
  spread(`amostragem/registro/tipo_forma_vida`, n, fill = 0)

### join raster attributes to data.table

library(terra)
library(data.table)

# extract_raster_to_dt_final function
extract_raster_to_dt_final <- function(dt, coord_col = "amostragem/ponto_inicio_transecto", directory = ".", unzip_files = TRUE) {
  
  # Ensure dt is a data.table
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  
  # Create result data.table as a copy of original
  result_dt <- copy(dt)
  
  # Find all TIFF files
  tiff_files <- list.files(
    path = directory,
    pattern = "*.[tT][iI][fF]{1,2}$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  
  if (length(tiff_files) == 0) {
    return(result_dt)
  }
  
  # Parse coordinates: "latitude longitude altitude accuracy" -> (longitude, latitude)
  coord_strings <- dt[[coord_col]]
  long_vec <- numeric(length(coord_strings))
  lat_vec <- numeric(length(coord_strings))
  
  for (i in seq_along(coord_strings)) {
    parts <- strsplit(trimws(coord_strings[i]), "\\s+")[[1]]
    if (length(parts) >= 2) {
      # Format: "latitude longitude altitude accuracy"
      # Convert to GIS format: (longitude, latitude)
      lat_vec[i] <- as.numeric(parts[1])  # First value is latitude
      long_vec[i] <- as.numeric(parts[2]) # Second value is longitude
    } else {
      long_vec[i] <- NA
      lat_vec[i] <- NA
    }
  }
  
  # Valid coordinates for extraction
  valid_indices <- which(!is.na(long_vec) & !is.na(lat_vec))
  
  if (length(valid_indices) == 0) {
    return(result_dt)
  }
  
  # Create coordinate matrix in GIS format: (longitude, latitude)
  coords_matrix <- cbind(long_vec[valid_indices], lat_vec[valid_indices])
  
  # Initialize all raster columns with NA
  for (tiff_file in tiff_files) {
    layer_name <- tools::file_path_sans_ext(basename(tiff_file))
    result_dt[, (layer_name) := NA_real_]
  }
  
  # Extract values for valid coordinates
  for (i in seq_along(tiff_files)) {
    tiff_file <- tiff_files[i]
    layer_name <- tools::file_path_sans_ext(basename(tiff_file))
    
    tryCatch({
      raster_layer <- rast(tiff_file)
      raster_extent <- ext(raster_layer)
      
      # Check which points fall within this raster's extent
      points_in_extent <- (
        coords_matrix[,1] >= raster_extent$xmin &
          coords_matrix[,1] <= raster_extent$xmax &
          coords_matrix[,2] >= raster_extent$ymin &
          coords_matrix[,2] <= raster_extent$ymax
      )
      
      if (any(points_in_extent)) {
        points_within_indices <- valid_indices[points_in_extent]
        points_within_coords <- coords_matrix[points_in_extent, , drop = FALSE]
        
        points_vect <- vect(points_within_coords, crs = crs(raster_layer))
        extracted <- extract(raster_layer, points_vect)
        
        # Update only the points within extent
        set(result_dt, i = points_within_indices, j = layer_name, value = extracted[[2]])
      }
      
    }, error = function(e) {
      # continue if extraction fails
      NULL
    })
  }
  
  return(result_dt)
}

# extract_raster_to_dt_final
reg_stat_joined_raster_classes <- extract_raster_to_dt_final(reg_stat, "amostragem/ponto_inicio_transecto")

### kml/kmz export

library(sf)
library(data.table)

# KML creation with custom UA labels
create_kml_ua_labels <- function(dt, output_file = "transectos.kml") {
  
  # Load required libraries
  suppressPackageStartupMessages({
    library(sf)
    library(data.table)
  })
  
  # Coordinate parsing function
  parse_coord <- function(coord_string) {
    if (is.null(coord_string) || is.na(coord_string) || coord_string == "") {
      return(list(lat = NA, lon = NA))
    }
    tryCatch({
      parts <- strsplit(trimws(coord_string), "\\s+")[[1]]
      if (length(parts) >= 2) {
        lat <- as.numeric(parts[1])
        lon <- as.numeric(parts[2])
        if (!is.na(lat) && !is.na(lon) && lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180) {
          return(list(lat = lat, lon = lon))
        }
      }
      return(list(lat = NA, lon = NA))
    }, error = function(e) {
      return(list(lat = NA, lon = NA))
    })
  }
  
  coord_ini_col <- "amostragem/ponto_inicio_transecto"
  coord_end_col <- "amostragem/ponto_fim_transecto"
  
  # Start KML document
  kml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
<Document>
  <name>transectos</name>
  
  <!-- Styles for lines -->
  <Style id="yellowLine">
    <LineStyle>
      <color>7f00ffff</color>
      <width>4</width>
    </LineStyle>
  </Style>
  
  <!-- Style for initial points -->
  <Style id="startPoint">
    <IconStyle>
      <color>ff00ff00</color>
      <scale>1.2</scale>
      <Icon>
        <href>http://maps.google.com/mapfiles/kml/paddle/grn-circle.png</href>
      </Icon>
    </IconStyle>
    <LabelStyle>
      <scale>1.0</scale>
    </LabelStyle>
  </Style>
  
  <!-- Style for end points -->
  <Style id="endPoint">
    <IconStyle>
      <color>ffff0000</color>
      <scale>1.2</scale>
      <Icon>
        <href>http://maps.google.com/mapfiles/kml/paddle/red-circle.png</href>
      </Icon>
    </IconStyle>
    <LabelStyle>
      <scale>1.0</scale>
    </LabelStyle>
  </Style>'
  
  # Process each row
  for (i in seq_len(nrow(dt))) {
    ini <- parse_coord(dt[[coord_ini_col]][i])
    end <- parse_coord(dt[[coord_end_col]][i])
    
    if (!is.na(ini$lat) && !is.na(ini$lon) && !is.na(end$lat) && !is.na(end$lon)) {
      
      # Create description with all attributes
      description <- "<![CDATA[<table border='1' style='font-size: 12px;'>"
      for (col in names(dt)) {
        if (!col %in% c(coord_ini_col, coord_end_col)) {
          description <- paste0(description, 
                                "<tr><td><b>", col, "</b></td><td>", 
                                dt[i, get(col)], "</td></tr>")
        }
      }
      description <- paste0(description, "</table>]]>")
      
      # Get UA value or use default
      if ("ua" %in% names(dt)) {
        ua_value <- dt[i, ua]
        start_label <- paste0(ua_value, "_ini")
        end_label <- paste0(ua_value, "_end")
      } else {
        start_label <- paste0("start_", i)
        end_label <- paste0("end_", i)
      }
      
      # Add LINE
      line_coords <- paste0(ini$lon, ",", ini$lat, ",0 ", end$lon, ",", end$lat, ",0")
      kml_content <- paste0(kml_content, '
  <Placemark>
    <name>Linha ', i, ' (', start_label, ' - ', end_label, ')</name>
    <description>', description, '</description>
    <styleUrl>#yellowLine</styleUrl>
    <LineString>
      <tessellate>1</tessellate>
      <coordinates>', line_coords, '</coordinates>
    </LineString>
  </Placemark>')
      
      # Add START POINT with UA_ini label
      kml_content <- paste0(kml_content, '
  <Placemark>
    <name>', start_label, '</name>
    <description>Ponto inicial: ', start_label, '<br/>', description, '</description>
    <styleUrl>#startPoint</styleUrl>
    <Point>
      <coordinates>', ini$lon, ',', ini$lat, ',0</coordinates>
    </Point>
  </Placemark>')
      
      # Add END POINT with UA_end label
      kml_content <- paste0(kml_content, '
  <Placemark>
    <name>', end_label, '</name>
    <description>Ponto final: ', end_label, '<br/>', description, '</description>
    <styleUrl>#endPoint</styleUrl>
    <Point>
      <coordinates>', end$lon, ',', end$lat, ',0</coordinates>
    </Point>
  </Placemark>')
    }
  }
  
  # Close document
  kml_content <- paste0(kml_content, '
</Document>
</kml>')
  
  # Write file
  writeLines(kml_content, output_file)
  return(invisible(output_file))
}

# UA_ini and UA_end labels
create_kml_ua_labels(reg_stat_joined_raster_classes)

### Analysis

if (!require("corrplot"))
  install.packages("corrplot")
library("corrplot")

if (!require("data.table"))
  install.packages("data.table")
library("data.table")

if (!require("ggplot2"))
  install.packages("ggplot2")
library("ggplot2")

if (!require("corrplot"))
  install.packages("corrplot")
library("corrplot")

if (!require("psych"))
  install.packages("psych")
library("psych")

if (!require("dplyr"))
  install.packages("dplyr")
library("dplyr")

library(data.table)
library(ggplot2)
library(corrplot)
library(psych)

# # 1. BASIC DATA OVERVIEW WITH MISSING VALUE CHECK
# cat("=== BASIC DATA OVERVIEW ===\n")
# str(reg_stat_joined_raster_classes)
# 
# # Check missing values in key variables
# cat("\n=== MISSING VALUES IN KEY VARIABLES ===\n")
# missing_summary <- reg_stat_joined_raster_classes[, .(
#   lenhoso_na = sum(is.na(lenhoso)),
#   nao_lenhoso_na = sum(is.na(nao_lenhoso)),
#   solo_nu_na = sum(is.na(solo_nu)),
#   queimado_na = sum(is.na(tratamento_queima))
# )]
# print(missing_summary)
# 
# # Remove rows with missing vegetation cover data
# clean_data <- reg_stat_joined_raster_classes[!is.na(lenhoso) & !is.na(nao_lenhoso) & !is.na(solo_nu)]
# cat("\nRows after removing NA in vegetation cover:", nrow(clean_data), "\n")
# 
# # 2. Natural Forest Regeneration Classes DISTRIBUTION
# cat("\n=== Natural Forest Regeneration Classes DISTRIBUTION ===\n")
# burn_counts <- clean_data[, .N, by = tratamento_queima]
# print(burn_counts)
# 
# # 3. SUMMARY STATISTICS BY Natural Forest Regeneration Classes
# cat("\n=== VEGETATION COVER BY Natural Forest Regeneration Classes ===\n")
# cover_by_burn <- clean_data[, .(
#   n_obs = .N,
#   mean_lenhoso = mean(lenhoso, na.rm = TRUE),
#   mean_nao_lenhoso = mean(nao_lenhoso, na.rm = TRUE),
#   mean_solo_nu = mean(solo_nu, na.rm = TRUE),
#   sd_lenhoso = sd(lenhoso, na.rm = TRUE),
#   sd_nao_lenhoso = sd(nao_lenhoso, na.rm = TRUE),
#   sd_solo_nu = sd(solo_nu, na.rm = TRUE),
#   min_lenhoso = min(lenhoso, na.rm = TRUE),
#   max_lenhoso = max(lenhoso, na.rm = TRUE)
# ), by = tratamento_queima]
# 
# print(cover_by_burn)
# 
# # 4. VISUALIZATION - Distribution by Natural Forest Regeneration Classes
# # Boxplots for each vegetation cover type by Natural Forest Regeneration Classes
# p1 <- ggplot(clean_data, aes(x = tratamento_queima, y = lenhoso, fill = tratamento_queima)) +
#   geom_boxplot(alpha = 0.7) +
#   scale_fill_manual(values = c("amarelo" = "yellow", "laranja" = "orange", "verde" = "green")) +
#   labs(title = "Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Woody Cover (%)") +
#   theme_minimal()
# 
# p2 <- ggplot(clean_data, aes(x = tratamento_queima, y = nao_lenhoso, fill = tratamento_queima)) +
#   geom_boxplot(alpha = 0.7) +
#   scale_fill_manual(values = c("amarelo" = "yellow", "laranja" = "orange", "verde" = "green")) +
#   labs(title = "Non-Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Non-Woody Cover (%)") +
#   theme_minimal()
# 
# p3 <- ggplot(clean_data, aes(x = tratamento_queima, y = solo_nu, fill = tratamento_queima)) +
#   geom_boxplot(alpha = 0.7) +
#   scale_fill_manual(values = c("amarelo" = "yellow", "laranja" = "orange", "verde" = "green")) +
#   labs(title = "Bare Soil by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Bare Soil (%)") +
#   theme_minimal()
# 
# # Display plots
# print(p1)
# print(p2)
# print(p3)
# 
# # 5. CORRELATION ANALYSIS
# 
# # 6. EXPLORATORY ANALYSIS BY Natural Forest Regeneration Classes
# cat("\n=== ANALYSIS BY Natural Forest Regeneration Classes ===\n")
# burn_levels <- unique(clean_data$tratamento_queima)
# 
# for(burn in burn_levels) {
#   cat("\n--- Natural Forest Regeneration Classes:", burn, "---\n")
#   burn_data <- clean_data[tratamento_queima == burn]
#   
#   if(nrow(burn_data) > 3) {
#     # Basic statistics for this Natural Forest Regeneration Classes
#     burn_summary <- burn_data[, .(
#       n = .N,
#       mean_wood = mean(lenhoso),
#       mean_herb = mean(nao_lenhoso),
#       mean_soil = mean(solo_nu)
#     )]
#     print(burn_summary)
#     
#     # Check if we have enough data for correlations
#     if(nrow(burn_data) > 5) {
#       burn_numeric <- burn_data[, .SD, .SDcols = sapply(burn_data, is.numeric)]
#       burn_numeric_clean <- burn_numeric[, ..cols_to_keep]
#       burn_complete <- burn_numeric_clean[complete.cases(burn_numeric_clean)]
#       
#       if(nrow(burn_complete) > 3) {
#         # Simple correlation with top 3 variables
#         tryCatch({
#           burn_cor <- cor(burn_complete$lenhoso, 
#                           burn_complete[, !c("lenhoso", "nao_lenhoso", "solo_nu"), with = FALSE], 
#                           use = "complete.obs")
#           top_vars <- names(sort(abs(burn_cor[1, ]), decreasing = TRUE)[1:3])
#           cat("Top correlates with woody cover:", paste(top_vars, collapse = ", "), "\n")
#         }, error = function(e) cat("Not enough data for correlation in this group\n"))
#       }
#     }
#   }
# }
# 
# # 7. SCATTERPLOTS FOR KEY RELATIONSHIPS
# # Let's examine relationships with the most promising variables based on data availability
# potential_predictors <- c("ageYears_2024", "frequency_2024", "classification_2023")
# 
# for(predictor in potential_predictors) {
#   if(predictor %in% names(clean_data)) {
#     p <- ggplot(clean_data, aes_string(x = predictor, y = "lenhoso", color = "tratamento_queima")) +
#       scale_color_manual(values = c("amarelo" = "yellow", "laranja" = "orange", "verde" = "green")) +
#       geom_point(alpha = 0.6) +
#       geom_smooth(method = "lm", se = FALSE) +
#       labs(title = paste("Woody Cover vs", predictor),
#            x = predictor, y = "Woody Cover (%)") +
#       theme_minimal()
#     print(p)
#   }
# }
# 
# # 8. COMPOSITION ANALYSIS
# # Check if vegetation cover sums to approximately 100%
# clean_data[, total_cover := lenhoso + nao_lenhoso + solo_nu]
# cat("\n=== COVER COMPOSITION CHECK ===\n")
# summary(clean_data$total_cover)
# 
# # 9. SIMPLE LINEAR MODELS
# cat("\n=== SIMPLE REGRESSION MODELS ===\n")
# 
# # Model for woody cover
# if("ageYears_2024" %in% names(clean_data)) {
#   model_wood <- lm(lenhoso ~ tratamento_queima + ageYears_2024, data = clean_data)
#   cat("Model for Woody Cover:\n")
#   print(summary(model_wood))
# }
# 
# # 10. KEY INSIGHTS SUMMARY
# cat("\n=== KEY INSIGHTS ===\n")
# cat("1. Sample size by Natural Forest Regeneration Classes:\n")
# print(burn_counts)
# cat("\n2. Average cover patterns:\n")
# print(cover_by_burn[, .(Treatment = tratamento_queima, 
#                         Woody = round(mean_lenhoso, 1),
#                         Herbaceous = round(mean_nao_lenhoso, 1),
#                         Bare_Soil = round(mean_solo_nu, 1))])

# FIXED VERSION - MOVE COLS_TO_KEEP TO GLOBAL SCOPE
library(data.table)
library(ggplot2)
library(corrplot)

# 1. BASIC DATA OVERVIEW WITH MISSING VALUE CHECK
cat("=== BASIC DATA OVERVIEW ===\n")
str(reg_stat_joined_raster_classes)

# Check missing values in key variables
cat("\n=== MISSING VALUES IN KEY VARIABLES ===\n")
missing_summary <- reg_stat_joined_raster_classes[, .(
  lenhoso_na = sum(is.na(lenhoso)),
  nao_lenhoso_na = sum(is.na(nao_lenhoso)),
  solo_nu_na = sum(is.na(solo_nu)),
  queimado_na = sum(is.na(tratamento_queima))
)]
print(missing_summary)

# Remove rows with missing vegetation cover data
clean_data <- reg_stat_joined_raster_classes[!is.na(lenhoso) & !is.na(nao_lenhoso) & !is.na(solo_nu)]
cat("\nRows after removing NA in vegetation cover:", nrow(clean_data), "\n")

# 2. Natural Forest Regeneration Classes DISTRIBUTION
cat("\n=== Natural Forest Regeneration Classes DISTRIBUTION ===\n")
burn_counts <- clean_data[, .N, by = tratamento_queima]
print(burn_counts)

# 3. SUMMARY STATISTICS BY Natural Forest Regeneration Classes
cat("\n=== VEGETATION COVER BY Natural Forest Regeneration Classes ===\n")
cover_by_burn <- clean_data[, .(
  n_obs = .N,
  mean_lenhoso = mean(lenhoso, na.rm = TRUE),
  mean_nao_lenhoso = mean(nao_lenhoso, na.rm = TRUE),
  mean_solo_nu = mean(solo_nu, na.rm = TRUE),
  sd_lenhoso = sd(lenhoso, na.rm = TRUE),
  sd_nao_lenhoso = sd(nao_lenhoso, na.rm = TRUE),
  sd_solo_nu = sd(solo_nu, na.rm = TRUE),
  min_lenhoso = min(lenhoso, na.rm = TRUE),
  max_lenhoso = max(lenhoso, na.rm = TRUE)
), by = tratamento_queima]

print(cover_by_burn)

# 4. VISUALIZATION - Distribution by Natural Forest Regeneration Classes
# Boxplots for each vegetation cover type by Natural Forest Regeneration Classes
p1 <- ggplot(clean_data, 
             aes(x = factor(tratamento_queima, 
                            levels = c("amarelo", "laranja", "verde", "queimado")), 
                 y = lenhoso, 
                 fill = tratamento_queima)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
      values = c(
        "amarelo" = "yellow", 
        "laranja" = "orange", 
        "verde" = "green",
        "queimado" = "gray"
      ),
      breaks = c("amarelo", "laranja", "verde", "queimado"),
      labels = c(
        "amarelo" = "área pred. gramíneas",
        "laranja" = "área pred. gramíneas e arbustos", 
        "verde" = "área pred. plantas florestais",
        "queimado" = "área recém queimada"
      ),
      name = "Natural forest regeneration classes"
    ) +
  labs(title = "Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Woody Cover (%)") +
  theme_minimal() + 
  theme(axis.text.x = element_blank())

p2 <- ggplot(clean_data, aes(x = factor(tratamento_queima, 
                                        levels = c("amarelo", "laranja", "verde", "queimado")), 
                             y = lenhoso, 
                             fill = tratamento_queima)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "amarelo" = "yellow", 
      "laranja" = "orange", 
      "verde" = "green",
      "queimado" = "gray"
    ),
    breaks = c("amarelo", "laranja", "verde", "queimado"),
    labels = c(
      "amarelo" = "área pred. gramíneas",
      "laranja" = "área pred. gramíneas e arbustos", 
      "verde" = "área pred. plantas florestais",
      "queimado" = "área recém queimada"
    ),
    name = "Natural forest regeneration classes"
  ) +
  labs(title = "Non-Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Non-Woody Cover (%)") +
  theme_minimal()

p3 <- ggplot(clean_data, aes(x = factor(tratamento_queima, 
                                        levels = c("amarelo", "laranja", "verde", "queimado")), 
                             y = lenhoso, 
                             fill = tratamento_queima)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "amarelo" = "yellow", 
      "laranja" = "orange", 
      "verde" = "green",
      "queimado" = "gray"
    ),
    breaks = c("amarelo", "laranja", "verde", "queimado"),
    labels = c(
      "amarelo" = "área pred. gramíneas",
      "laranja" = "área pred. gramíneas e arbustos", 
      "verde" = "área pred. plantas florestais",
      "queimado" = "área recém queimada"
    ),
    name = "Natural forest regeneration classes"
  ) +
  labs(title = "Non-Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Non-Woody Cover (%)") +
  theme_minimal()

# Display plots
print(p1)
print(p2)
print(p3)

# 5. CORRELATION ANALYSIS WITH PROPER MISSING VALUE HANDLING
# Select only numeric variables and remove columns with too many NAs
numeric_vars <- clean_data[, .SD, .SDcols = sapply(clean_data, is.numeric)]

# Remove columns with all NAs or too many NAs - MAKE THIS GLOBAL
na_count <- numeric_vars[, lapply(.SD, function(x) sum(is.na(x)))]
cols_to_keep <- names(na_count)[na_count < nrow(numeric_vars) * 0.5]  # Keep cols with <50% NAs
numeric_vars_clean <- numeric_vars[, ..cols_to_keep]

# Remove rows with any NAs for correlation analysis
numeric_vars_complete <- numeric_vars_clean[complete.cases(numeric_vars_clean)]
cat("\nComplete cases for correlation:", nrow(numeric_vars_complete), "\n")

if(nrow(numeric_vars_complete) > 10) {
  # Correlation matrix focusing on vegetation cover
  cover_vars <- c("lenhoso", "nao_lenhoso", "solo_nu")
  
  # Ensure cover variables are present
  if(all(cover_vars %in% names(numeric_vars_complete))) {
    correlation_matrix <- cor(numeric_vars_complete[, ..cover_vars], 
                              numeric_vars_complete[, !..cover_vars], 
                              use = "complete.obs")
    
    print("=== CORRELATION WITH VEGETATION COVER ===")
    print(round(correlation_matrix, 3))
    
    # Visualize top correlations
    corrplot(correlation_matrix, method = "color", type = "full", 
             tl.cex = 0.7, tl.col = "black", 
             title = "Correlation with Vegetation Cover")
  }
}

# 6. EXPLORATORY ANALYSIS BY Natural Forest Regeneration Classes - FIXED VERSION
cat("\n=== ANALYSIS BY Natural Forest Regeneration Classes ===\n")
burn_levels <- unique(clean_data$tratamento_queima)

for(burn in burn_levels) {
  cat("\n--- Natural Forest Regeneration Classes:", burn, "---\n")
  burn_data <- clean_data[tratamento_queima == burn]
  
  if(nrow(burn_data) > 3) {
    # Basic statistics for this Natural Forest Regeneration Classes
    burn_summary <- burn_data[, .(
      n = .N,
      mean_wood = mean(lenhoso),
      mean_herb = mean(nao_lenhoso),
      mean_soil = mean(solo_nu)
    )]
    print(burn_summary)
    
    # Check if we have enough data for correlations
    if(nrow(burn_data) > 5) {
      burn_numeric <- burn_data[, .SD, .SDcols = sapply(burn_data, is.numeric)]
      
      # USE COLS_TO_KEEP FROM GLOBAL SCOPE (no .. prefix)
      available_cols <- cols_to_keep[cols_to_keep %in% names(burn_numeric)]
      burn_numeric_clean <- burn_numeric[, ..available_cols]
      
      burn_complete <- burn_numeric_clean[complete.cases(burn_numeric_clean)]
      
      if(nrow(burn_complete) > 3) {
        # Simple correlation with top 3 variables
        tryCatch({
          # Use direct column selection instead of .. syntax
          predictor_cols <- setdiff(names(burn_complete), cover_vars)
          if(length(predictor_cols) > 0) {
            burn_cor <- cor(burn_complete$lenhoso, 
                            burn_complete[, ..predictor_cols], 
                            use = "complete.obs")
            top_vars <- names(sort(abs(burn_cor[1, ]), decreasing = TRUE)[1:3])
            cat("Top correlates with woody cover:", paste(top_vars, collapse = ", "), "\n")
          }
        }, error = function(e) cat("Not enough data for correlation in this group:", e$message, "\n"))
      }
    }
  }
}

# 7. SCATTERPLOTS FOR KEY RELATIONSHIPS
potential_predictors <- c("ageYears_2024", "frequency_2024", "classification_2023")

for(predictor in potential_predictors) {
  if(predictor %in% names(clean_data)) {
    p <- ggplot(clean_data, aes(x = .data[[predictor]], y = lenhoso, color = tratamento_queima)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(
        values = c(
          "amarelo" = "yellow", 
          "laranja" = "orange", 
          "verde" = "green",
          "queimado" = "gray"
        ),
        breaks = c("amarelo", "laranja", "verde","queimado"),  # This controls legend order
        labels = c(
          "amarelo" = "área pred. gramíneas", 
          "laranja" = "área pred. gramíneas e arbustos",
          "verde" = "área pred. plantas florestais",
          "queimado" = "área recém queimada"
        ),
        name = "Natural forest regeneration classes"
      ) +
      labs(title = paste("Woody Cover vs", predictor),
           x = predictor, y = "Woody Cover (%)") +
      theme_minimal()
    print(p)
  }
}

# 8. COMPOSITION ANALYSIS
# Check if vegetation cover sums to approximately 100%
clean_data[, total_cover := lenhoso + nao_lenhoso + solo_nu]
cat("\n=== COVER COMPOSITION CHECK ===\n")
summary(clean_data$total_cover)

# 9. SIMPLE LINEAR MODELS
cat("\n=== SIMPLE REGRESSION MODELS ===\n")

# Model for woody cover
if("ageYears_2024" %in% names(clean_data)) {
  model_wood <- lm(lenhoso ~ tratamento_queima + ageYears_2024, data = clean_data)
  cat("Model for Woody Cover:\n")
  print(summary(model_wood))
}

# 10. KEY INSIGHTS SUMMARY
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Sample size by Natural Forest Regeneration Classes:\n")
print(burn_counts)
cat("\n2. Average cover patterns:\n")
print(cover_by_burn[, .(Treatment = `tratamento_queima`, 
                        Woody = round(mean_lenhoso, 1),
                        Herbaceous = round(mean_nao_lenhoso, 1),
                        Bare_Soil = round(mean_solo_nu, 1))])

### export csv

if (exists("reg_filtered"))
  fwrite(reg_filtered,
         file.path("reg_filtered.csv"),
         row.names = FALSE)

if (exists("reg_stat_joined_raster_classes"))
  fwrite(reg_stat_joined_raster_classes,
         file.path("reg_stat_joined_raster_classes.csv"),
         row.names = FALSE)

### Batch Export All Analysis Plots

# Create directory for plots
if(!dir.exists("analysis_plots")) {
  dir.create("analysis_plots")
}

# Function to save plot to directory
save_plot <- function(plot, name) {
  filename <- file.path("analysis_plots", paste0(name, ".png"))
  ggsave(filename, 
         plot = plot, 
         width = 10, 
         height = 8, 
         dpi = 300, 
         bg = "white")
  return(filename)
}

# Save all main plots
plot_files <- c()

# Boxplots
p1 <- ggplot(clean_data, aes(x = tratamento_queima, y = lenhoso, fill = tratamento_queima)) +
  geom_boxplot(alpha = 0.7) +
  scale_color_manual(
    values = c(
      "amarelo" = "yellow", 
      "laranja" = "orange", 
      "verde" = "green",
      "queimado" = "gray"
    ),
    breaks = c("amarelo", "laranja", "verde","queimado"),  # This controls legend order
    labels = c(
      "amarelo" = "área pred. gramíneas", 
      "laranja" = "área pred. gramíneas e arbustos",
      "verde" = "área pred. plantas florestais",
      "queimado" = "área recém queimada"
    ),
    name = "Natural forest regeneration classes"
  ) +
  labs(title = "Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Woody Cover (%)") +
  theme_minimal()

plot_files[1] <- save_plot(p1, "01_woody_cover_boxplot")

p2 <- ggplot(clean_data, aes(x = tratamento_queima, y = nao_lenhoso, fill = tratamento_queima)) +
  geom_boxplot(alpha = 0.7) +
  scale_color_manual(
    values = c(
      "amarelo" = "yellow", 
      "laranja" = "orange", 
      "verde" = "green",
      "queimado" = "gray"
    ),
    breaks = c("amarelo", "laranja", "verde","queimado"),  # This controls legend order
    labels = c(
      "amarelo" = "área pred. gramíneas", 
      "laranja" = "área pred. gramíneas e arbustos",
      "verde" = "área pred. plantas florestais",
      "queimado" = "área recém queimada"
    ),
    name = "Natural forest regeneration classes"
  ) +
  labs(title = "Non-Woody Cover by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Non-Woody Cover (%)") +
  theme_minimal()

plot_files[2] <- save_plot(p2, "02_non_woody_cover_boxplot")

p3 <- ggplot(clean_data, aes(x = tratamento_queima, y = solo_nu, fill = tratamento_queima)) +
  geom_boxplot(alpha = 0.7) +
  scale_color_manual(
    values = c(
      "amarelo" = "yellow", 
      "laranja" = "orange", 
      "verde" = "green",
      "queimado" = "gray"
    ),
    breaks = c("amarelo", "laranja", "verde","queimado"),  # This controls legend order
    labels = c(
      "amarelo" = "área pred. gramíneas", 
      "laranja" = "área pred. gramíneas e arbustos",
      "verde" = "área pred. plantas florestais",
      "queimado" = "área recém queimada"
    ),
    name = "Natural forest regeneration classes"
  ) +
  labs(title = "Bare Soil by Natural Forest Regeneration Classes", x = "Natural Forest Regeneration Classes", y = "Bare Soil (%)") +
  theme_minimal()

plot_files[3] <- save_plot(p3, "03_bare_soil_boxplot")

# Scatterplots
predictors <- c("ageYears_2024", "frequency_2024")
for(i in seq_along(predictors)) {
  if(predictors[i] %in% names(clean_data)) {
    p <- ggplot(clean_data, aes_string(x = predictors[i], y = "lenhoso", color = "tratamento_queima")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(
        values = c(
          "amarelo" = "yellow", 
          "laranja" = "orange", 
          "verde" = "green",
          "queimado" = "gray"
        ),
        breaks = c("amarelo", "laranja", "verde","queimado"),  # This controls legend order
        labels = c(
          "amarelo" = "área pred. gramíneas", 
          "laranja" = "área pred. gramíneas e arbustos",
          "verde" = "área pred. plantas florestais",
          "queimado" = "área recém queimada"
        ),
        name = "Natural forest regeneration classes"
      ) +
      labs(title = paste("Woody Cover vs", predictors[i]),
           x = predictors[i], y = "Woody Cover (%)") +
      theme_minimal()
    
    plot_files[3 + i] <- save_plot(p, paste0("04_scatter_", predictors[i]))
  }
}

cat("\n📁 All plots saved in 'analysis_plots' directory:\n")
for(file in plot_files) {
  if(!is.na(file)) cat("•", file, "\n")
}