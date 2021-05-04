setwd ("C:/Users/Kyle/Dropbox/Research/Development and Diffusion")
library(foreign)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(ggdendro)
library(dplyr)
library(stats)
require(graphics)
library(xtable)
library(apsrtable)
library(gplots)
library(vegan)
library(fields)
library(ecodist)
library(ncf)
library(reshape2)
library(stargazer)

##############################################################################################################
# Actual code (w/ trade analysis)
##############################################################################################################
# Compare distance matrices for growth patterns (Maddison) and language (Ruhlen)
##############################################################################################################

# Clear R environment
rm(list=ls())

# Choose ONE of the following versions of the Maddison growth pattern data:
mad <- read.csv("./input_data/maddison_subset4a.csv") # 1950-2000, 138 countries, 10-year CAGR
mad <- read.csv("./input_data/maddison_subset5.csv") # 1950-2008, 137 countries, 5-year CAGR
mad <- read.csv("./input_data/maddison_subset6.csv") # random (check on Mantel test)
mad <- read.csv("./input_data/maddison_subset7.csv") # first half of countries are randomized (check on Mantel test)

# By region
mad <- read.csv("./input_data/maddison_subset_europe.csv") # 1950-2008, 23 countries, 5-year CAGR, Europe only
mad <- read.csv("./input_data/maddison_subset_africa.csv") # 1950-2008, 48 countries, 5-year CAGR, Sub-Saharan Africa only
mad <- read.csv("./input_data/maddison_subset_eastasia.csv") # 1950-2008, 15 countries, 5-year CAGR, East Asia only

mad <- read.csv("./input_data/maddison_subset_exeurope.csv") # 1950-2008, 115 countries, 5-year CAGR, excluding Europe

# Get linguistic data from Ruhlen
ruhlen <- read.csv("./input_data/ruhlen.csv")
ruhlen <- rename(ruhlen, country_code = Country..ISO.Alpha.3.Code.) # Clean up column name for country code

# Get most spoken language for each country
ruhlen2 <- ruhlen %>%
  group_by(country_code) %>%
  arrange(desc(Population)) %>%
  slice(1) %>% # In case there's more than one max language, just take the first one
  ungroup

## Dealing with missing languages
# The real dominant languages of certain countries are missing
# E.g. For Australia, there's no Australian English and so the dominant language is (incorrectly) a small aboriginal language
# In this section, I manually substitute the phonemic data for some countries' languages with others that make sense
# E.g. Substitute Australia with the phonemic data from Britain's English

ruhlen2[which(ruhlen2$country_code == "AFG"), 12:739] <- ruhlen[which(ruhlen$ISO_A3 == "pbt-AFG"), 12:739] # Pashto (not Farsi) is the largest language in Afghanistan
ruhlen2[which(ruhlen2$country_code == "AUS"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "GBR"), 12:739] # Australia <- Britain
ruhlen2[which(ruhlen2$country_code == "ARG"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Argentina <- Colombia
ruhlen2[which(ruhlen2$country_code == "BOL"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Bolivia <- Colombia
ruhlen2[which(ruhlen2$country_code == "BWA"), 12:739] <- ruhlen[which(ruhlen$ISO_A3 == "tsn-ZAF"), 12:739] # Tswana is main language of Botswana (but categorized as a language of South Africa)
ruhlen2[which(ruhlen2$country_code == "CHE"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "DEU"), 12:739] # Switzerland <- Germany
ruhlen2[which(ruhlen2$country_code == "CHL"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Chile <- Colombia
ruhlen2[which(ruhlen2$country_code == "GTM"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # Guatemala <- Costa Rica
ruhlen2[which(ruhlen2$country_code == "HND"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # Honduras <- Costa Rica
ruhlen2[which(ruhlen2$country_code == "NIC"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # Nicaragua <- Costa Rica
ruhlen2[which(ruhlen2$country_code == "NZL"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "GBR"), 12:739] # New Zealand <- Britain
ruhlen2[which(ruhlen2$country_code == "PER"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Peru <- Colombia
ruhlen2[which(ruhlen2$country_code == "PRY"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Paraguay <- Colombia
ruhlen2[which(ruhlen2$country_code == "SDN"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "EGY"), 12:739] # Sudan <- Egypt
ruhlen2[which(ruhlen2$country_code == "SLV"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # El Salvador <- Costa Rica
ruhlen2[which(ruhlen2$country_code == "VEN"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Venezuela <- Colombia

# Get geograhic data for each country (longitude and latitude)
geo <- read.csv("./input_data/geo.csv")

# Get 2008 levels of income for each country
mad08 <- read.csv("./input_data/maddison_subset_2008gdp.csv")


##############################################################################################################
# Trade data
##############################################################################################################

# Get trade distance matrix -- this can be skipped if you use the direct input method below
trade <- read.dta("input_data/wtf_bilat.dta")

# Remove Fm Yemen Ar
trade <- filter(trade, importer != "Fm Yemen Ar", exporter != "Fm Yemen Ar")
trade <- filter(trade, importer != "Fm Yemen AR", exporter != "Fm Yemen AR")
trade <- filter(trade, importer != "Fm Yemen Dm", exporter != "Fm Yemen Dm")
trade <- filter(trade, importer != "Fm German FR", exporter != "Fm German FR")
trade <- filter(trade, importer != "Fm German DR", exporter != "Fm German DR")

pick.years <- which( colnames(trade)=="value91" ):which( colnames(trade)=="value00")  # Choose which years of trade data to average over (currently set at 1991-2000)
trade$value <- rowMeans(trade[ , pick.years], na.rm=TRUE) # Create average value
write.csv(as.matrix(trade), "./output_tables/trade.csv") # Write to Excel to spot check

by.importer <- group_by(trade, importer)
trade.importer <- summarise(by.importer, total = sum(value, na.rm = TRUE))

by.exporter <- group_by(trade, exporter)
trade.exporter <- summarise(by.exporter, total = sum(value, na.rm = TRUE))

n <- nrow(trade.importer)

import.dist.matrix <- matrix(nrow = n, ncol = n)
rownames(import.dist.matrix) <- trade.importer$importer
colnames(import.dist.matrix) <- trade.importer$importer

# Create one-sided import matrix: each row is an importer, each column is an exporter to that importer
# (takes 5 min to run these FOR loops so can just use next line to skip)
for(j in 1:n){
  for(i in 1:n){
    import.dist.matrix[i, j] <- as.numeric(
      trade %>%
        filter(importer == rownames(import.dist.matrix)[i], exporter == colnames(import.dist.matrix)[j]) %>%
        select(value)
    )
    print(c(j, i))  
  }
}

# Create symmetric trade distance matrix by summing imports and exports for each pair of countries
import.dist.matrix <- round(import.dist.matrix)
mode(import.dist.matrix) <- "numeric"
trade.dist.matrix <- import.dist.matrix + t(import.dist.matrix)

write.csv(as.matrix(import.dist.matrix), "./output_tables/import_distance_matrix.csv")
write.csv(as.matrix(trade.dist.matrix), "./output_tables/trade_distance_matrix.csv")

country_codes <- read.csv("input_data/country_codes.csv")

trade.dist.matrix <- as.data.frame(trade.dist.matrix)
trade.dist.matrix$country_code <- country_codes$country_code

# Trade distance matrix - direct input method (i.e. data is already cleaned up in Excel)
trade.dist.matrix <- read.csv("./input_data/trade_distance_matrix.csv")
colnames(trade.dist.matrix)[1] <- "country_code"

# Harmonize all datasets by selecting country rows that they all have
full <- merge(ruhlen2, mad, by = "country_code")
full <- merge(full, geo, by = "country_code")
full <- merge(full, trade.dist.matrix, by = "country_code") ############ Trade
full <- select(full, country_code)
ruhlen2 <- merge(full, ruhlen2, by = 'country_code')
mad2 <- merge(full, mad, by = 'country_code')
geo2 <- merge(full, geo, by = 'country_code')
mad08 <- merge(full, mad08, by = 'country_code')
trade.dist.matrix <- merge(full, trade.dist.matrix, by = 'country_code') ############ Trade

identical(as.vector(mad2$country_code), as.vector(ruhlen2$country_code)) # Check to make sure country codes match up exactly
identical(as.vector(geo2$country_code), as.vector(ruhlen2$country_code)) # Check to make sure country codes match up exactly
identical(as.vector(trade.dist.matrix$country_code), as.vector(ruhlen2$country_code)) # Check to make sure country codes match up exactly

# Fix trade country code labels
trade.dist.matrix <- t(trade.dist.matrix)
colnames(trade.dist.matrix) <- trade.dist.matrix[1, ]
trade.dist.matrix <- trade.dist.matrix[-1, ]
trade.dist.matrix <- as.data.frame(trade.dist.matrix)
trade.dist.matrix$country_code <- country_codes$country_code
trade.dist.matrix <- merge(full, trade.dist.matrix, by = 'country_code')
trade.dist.matrix <- t(trade.dist.matrix)
colnames(trade.dist.matrix) <- trade.dist.matrix[1, ]
trade.dist.matrix <- trade.dist.matrix[-1, ]
mode(trade.dist.matrix) <- "numeric"

# Note: for some reason, the following countries have trade with themselves: Australia, Denmark, France, Indonesia, South Africa, Taiwan
# Thus, make the diagonals (i.e. same countries) NAs
diag(trade.dist.matrix) <- NA

# Create growth distance matrix based on Maddison income data using Euclidean distance
mad3 <- mad2[, 3:ncol(mad2)] # Extract the growth numbers
rownames(mad3) <- mad2$country_code # Rename rows using country codes
mad3 <- t(scale(t(mad3))) # Standardize growth rates within each country so that mean = 0, SD = 1
growth.dist.matrix <- vegdist(mad3, "euclidean")

# Create language distance matrix for Ruhlen linguistic data using Jaccard distance
ruhlen3 <- ruhlen2[, 12:ncol(ruhlen2)] # Extract phoneme presence/absence for all 728 phonemes
rownames(ruhlen3) <- ruhlen2$country_code # Rename rows to ISO country codes
lang.dist.matrix <- vegdist(ruhlen3, "jaccard", binary = TRUE)

# Create geographic distance matrix for geographic data using great circle distance
geo3 <- geo2[ , 2:3]
geo.dist.matrix <- rdist.earth(geo3, geo3)
rownames(geo.dist.matrix) <- geo2[, 1]
colnames(geo.dist.matrix) <- geo2[, 1]
geo.dist.matrix <- as.dist(geo.dist.matrix)

# Create distance matrix for 2008 levels of income
mad08_2 <- as.matrix(log(mad08$X2008)) # Extract the income numbers and log them
rownames(mad08_2) <- mad08$country_code # Rename rows using country codes
mad08.dist.matrix <- vegdist(mad08_2, "euclidean")

##############################################################################################################
# Mantel tests
##############################################################################################################
# Use ecodist package: faster and allows for multiple controls when doing partial Mantel

# Mantel tests
vegan::mantel(growth.dist.matrix, lang.dist.matrix, permutations = 99999)
mantel.partial(growth.dist.matrix, lang.dist.matrix, geo.dist.matrix, permutations = 99999)


# Extra Mantel tests
ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix, nperm = 99999)
ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix + geo.dist.matrix, nperm = 99999)
ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix + mad08.dist.matrix, nperm = 99999)
ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix + geo.dist.matrix + mad08.dist.matrix, nperm = 99999)

ncf::mantel.test(as.matrix(growth.dist.matrix), as.matrix(lang.dist.matrix), quiet=TRUE)
ncf::partial.mantel.test(as.matrix(growth.dist.matrix), as.matrix(lang.dist.matrix), as.matrix(trade.dist.matrix),  quiet=TRUE)

ncf::mantel.test(as.matrix(lang.dist.matrix), as.matrix(trade.dist.matrix), resamp = 9999)

ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix + geo.dist.matrix, nperm = 99999)
ncf::partial.mantel.test(as.matrix(growth.dist.matrix), as.matrix(lang.dist.matrix), as.matrix(geo.dist.matrix),  quiet=TRUE)


ecodist::mantel(mad08.dist.matrix ~ lang.dist.matrix, nperm = 99999)
ecodist::mantel(mad08.dist.matrix ~ lang.dist.matrix + geo.dist.matrix, nperm = 99999)
ecodist::mantel(mad08.dist.matrix ~ geo.dist.matrix + lang.dist.matrix, nperm = 99999)
ecodist::mantel(mad08.dist.matrix ~ geo.dist.matrix, nperm = 99999)

mantel.partial(geo.dist.matrix, lang.dist.matrix, growth.dist.matrix, permutations = 9999)
mantel.partial(lang.dist.matrix, geo.dist.matrix, growth.dist.matrix, permutations = 9999)

# Create distance matrix tables for Excel
write.csv(as.matrix(growth.dist.matrix), "./output_tables/growth_distance_matrix.csv")
write.csv(as.matrix(lang.dist.matrix), "./output_tables/language_distance_matrix.csv")
write.csv(as.matrix(geo.dist.matrix), "./output_tables/geographic_distance_matrix.csv")
write.csv(mad3, "./output_tables/growth_standardized.csv")
write.csv(as.matrix(mad08.dist.matrix), "./output_tables/mad08_distance_matrix.csv")





##############################################################################################################
# Actual code (without trade analysis)
##############################################################################################################
# Compare distance matrices for growth patterns (Maddison) and language (Ruhlen)
##############################################################################################################

# Clear R environment
rm(list=ls())

######################################################################################################################
# Function for creating distance matrices
# Takes argument "mad": set of Maddison growth pattern data
# Returns list of distance matrices: 1) growth, 2) language, 3) geo, 4) 2008 income levels
######################################################################################################################

create.dist.matrices <- function (mad) {
  
  # Get linguistic data from Ruhlen
  ruhlen <- read.csv("./input_data/ruhlen.csv")
  ruhlen <- rename(ruhlen, country_code = Country..ISO.Alpha.3.Code.) # Clean up column name for country code
  
  # Get most spoken language for each country
  ruhlen2 <- ruhlen %>%
    group_by(country_code) %>%
    arrange(desc(Population)) %>%
    slice(1) %>% # In case there's more than one max language, just take the first one
    ungroup
  
  ## Dealing with missing languages
  # The real dominant languages of certain countries are missing
  # E.g. For Australia, there's no Australian English and so the dominant language is (incorrectly) a small aboriginal language
  # In this section, I manually substitute the phonemic data for some countries' languages with others that make sense
  # E.g. Substitute Australia with the phonemic data from Britain's English
  
  ruhlen2[which(ruhlen2$country_code == "AFG"), 12:739] <- ruhlen[which(ruhlen$ISO_A3 == "pbt-AFG"), 12:739] # Pashto (not Farsi) is the largest language in Afghanistan
  ruhlen2[which(ruhlen2$country_code == "AUS"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "GBR"), 12:739] # Australia <- Britain
  ruhlen2[which(ruhlen2$country_code == "ARG"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Argentina <- Colombia
  ruhlen2[which(ruhlen2$country_code == "BOL"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Bolivia <- Colombia
  ruhlen2[which(ruhlen2$country_code == "BWA"), 12:739] <- ruhlen[which(ruhlen$ISO_A3 == "tsn-ZAF"), 12:739] # Tswana is main language of Botswana (but categorized as a language of South Africa)
  ruhlen2[which(ruhlen2$country_code == "CHE"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "DEU"), 12:739] # Switzerland <- Germany
  ruhlen2[which(ruhlen2$country_code == "CHL"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Chile <- Colombia
  ruhlen2[which(ruhlen2$country_code == "GTM"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # Guatemala <- Costa Rica
  ruhlen2[which(ruhlen2$country_code == "HND"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # Honduras <- Costa Rica
  ruhlen2[which(ruhlen2$country_code == "NIC"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # Nicaragua <- Costa Rica
  ruhlen2[which(ruhlen2$country_code == "NZL"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "GBR"), 12:739] # New Zealand <- Britain
  ruhlen2[which(ruhlen2$country_code == "PER"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Peru <- Colombia
  ruhlen2[which(ruhlen2$country_code == "PRY"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Paraguay <- Colombia
  ruhlen2[which(ruhlen2$country_code == "SDN"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "EGY"), 12:739] # Sudan <- Egypt
  ruhlen2[which(ruhlen2$country_code == "SLV"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "CRI"), 12:739] # El Salvador <- Costa Rica
  ruhlen2[which(ruhlen2$country_code == "VEN"), 12:739] <- ruhlen2[which(ruhlen2$country_code == "COL"), 12:739] # Venezuela <- Colombia
  
  # Get geograhic data for each country (longitude and latitude)
  geo <- read.csv("./input_data/geo.csv")
  
  # Get 2008 levels of income for each country
  mad08 <- read.csv("./input_data/maddison_subset_2008gdp.csv")
  
  # Harmonize all datasets by selecting country rows that they all have
  full <- merge(ruhlen2, mad, by = "country_code")
  full <- merge(full, geo, by = "country_code")
  full <- select(full, country_code)
  ruhlen2 <- merge(full, ruhlen2, by = 'country_code')
  mad2 <- merge(full, mad, by = 'country_code')
  geo2 <- merge(full, geo, by = 'country_code')
  mad08 <- merge(full, mad08, by = 'country_code')
  
  identical(as.vector(mad2$country_code), as.vector(ruhlen2$country_code)) # Check to make sure country codes match up exactly
  identical(as.vector(geo2$country_code), as.vector(ruhlen2$country_code)) # Check to make sure country codes match up exactly
  
  # Create growth distance matrix based on Maddison income data using Euclidean distance
  mad3 <- mad2[, 3:ncol(mad2)] # Extract the growth numbers
  rownames(mad3) <- mad2$country_code # Rename rows using country codes
  mad3 <- t(scale(t(mad3))) # Standardize growth rates within each country so that mean = 0, SD = 1
  growth.dist.matrix <- vegdist(mad3, "euclidean")
  
  # Create language distance matrix for Ruhlen linguistic data using Jaccard distance
  ruhlen3 <- ruhlen2[, 12:ncol(ruhlen2)] # Extract phoneme presence/absence for all 728 phonemes
  rownames(ruhlen3) <- ruhlen2$country_code # Rename rows to ISO country codes
  lang.dist.matrix <- vegdist(ruhlen3, "jaccard", binary = TRUE)
  
  # Create geographic distance matrix for geographic data using great circle distance
  geo3 <- geo2[ , 2:3]
  geo.dist.matrix <- rdist.earth(geo3, geo3)
  rownames(geo.dist.matrix) <- geo2[, 1]
  colnames(geo.dist.matrix) <- geo2[, 1]
  geo.dist.matrix <- as.dist(geo.dist.matrix)
  
  # Create distance matrix for 2008 levels of income
  mad08_2 <- as.matrix(mad08$X2008) # Extract the income numbers
  rownames(mad08_2) <- mad08$country_code # Rename rows using country codes
  mad08.dist.matrix <- vegdist(mad08_2, "euclidean")

  
  output <- list(growth.dist.matrix, lang.dist.matrix, geo.dist.matrix, mad08.dist.matrix)
  return(output)
}

#################################################################
# Function for running a set of Mantel tests
#################################################################

mantel.set <- function(growth.dist.matrix, lang.dist.matrix, geo.dist.matrix, mad08.dist.matrix, n) {
  mantel1 <- ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix, nperm = n)
  mantel2 <- ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix + geo.dist.matrix, nperm = n)
  mantel3 <- ecodist::mantel(growth.dist.matrix ~ lang.dist.matrix + geo.dist.matrix + mad08.dist.matrix, nperm = n)
  output <- list(mantel1, mantel2, mantel3)
  return(output)
}

format.results <- function(results) {
  cell1 <- paste0(formatC(round(results[[1]][1], digits = 3), digits = 3, format = "f"), " (",format(results[[1]][4], scientific=TRUE),")")
  cell2 <- paste0(formatC(round(results[[2]][1], digits = 3), digits = 3, format = "f"), " (",format(results[[2]][4], scientific=TRUE),")")
  cell3 <- paste0(formatC(round(results[[3]][1], digits = 3), digits = 3, format = "f"), " (",format(results[[3]][4], scientific=TRUE),")")
  mantel.row <- data.frame(cell1, cell2, cell3)
  return(mantel.row)
}

create.mantel.row <- function(mad, data.name) {
  matrices <- create.dist.matrices(mad)
  n <- nrow(as.matrix(matrices[[1]]))
  distances <- formatC(round(n*(n-1)/2, 0), format = "d", big.mark = ',')
  results <- mantel.set(matrices[[1]], matrices[[2]], matrices[[3]], matrices[[4]], 100000)
  results.row <- cbind(n, distances, format.results(results))
  rownames(results.row) <- data.name
  return(results.row)
}

##############################################################################################################
# Mantel tests
##############################################################################################################

# Choose ONE of the following versions of the Maddison growth pattern data:
mad_subset5 <- read.csv("./input_data/maddison_subset5.csv") # 1950-2008, 137 countries, 5-year CAGR ----- PRIMARY OPTION

# These are for checking only
mad <- read.csv("./input_data/maddison_subset4a.csv") # 1950-2000, 138 countries, 10-year CAGR
mad <- read.csv("./input_data/maddison_subset6.csv") # random (check on Mantel test)
mad <- read.csv("./input_data/maddison_subset7.csv") # first half of countries are randomized (check on Mantel test)

# By region
mad_subset_europe <- read.csv("./input_data/maddison_subset_europe.csv") # 1950-2008, 23 countries, 5-year CAGR, Europe only
mad_subset_africa <- read.csv("./input_data/maddison_subset_africa.csv") # 1950-2008, 48 countries, 5-year CAGR, Sub-Saharan Africa only
mad_subset_eastasia <- read.csv("./input_data/maddison_subset_eastasia.csv") # 1950-2008, 15 countries, 5-year CAGR, East Asia only
mad_subset_latam <- read.csv("./input_data/maddison_subset_latam.csv") # 1950-2008, 23 countries, 5-year CAGR, Latin America only
mad_subset_anglo <- read.csv("./input_data/maddison_subset_anglo.csv") # 1950-2008, 6 countries, 5-year CAGR, Anglo only
mad_subset_oecd <- read.csv("./input_data/maddison_subset_oecd.csv") # 1950-2008, 29 countries, 5-year CAGR, OECD members only
mad_subset_opec <- read.csv("./input_data/maddison_subset_opec.csv") # 1950-2008, 13 countries, 5-year CAGR, OPEC members only

# By excluded region
mad_subset_exeurope <- read.csv("./input_data/maddison_subset_exeurope.csv") # 1950-2008, 115 countries, 5-year CAGR, excluding Europe
mad_subset_exafrica <- read.csv("./input_data/maddison_subset_exafrica.csv") # 1950-2008, 89 countries, 5-year CAGR, excluding Sub-Saharan Africa
mad_subset_exeastasia <- read.csv("./input_data/maddison_subset_exeastasia.csv") # 1950-2008, 122 countries, 5-year CAGR, excluding East Asia
mad_subset_exlatam <- read.csv("./input_data/maddison_subset_exlatam.csv") # 1950-2008, 114 countries, 5-year CAGR, excluding Latin America
mad_subset_exanglo <- read.csv("./input_data/maddison_subset_exanglo.csv") # 1950-2008, 131 countries, 5-year CAGR, excluding Anglophone countries (USA, UK, Australia, Ireland, Canada)
mad_subset_exoecd <- read.csv("./input_data/maddison_subset_exoecd.csv") # 1950-2008, 108 countries, 5-year CAGR, excluding OECD members
mad_subset_exopec <- read.csv("./input_data/maddison_subset_exopec.csv") # 1950-2008, 124 countries, 5-year CAGR, excluding OPEC members

# For each country subset, create a row filled with Mantel r outputs
full_set <- create.mantel.row(mad_subset5, "All Countries")
ex_europe <- create.mantel.row(mad_subset_exeurope, "Ex-Europe")
ex_africa <- create.mantel.row(mad_subset_exafrica, "Ex-Sub-Saharan Africa")
ex_eastasia <- create.mantel.row(mad_subset_exeastasia, "Ex-East Asia")
ex_latam <- create.mantel.row(mad_subset_exlatam, "Ex-Latin America")
ex_anglo <- create.mantel.row(mad_subset_exanglo, "Ex-Anglo")
ex_oecd <- create.mantel.row(mad_subset_exoecd, "Ex-OECD")
ex_opec <- create.mantel.row(mad_subset_exopec, "Ex-OPEC")

# Combine all rows together and print to table in LaTeX
full.table <- data.frame(rbind(full_set, ex_europe, ex_africa, ex_eastasia, ex_latam, ex_anglo, ex_oecd, ex_opec))
colnames(full.table) <- c("Countries", "Distance \\hspace{20 mm} Values", "(1) Linguistic \\hspace{20 mm} only", "(2) Controls: \\hspace{20 mm} Geo", "(3) Controls: \\hspace{20 mm} Geo, Income")
print(xtable(full.table, align = c("l", "r", "p{1.2cm}", rep("p{2.3cm}", 3))), type='latex', sanitize.text.function=identity)

##############################################################################################################
# Distance summary stats by subset
##############################################################################################################

# Takes a distance matrix and returns its mean and standard deviation
dist.matrix.stats <- function(dist.matrix) {
  dist.matrix2 <- as.matrix(dist.matrix)
  dist.matrix2[upper.tri(dist.matrix2, diag=TRUE)] <- NA
  mean <- mean(dist.matrix2, na.rm=TRUE)
  sd <- sd(dist.matrix2, na.rm=TRUE)
  output <- data.frame(mean, sd)
  return(output)
}

# Format the output from dist.matrix.stats so that it shows a certain number of decimal places
format.stats <- function(stats, digits) {
  mean <- formatC(round(stats$mean, digits = digits), digits = digits, format = "f")
  sd <- formatC(round(stats$sd, digits = digits), digits = digits, format = "f")
  output <- paste0(mean, " (", sd, ")")
  return(output)
}

# Combine the mean and SD for each distance matrix into a single-row data frame
stats.row <- function(data, data.name) {
  matrices <- create.dist.matrices(data)
  stats <- lapply(matrices, dist.matrix.stats)
  n <- nrow(as.matrix(matrices[[1]]))
  distances <- formatC(round(n*(n-1)/2, 0), format = "d", big.mark = ',')
  growth <- format.stats(stats[[1]], 3)
  linguistic <- format.stats(stats[[2]], 3)
  geographic <- format.stats(stats[[3]], 0)
  income <- format.stats(stats[[4]], 0)
  results <- data.frame(n, distances, growth, linguistic, geographic, income)
  rownames(results) <- data.name
  return(results)
}

# Run summary stats on each subset
full_set2 <- stats.row(mad_subset5, "All Countries")
europe <- stats.row(mad_subset_europe, "Europe")
africa <- stats.row(mad_subset_africa, "Sub-Saharan Africa")
eastasia <- stats.row(mad_subset_eastasia, "East Asia")
latam <- stats.row(mad_subset_latam, "Latin America")
anglo <- stats.row(mad_subset_anglo, "Anglo")
oecd <- stats.row(mad_subset_oecd, "OECD")
opec <- stats.row(mad_subset_opec, "OPEC")

# Combine into a full table and print to LaTeX
full.table2 <- data.frame(rbind(full_set2, europe, africa, eastasia, latam, anglo, oecd, opec))
colnames(full.table2) <- c("Countries", "Distance \\hspace{20 mm} Values", "(1) Growth", "(2) Linguistic", "(3) Geographic", "(4) Income")
print(xtable(full.table2, align = c("l", "r", "p{1.2cm}", rep("l", 4))), type='latex', sanitize.text.function=identity)



##############################################################################################################


# Create distance matrix tables for Excel
write.csv(as.matrix(growth.dist.matrix), "./output_tables/growth_distance_matrix.csv")
write.csv(as.matrix(lang.dist.matrix), "./output_tables/language_distance_matrix.csv")
write.csv(as.matrix(geo.dist.matrix), "./output_tables/geographic_distance_matrix.csv")
write.csv(mad3, "./output_tables/growth_standardized.csv")
write.csv(as.matrix(mad08.dist.matrix), "./output_tables/mad08_distance_matrix.csv")


# Hierarchical clustering

ruhlen5 <- ruhlen %>%
  group_by(country_code) %>%
  arrange(desc(Population)) %>%
  slice(1) %>% # In case there's more than one max language, just take the first one
  ungroup

ruhlen6 <- ruhlen2[ , 12:ncol(ruhlen5)]
row.names(ruhlen6) <- ruhlen2$Country..Name.


dev.off()

dist.ruhlen6 <- vegdist(ruhlen6, "jaccard", binary = TRUE)

hc <- hclust(dist.ruhlen6, "ward.D2")

dhc <- as.dendrogram(hc,hang=0.1)
ddata <- dendro_data(dhc, type="rectangle")
ddata$labels[, 3] <- as.character(ruhlen5$Country..Name.)

p <- ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
p + geom_text(aes(x = x, y = y, label = label, hjust = 1), data= ddata$labels, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.3, 0))

rownames(mad.incomes) <- mad$country

d <- dist(mad.incomes) # distance matrix
dist.matrix <- as.matrix(d)

hc <- hclust(dist.ruhlen6, "complete")
plot(hc) # display dendogram

groups <- cutree(hc, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(hc, k=5, border="red")


lang.dist.matrix
hc <- hclust(lang.dist.matrix, "ward.D2")
plot(hc) # display dendogram

hc <- hclust(growth.dist.matrix, "ward.D2")
plot(hc) # display dendogram
groups <- cutree(hc, k=5) # cut tree into 5 clusters
rect.hclust(hc, k=5, border="red")

mad08.dist.matrix
hc <- hclust(mad08.dist.matrix, "ward.D2")
plot(hc) # display dendogram
groups <- cutree(hc, k=5) # cut tree into 5 clusters
rect.hclust(hc, k=5, border="red")

geo.dist.matrix
hc <- hclust(geo.dist.matrix, "complete")
plot(hc) # display dendogram
groups <- cutree(hc, k=5) # cut tree into 5 clusters
rect.hclust(hc, k=5, border="red")


####################################################################################################
# Interesting summary stats
####################################################################################################

# Average Euclidean distances
growth.dist.matrix2 <- as.matrix(growth.dist.matrix)
growth.dist.matrix2[upper.tri(growth.dist.matrix2, diag=TRUE)] <- NA
mean(growth.dist.matrix2, na.rm=TRUE)

# Function for converting a distance matrix into a normal matrix of country-pairs with their distance values
convert.dist.matrix <- function(dist.matrix) {
  dist.matrix <- as.matrix(dist.matrix)
  dist.matrix[upper.tri(dist.matrix, diag=TRUE)] <- NA
  df <- melt(dist.matrix, varnames = c("country1", "country2"))
  df <- na.omit(df)
  return(df)
}

# Compare growth and geo
growth.pairs <- convert.dist.matrix(growth.dist.matrix)
geo.pairs <- convert.dist.matrix(geo.dist.matrix)

growth.pairs2 <- growth.pairs
growth.pairs2 <- transmute(growth.pairs2, pairs = paste(as.character(country1), as.character(country2)), value)

geo.pairs2 <- geo.pairs
geo.pairs2 <- transmute(geo.pairs2, pairs = paste(as.character(country1), as.character(country2)), value)

merged.pairs <- merge(growth.pairs2, geo.pairs2, by = "pairs")
colnames(merged.pairs) <- c("pair", "growth", "geo")

ggplot(merged.pairs, aes(growth, geo)) +
  geom_text(aes(label=pair)) +
  geom_smooth(method = "lm", se = FALSE)

# Compare income and geo
growth.pairs <- convert.dist.matrix(mad08.dist.matrix)
geo.pairs <- convert.dist.matrix(geo.dist.matrix)

growth.pairs2 <- growth.pairs
growth.pairs2 <- transmute(growth.pairs2, pairs = paste(as.character(country1), as.character(country2)), value)

geo.pairs2 <- geo.pairs
geo.pairs2 <- transmute(geo.pairs2, pairs = paste(as.character(country1), as.character(country2)), value)

merged.pairs <- merge(growth.pairs2, geo.pairs2, by = "pairs")
colnames(merged.pairs) <- c("pair", "growth", "geo")

merged.pairs$growth <- log(merged.pairs$growth)
merged.pairs$geo <- log(merged.pairs$geo)

ggplot(merged.pairs, aes(growth, geo)) +
  geom_text(aes(label=pair)) +
  geom_smooth(method = "lm", se = FALSE)
