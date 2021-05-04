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


##################################################################################

ruhlen <- read.csv("input_data/ruhlen.csv")

ruhlen.phonemes <- ruhlen[, 12:739]
ruhlen.region <- ruhlen[, 9]

###################################################################################
##                                     PCA                                       ##
###################################################################################

# Remove constant columns
ruhlen.phonemes <- ruhlen.phonemes[,sapply(ruhlen.phonemes, function(v) var(v, na.rm=TRUE)!=0)]

ruhlen.pca <- prcomp(ruhlen.phonemes, center = TRUE, scale. = TRUE)

print(ruhlen.pca)

plot(ruhlen.pca, type = "lines")

summary(ruhlen.pca)

predict(ruhlen.pca, newdata=tail(ruhlen.phonemes, 2))

g <- ggbiplot(ruhlen.pca, obs.scale = 1, var.scale = 1,
              groups = ruhlen.region, ellipse = TRUE,
              circle = TRUE, var.axes = FALSE, labels = ruhlen$Country..ISO.Alpha.3.Code.)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
g <- g + scale_x_continuous(limits = c(-10, 10))
g <- g + scale_y_continuous(limits = c(-10, 10))
print(g)


###################################################################################
##                           Linguistic Distance                                 ##
###################################################################################

# Set base language
base.lang <- 678 # 678 is the row number for US-English
base.country <- ruhlen[base.lang, ]$Country..ISO.Alpha.3.Code.

# Jaccard distance, takes row numbers for languages, denominator is number of phonemes for either language
jaccard <- function(lang1, lang2) {
  difference <- sum(abs(ruhlen.phonemes[lang1, ] - ruhlen.phonemes[lang2, ]))
  denom <- sum(ruhlen.phonemes[lang1, ] + ruhlen.phonemes[lang2, ] > 0)
  result <- difference / denom
  return(result)
}

sum(abs(ruhlen.phonemes[base.lang, ] - ruhlen.phonemes[457, ]))
sum(ruhlen.phonemes[base.lang, ] + ruhlen.phonemes[457, ] > 0)

sum(abs(ruhlen.phonemes[base.lang, ] - ruhlen.phonemes[667, ]))
sum(ruhlen.phonemes[base.lang, ] + ruhlen.phonemes[667, ] > 0)

# Hammond distance, takes row numbers for l, denominator is total number of phonemes across all languages
hammond <- function(lang1, lang2) {
  difference <- sum(abs(ruhlen.phonemes[lang1, ] - ruhlen.phonemes[lang2, ]))
  denom <- ncol(ruhlen.phonemes)
  result <- difference / denom
  return(result)
}

jaccard.dist <- sapply(1:nrow(ruhlen.phonemes), jaccard, lang2=base.lang)
hammond.dist <- sapply(1:nrow(ruhlen.phonemes), hammond, lang2=base.lang)

# Append Jaccard and Hammond distances to original Ruhlen dataset
ruhlen2 <- ruhlen
ruhlen2$jaccard <- jaccard.dist
ruhlen2$hammond <- hammond.dist
ruhlen2$exp.jaccard <- exp(ruhlen2$jaccard) # Exponentiated version of linguistic distance to increase effect visibility
ruhlen2$exp.hammond <- exp(ruhlen2$hammond) # Exponentiated version of linguistic distance to increase effect visibility

ruhlen2 %>%
  filter(Country..ISO.Alpha.3.Code. == "POL") %>%
  select(X2, Language.Group, jaccard, Population)

jaccard(base.lang, 457)
jaccard(base.lang, base.lang-1)

# Aggregate languages by country
by.country <- ruhlen2 %>%
  group_by(Country..ISO.Alpha.3.Code.)

ruhlen.country <- summarise(by.country, jaccard = weighted.mean(as.numeric(jaccard), as.numeric(Population)), hammond = weighted.mean(as.numeric(hammond), as.numeric(Population)), exp.jaccard = weighted.mean(as.numeric(exp.jaccard), as.numeric(Population)), exp.hammond = weighted.mean(as.numeric(exp.hammond), as.numeric(Population)), region = first(Region))

names(ruhlen.country)[names(ruhlen.country)=="Country..ISO.Alpha.3.Code."] <- "countrycode" # Rename country code variable
ruhlen.country <- filter(ruhlen.country, countrycode != base.country) # Remove base country from data set

# Show countries in order of Jaccard linguistic distance to USA
table1 <- ruhlen.country %>%
  arrange(jaccard)

xtable(table1[10:20, ])
###################################################################################
##                             Country Income                                    ##
###################################################################################

penn <- read.csv("penn.csv")
penn <- penn %>%
  filter(year == 2011) %>%
  select(countrycode, rgdpe, pop)

penn$income <- penn$rgdpe / penn$pop
penn$log.income <- log(penn$income)

data <- merge(ruhlen.country, penn)

# Regression of log income on Jaccard distance
data <- within(data, region <- relevel(region, ref = "Europe"))
formula1 <- log.income ~ jaccard + region
reg1 <- lm(formula1, data)
summary(reg1)



formula2 <- log.income ~ jaccard
reg2 <- lm(formula2, data)
summary(reg2)

xtable(reg2)
xtable(reg1)


data %>%
  filter(countrycode != "CAN") %>%
  ggplot(aes(log.income, jaccard, colour = region)) + 
  geom_text(aes(label=countrycode)) +
  geom_smooth(method = "lm", se = FALSE)

data %>%
  filter(countrycode != "BWA") %>%
  ggplot(aes(log.income, hammond)) + 
  geom_text(aes(label=countrycode)) +
  geom_smooth(method = "lm", se = FALSE)
  
data %>%
  ggplot(aes(log.income, hammond, colour = region)) + 
  geom_text(aes(label=countrycode)) +
  geom_smooth(method = "lm", se = FALSE)


data %>%
  ggplot(aes(log.income, hammond)) + 
  geom_text(aes(label=countrycode, colour = region)) +
  geom_smooth(method = "lm", se = FALSE)

data %>%
  ggplot(aes(log.income, exp.hammond)) + 
  geom_text(aes(label=countrycode, colour = region)) +
  geom_smooth(method = "lm", se = FALSE)

data$exp.hammond100 <- exp(100 * data$hammond)

data %>%
  filter(exp.hammond100 < 300) %>%
  ggplot(aes(log.income, exp.hammond100)) + 
  geom_text(aes(label=countrycode, colour = region)) +
  geom_smooth(method = "lm", se = FALSE)


########### Remove Australia!!! #############


hc <- hclust(dist(ruhlen.phonemes), "ave")
ggdendrogram(hcity.D, rotate = TRUE, size = 2)


hc <- hclust(dist(USArrests), "ave")
dhc <- as.dendrogram(hc,hang=0.1)
ddata <- dendro_data(dhc, type="rectangle")
p <- ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
p + geom_text(aes(x = x, y = y, label = label, angle = -90, hjust = 0), data= label(ddata)) +
  scale_y_continuous(expand = c(0.3, 0))


region_subset <- filter(ruhlen, Region == "EastAsia")
hc <- hclust(dist(region_subset[, 12:739]), "ward.D")
dhc <- as.dendrogram(hc,hang=0.1)
ddata <- dendro_data(dhc, type="rectangle")
ddata$labels[, 3] <- as.character(region_subset$X2)

p <- ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
p + geom_text(aes(x = x, y = y, label = label, hjust = 1), data= ddata$labels, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0.3, 0))



########################################################################################################################
########################################################################################################################


###################################################################################
##                            Clustering by Income                               ##
###################################################################################


# Using Penn World Tables income data for 1950-2011

penn <- read.csv("penn.csv")
penn <- penn %>%
  filter(year == 2011) %>%
  select(countrycode, country, rgdpe, pop)

penn$y <- penn$rgdpe / penn$pop
penn$ly <- log(penn$y)

penn %>%
  filter(countrycode != "QAT") %>%
  ggplot(aes(reorder(country, ly), ly)) +
  geom_bar(stat="identity") +
  coord_flip()


# Hierarchical clustering by log income

hc <- hclust(dist(penn$ly), "ward.D")
dhc <- as.dendrogram(hc,hang=0.1)
ddata <- dendro_data(dhc, type="rectangle")
ddata$labels[, 3] <- as.character(penn$country)

p <- ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
p + geom_text(aes(x = x, y = y, label = label, hjust = 1), data= ddata$labels, size = 5) +
  coord_flip() +
  scale_y_continuous(expand = c(0.3, 0))


# Using Maddison income data for 1870-2010 for a subset of countries (based on data availability)

# Choose a subset of the Maddison income data to analyze
# maddison_subset:  1900-2010 for 28 rich countries (incl. Japan)
# maddison_subset2: 1950-2010 for 28 rich countries (incl. Japan)
# maddison_subset3: 1950-2000 for 138 countries
# maddison_subset4: 1950-2000 10-year growth rates for 138 countries

mad <- read.csv("maddison_subset4.csv")
mad.incomes <- mad[, -1]

# Choose one these next two lines: 1) no log, 2) log
mad.pca <- prcomp(mad.incomes, center = TRUE, scale. = TRUE)
mad.pca <- prcomp(log(mad.incomes), center = TRUE, scale. = TRUE)
print(mad.pca)

plot(mad.pca, type = "lines")

summary(mad.pca)

predict(mad.pca, newdata=tail(mad.incomes, 2))

g <- ggbiplot(mad.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE,
              circle = TRUE, var.axes = FALSE, labels = mad$country)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

dev.off()

hc <- hclust(dist(mad.incomes), "ward.D2")
dhc <- as.dendrogram(hc,hang=0.1)
ddata <- dendro_data(dhc, type="rectangle")
ddata$labels[, 3] <- as.character(mad$country)

p <- ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
p + geom_text(aes(x = x, y = y, label = label, hjust = 1), data= ddata$labels, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.3, 0))

rownames(mad.incomes) <- mad$country

d <- dist(mad.incomes) # distance matrix
dist.matrix <- as.matrix(d)
write.csv(dist.matrix, "dist_matrix.csv")
help(dist)
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

mad %>%
  arrange(desc(X1900)) %>%
  select(country, X1900)

mad %>%
  arrange(desc(X2010)) %>%
  select(country, X2010)


##############################################################################################################
# Create distance matrix for maddison_subset4 (based on 10-year growth rates for 138 countries, 1950-2000)
##############################################################################################################

mad <- read.csv("maddison_subset4.csv")

# Distance function: takes two vectors and returns a scalar distance
distance.euclidean <- function(a, b) {
  d <- sqrt(sum((a-b)^2))
  return(d)
}

# Jaccard distance INCOMPLETE!!!
distance.jaccard <- function(a, b) {
  d <- sqrt(sum((a-b)^2)) ## Need to write this
  return(d)
}

dist.matrix <- function(x) {
  m <- matrix(ncol = nrow(x), nrow = nrow(x))
  rownames(m) <- x[ , 1]
  colnames(m) <- x[ , 1]
  x <- x[, -1]
  for (i in 1:nrow(x)) {
    for (j in 1:nrow(x)) {
      m[i, j] <- distance.euclidean(x[i, ], x[j, ])
    }
  }
  return (m)
}

matrix1 <- dist.matrix(mad)
write.csv(matrix1, "matrix1.csv")

##############################################################################################################
# OLD CODE
##############################################################################################################
# Harmonize with Ruhlen, Maddison, and geographic datasets by selecting country rows that they all have
ruhlen2 <- filter(ruhlen2, country_code %in% mad$country_code)
mad2 <- filter(mad, country_code %in% ruhlen2$country_code)

# Was trying to figure out why ecodist::mantel wasn't work. Turns out it can't handle missing values


ncf:::partial.mantel.test

ecodist:::mantel
ecodist::mantel(growth.dist.matrix ~ as.dist(trade.dist.matrix), nperm = 99999)

vegan::mantel(growth.dist.matrix, as.dist(trade.dist.matrix), permutations = 9999)
cor(growth.dist.matrix, as.dist(trade.dist.matrix), use = "pairwise.complete.obs")
isSymmetric(as.dist(trade.dist.matrix))
isSymmetric(as.matrix(growth.dist.matrix))
isSymmetric(as.matrix(as.dist(trade.dist.matrix)))
View(solve(trade.dist.matrix))

class(trade.dist.matrix)
class(trade.dist.matrix[10, 10])
trade.dist.matrix2 <- trade.dist.matrix + t(trade.dist.matrix)
dim(as.dist(trade.dist.matrix))
dim(growth.dist.matrix)

isSymmetric(trade.dist.matrix)
dim(as.matrix(growth.dist.matrix))
dim(as.matrix(as.dist(trade.dist.matrix)))
identical(labels(as.dist(trade.dist.matrix)), labels(growth.dist.matrix))
class(labels(as.dist(trade.dist.matrix)))
class(labels(growth.dist.matrix))
class(as.dist(trade.dist.matrix))
class(growth.dist.matrix)

dim(as.matrix(as.dist(trade.dist.matrix)))
solve(trade.dist.matrix)
class(data.matrix(trade.dist.matrix)[40, 40])
class(trade.dist.matrix[40, 40])
as.numeric(trade.dist.matrix[2, 6])
trade.dist.matrix2 <- as.data.frame(lapply(trade.dist.matrix,as.numeric))
class(trade.dist.matrix[1, 4])

View(as.matrix(as.dist(trade.dist.matrix)))
solve(as.dist(trade.dist.matrix))

mode(trade.dist.matrix) <- "numeric"


##############################################################################################################
# Start real code
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


germ <- read.csv("germany.csv")
rownames(germ) <- germ[ , 1]
germ <- germ[ , -1]

germ %>%
  ggplot(aes(lang, geo)) +
  geom_text(aes(label=rownames(germ))) +
  geom_smooth(method = "lm", formula = y ~ I(x^2), se = FALSE)




##############################################################################################################
# Procrustes
##############################################################################################################

korea <- filter(mad2, country_code == "KOR")
thailand <- filter(mad2, country_code == "THA")
colombia <- filter(mad2, country_code == "COL")

korea <- korea[ , 3:ncol(korea)]
thailand <- thailand[ , 3:ncol(thailand)]
colombia <- colombia[ , 3:ncol(colombia)]

mini <- rbind(korea, thailand, colombia)
row.names(mini) <- c("korea", "thailand", "colombia")
mini.dist.matrix <- vegdist(mini, "euclidean")
as.matrix(mini.dist.matrix)

mini <- as.data.frame(t(mini))
colnames(mini) <- c("korea", "thailand", "colombia")
lm(korea ~ thailand, data = mini)
plot(korea ~ thailand, data = mini)

fitted(procrustes(korea, thailand))
``