setwd ("C:/Users/Kyle/Dropbox/Research/Genetic Distance")
library(foreign)
library(ggplot2)
library(rworldmap) # To produce maps with countries filled in with different colors

# Read in genetic distance data from Spolaore, Wacziarg (2004)
g <- read.dta("genetic_distance.dta")

# Subset data to just have genetic distance to US
g.us <- subset(g, country_2=="U.S.A")

# Read in World Bank data on per capita GDP (PPP) for 1995
w <- read.csv("World Bank per capita GDP (PPP) 1995.csv")
colnames(w)[1] <- "country_1" # Change column 1 name to match genetic distance data set label
colnames(w)[2] <- "y" # Change income column name to "y"
w$log.y <- log(w$y) # Add log of per capita GDP

g.us <- merge(g.us, w, by="country_1") # Add income to genetic distance data set
g.us <- g.us[!is.na(g.us$Region),] # Remove countries with missing region

# Plot genetic distance to US on log income, label by country name, color by region
plot.gendist.all <- ggplot(g.us, aes(fst_distance_weighted, log.y, label=country_1)) + geom_text(aes(label=country_1, color=factor(Region)), size=8)
plot.gendist.all

plot.gendist.europe <- ggplot(subset(g.us, g.us$Region=="Europe & Central Asia"), aes(fst_distance_weighted, log.y, label=country_1)) + geom_text(aes(label=country_1), size=8)
plot.gendist.europe

plot.gendist.ssa <- ggplot(subset(g.us, g.us$Region=="Sub-Saharan Africa"), aes(fst_distance_weighted, log.y, label=country_1)) + geom_text(aes(label=country_1), size=8)
plot.gendist.ssa

# Plot world map where country color varies by genetic distance to US
sPDF <- joinCountryData2Map (g.us, joinCode="ISO3", nameJoinColumn="CountryCode") # Join genetic distance data set to R package map
map.weighted <- mapCountryData(sPDF, nameColumnToPlot="fst_distance_weighted")
map.dominant <- mapCountryData(sPDF, nameColumnToPlot="fst_distance_dominant")


##### Run OLS of genetic distance to US on log income
log.y.us <- w[which(w$country_1=="United States"), ]$log.y # Get log income of US for 1995
g.us$log.y.us.distance <- abs(log.y.us - g.us$log.y) # Create new variable for absolute value of difference in log incomes relative to US

reg.gendist <- lm(fst_distance_weighted~log.y.us.distance, data=g.us)
summary(reg.gendist)

plot.gendist.all
abline(reg)

######

a <- data.frame(country1=NA, country2=NA)
n <- nrow(g.us)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    newrow <- c(g.us[i, ]$country_1, g.us[j, ]$country_1)
    a <- rbind(a, newrow)
  }
}




# Run OLS of linguistic distance on log income

