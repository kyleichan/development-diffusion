## Linguistic Distance and Patterns of Economic Growth
Kyle Chan
April 13, 2016
/  
/  
**Research question:**
Is there a relationship between linguistic distances and economic growth patterns?
(even after controlling for geographic distance)
/  
/  
**Data:**  
1. Linguistic: Ruhlen database of phonemic presence/absence, 2,082 languages, 728 phonemes, Source: Creanza et al (2015) PNAS paper
2. Economic: Historical per capita GDP data from 1 AD to present, complete 1950-2008 income data for 115 countries, Source: Maddison Project
3. Geographic: Latitude and longitude for all countries, Source: Google
<br>
<br>
**Step 1: Create linguistic, economic growth, and geographic distance matrices**
Linguistic:
Select one plurality language per country (automatic + manual)
Jaccard distance on phonemes

Economic:
Get 5-year compound annual growth rates (CAGRs) for 1950-2008 by country
Standardize these growth rates within each country (i.e., mean=0, SD=1)
Euclidean distance between each country’s set of growth rates

Geographic:
great-circle distance (haversine formula) on latitude and longitude
<br>
<br>
**Step 2: Run Mantel test on distance matrices**
Use simple Mantel test to check for significant relationship between two distance (dissimilarity) matrices
Running standard regression on distances matrices doesn’t work due to autocorrelation
Mantel test randomly permutes rows and columns of one matrix to see if this will yield a higher correlation coefficient

Use partial Mantel test to check for relationship between linguistic and economic distance while controlling for geographic distance
Given distance matrices X, Y, and Z: get residuals X’ and Y’ from regression on Z, then run Mantel test on X’ and Y’


