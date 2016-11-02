#Cleaning up gdp data
##removing columns that are all NAs or not useful information in gdp data
gdp <- gdp_raw[c(5:194, 196:219, 221, 223:235), c("X", "Gross.domestic.product.2012", "X.2", "X.3", "X.4")]
#renaming the column names
setnames(gdp, old = c('X', 'Gross.domestic.product.2012', 'X.2','X.3', 'X.4'), 
         new = c('Code', 'Ranking', 'Country', 'GDP','Notes'))
##replacing rows with .. as the value of GDP to be NA
for(j in 1:nrow(gdp)){if(gdp$GDP[j] == ".." || gdp$GDP[j] == "") gdp$GDP[j] = NA}
#replaing all blank values with NA
for(i in 1:nrow(gdp)){ for(j in 1:ncol(gdp))  { if(!is.na(gdp[i,j])) if(gdp[i,j] == "") gdp[i,j] = NA }}
#reformatting numeric to be able to sort later
gdp$GDP <- as.numeric(gsub("[^[:digit:]]","", gdp$GDP)) 
gdp$Ranking <- as.numeric(gsub("[^[:digit:]]","", gdp$Ranking))

#removing all NANs from rank and gdp
gdp_nan <- subset(gdp, !(is.na(gdp$Ranking) | is.na(gdp$GDP)))

gdp_nan$GDP <- as.numeric(gsub("[^[:digit:]]","", gdp_nan$GDP)) 
gdp_nan$Ranking <- as.numeric(gsub("[^[:digit:]]","", gdp_nan$Ranking))

#Cleaning up education data
#selecting only the colmuns that we want and need for our analyze
edu <- edu_raw[, c("CountryCode",  "Income.Group", "Short.Name")] 
for(i in 1:nrow(edu)){ for(j in 1:ncol(edu))  { if(!is.na(edu[i,j])) if(edu[i,j] == "") edu[i,j] = NA }}

#Merging data
##With NAs in rank and GDP
GDP_final <- merge(gdp, edu, by.x = "Code", by.y = "CountryCode" )
##Without NAs
GDP_final_nan <- merge(gdp_nan, edu, by.x = "Code", by.y = "CountryCode" )