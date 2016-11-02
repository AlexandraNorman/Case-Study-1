#Qustion 1: How many of the IDs match?
##With NAs
n_distinct(GDP_final$Code )
##Without NAs
n_distinct(GDP_final_nan$Code )

#count of NAs in GDP and ranking
##With NAs
count(GDP_final[is.na(GDP_final$GDP),])
count(GDP_final[is.na(GDP_final$Ranking),])
##Without NAs
count(GDP_final_nan[is.na(GDP_final_nan$GDP),])
count(GDP_final_nan[is.na(GDP_final_nan$Ranking),])


#Sorting the data in increasing GDPs 
##With NAs
GDP_final_sort <- GDP_final[order(GDP_final$GDP, decreasing = FALSE),]
##Without NAs
GDP_final_nan_sort <- GDP_final_nan[order(GDP_final_nan$GDP, decreasing = FALSE),]

#Question 2: What is the 13th country in the resulting data frame? 
##With NAs
GDP_final_sort[13,]
##Without NAs 
GDP_final_nan_sort[13,]

#Question 3: What are the average GDP rankings for the "High income: OECD" and "High income: nonOECD" groups?
#High Income: OECD
##With NAs
Rank.OECD <- GDP_final_sort[which(grepl("High income: OECD",GDP_final_sort$Income.Group)),]
mean(Rank.OECD$Ranking)
##Without NAs
Rank.OECD.nan <- GDP_final_nan_sort[which(grepl("High income: OECD",GDP_final_nan_sort$Income.Group)),]
mean(Rank.OECD.nan$Ranking) 
#High Income: nonOECD
##With NAs
Rank.nonOECD <- GDP_final_sort[which(grepl("High income: nonOECD",GDP_final_sort$Income.Group)),]
mean(Rank.nonOECD$Ranking)
#This mean is NA since there is NA data for ranking in this set this is why we want to filter out any NA values
##Without NAs
Rank.nonOECD.nan <- GDP_final_nan_sort[which(grepl("High income: nonOECD",GDP_final_nan_sort$Income.Group)),]
mean(Rank.nonOECD.nan$Ranking)


#Question 4:
ggplot(GDP_final_nan_sort)+ geom_point(aes(x= GDP,y=Income.Group,colour=Income.Group)) + scale_x_log10()

ggplot(GDP_final_nan_sort)+ geom_point(aes(x= Ranking,y=GDP,colour=Income.Group))


#Question 5:
GDP_final_nan_sort$Group <- cut(GDP_final_nan_sort$Ranking, breaks = 5)
table(GDP_final_nan_sort$Group, GDP_final_nan_sort$Income.Group)
top_ranked_lower_income <- GDP_final_nan_sort[which(GDP_final_nan_sort$Income.Group == "Lower middle income" & GDP_final_nan_sort$Group == "(0.811,38.8]"),] 
