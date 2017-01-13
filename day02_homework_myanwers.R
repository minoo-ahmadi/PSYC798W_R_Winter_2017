# Complete all of the items below
# Use comments where you're having trouble or questions

# 1. Read your data set into R
install.packages("haven") # althernatively: install.packages("tidyverse")
library(haven) #reading the package
getwd() # to see the current directory
setwd("C:/Users/ahmadi/Documents/Courses/PSYC798W/PSYC798W_R_Winter_2017") # changing directory to where the file I'm trying to read is saved.
my.data<- read_sav("Data_FYP.sav") # reading the SPSS data file



# 2. Peek at the top few rows
head(my.data)

# 3. Peek at the top few rows for only a few columns
head(my.data[ , 1:10]) #what if we want to see first few rows for specific columns?

# 4. How many rows does your data have?
nrow(my.data) # 27

# 5. Get a summary for every column
summary(my.data)

# 6. Get a summary for one column
summary(my.data$SocialAnxietyScore)

# 7. Are any of the columns giving you unexpected values?
#Unfortunatley not!

# 8. Select a few key columns, make a vector of the column names
useful.col.names <- c("Sex", "DominanceCongruencyEffect200", "DominanceCongruencyEffect400", "DominanceCongruencyEffect800")

# 9. Create a new data.frame with just that subset of columns
columns.I.care.about<- my.data[ , useful.col.names]
df.cols.I.care<- data.frame(columns.I.care.about)

# 10. Create a new data.frame that is just the first 10 rows
#     and the last 10 rows of the data from the previous step
new.df<- data.frame(c(head(df.cols.I.care, 10), tail(df.cols.I.care,10)))

# 11. Create a new data.frame that is a random sample of half of the rows.
# HINT: ?sample
newer.df1<- data.frame(sample_n(df.cols.I.care, (nrow(df.cols.I.care))/2)) # number of rows
#or:
newer.df2<- data.frame(sample_frac(df.cols.I.care, 1/2)) # fractio of rows
# since the df.cols.I.care has already been defined as a data.frame, 
#we don't actually need the data.frame() in either of these two any more.

# 12. Find a comparison in your data that is interesting to make
#     (comparing two sets of numbers)
#     - run a t.test for that comparison
#     - decide whether you need a non-default test
#       (e.g., Student's, paired)
#     - run the t.test with BOTH the formula and "vector"
#       formats, if possible
#     - if one is NOT possible, say why you can't do it

# Social Anxiety Score (SocialAnxietyScore) between m and f (Sex)
SAS.Sex.Comp<- t.test(my.data$SocialAnxietyScore ~ my.data$Sex)

# The vector method, shown below, can't be done because the x and y arguments
# in t.test need to be numerical. Here Gender (same as Sex above) is not numerical
# hence we should use the formula method used above.
SAS<- my.data[ ,208]
Gender<- my.data[ ,2]S
t.test(SAS, Gender)

# 13. Repeat #12 for TWO more comparisons
#     - ALTERNATIVELY, if correlations are more interesting,
#       do those instead of t-tests (and try both Spearman and
#       Pearson correlations)
#     - Tip: it's okay if the comparisons are kind of nonsensical, this is 
#       just a programming exercise
SAS.Dom_Cong_Eff.cor<- cor.test(my.data$SocialAnxietyScore, my.data$DominanceCongruencyEffect, method = "pearson")
SAS.SPS_SIAS_T.cor<- cor.test(my.data$SocialAnxietyScore, my.data$SPS_SIAS_Total, method = "spearman")

# 14. Save all results from #12 and #13 in an .RData file
save(SAS.Sex.Comp, SAS.Dom_Cong_Eff.cor, SAS.SPS_SIAS_T.cor, file= "homework2_12_13.RData")


# 15. Email me your version of this script, PLUS the .RData
#     file from #14
#     - ALTERNATIVELY, push your version of this script and your .RData results
#       to a repo on GitHub, and send me the link
