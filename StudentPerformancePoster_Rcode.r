#looking to see what my path is set to
getwd()
#setting my path where I want it
setwd("C:\\Users\\marle\\Downloads\\IST719_DataVis\\")

library(vioplot)
library(plyr)
library(tidyverse)
library(readxl)
library(plotrix)
library(maps)
library(mapproj)



#read in my data through the import data set
FL_schGrades <- SchoolGrades1999_2019_WITH_ZIP_CODES_REGION_SIZE_Copy


#including only the columns that I can use in this data set
FL_schGrades <- FL_schGrades[5:3341, c(1,2,4,5,6,7,9,10,11,12,13,14,20,22,24,47,48,50,51,52)]
#renaming the columns
colnames(FL_schGrades) <- c("District_Number", "District", "School", "Region"
                            , "Zip_Code", "Total_Students", "ELA_Proficiency", "ELA_Gains", "ELA_Gains_L25"
                            , "Math_Proficiency", "Math_Gains", "Math_Gains_L25", "Total_POints.Earned"
                            , "Percent_TotalPossiblePoints", "SchoolGrade_2019", "Charter_School_Y/N"
                            , "Title1_Y/N", "SchoolType", "Prct_Minority_Students", "Prct_Econ_Dis_Students")


#I only want complete data
#All of the public schools would have to report their data to the state.  Any
#schools who did notreport their data, would be private schools or the like.  These schools would 
#not be of interest to my analyses.  Therefore, I made no attempt to fill in NAs.
#Even using only complete cases, I still had a data set that was substantial enough
#for the purpose of this project.
FL_schGrades <- FL_schGrades %>%
  filter(complete.cases(.))

FL_schGrades <- as.data.frame(FL_schGrades)
str(FL_schGrades)


FL_schGrades[,6:14] <- lapply(FL_schGrades[,6:14], as.numeric)
FL_schGrades[,19:20] <- lapply(FL_schGrades[,19:20], as.numeric)
str(FL_schGrades)
View(FL_schGrades)

#Select for only elementary schools, and then remove that column
FL_schGrades <- FL_schGrades %>%
  filter(SchoolType =="01") %>%
  select(-contains("SchoolType"))


#How did FL_schools perform overall?
grade.freq <- aggregate(FL_schGrades$SchoolGrade_2019, list(FL_schGrades$SchoolGrade_2019), FUN = length)
colnames(grade.freq) <- c("sch_grade", "freq")
barplot(grade.freq$freq, names.arg = grade.freq$freq
        , col = "red4"
        ,border = NA
        ,main = "School Grade Frequency"
        ,xlab = "School Grade"
        , ylab = "Frequency"
        , ylim = c(0,600))

#is there a significant difference in ELA performance and math performance?
mean(FL_schGrades$ELA_Proficiency)
mean(FL_schGrades$Math_Proficiency)
#Taking an initial look at the distributions of proficiencies in math
#and reading
#(multidimensional plot)
par(bty = "n")
vioplot(FL_schGrades$ELA_Proficiency
        , side = "left"
        , plotCentre = "line"
        , col = "dodgerblue3"
        , main = "Florida's Student ELA and Math Proficiency"
        , ylab = "Percent of Students Meeting Proficiency"
        , ylim = c(10, 100)
        , border = NA)
vioplot(FL_schGrades$Math_Proficiency
        , side = "right"
        , plotCentre = "line"
        , ylim = c(10, 100)
        , col = "green3"
        , add = TRUE
        , border = NA)
legend("topleft", legend = c("ELA Proficiency", "Math Proficiency"), fill = c("dodgerblue3", "green3"))

mean(FL_schGrades$ELA_Proficiency)
mean(FL_schGrades$Math_Proficiency)

#Descriptive statistics around my independent factors (Title1, charter, Economically disadvantaged, 
#%minority)

#barplot of title1 schools vs. non title1 schools
#aggregating number of title1 schools
title1 <- aggregate(FL_schGrades$`Title1_Y/N`, list(FL_schGrades$`Title1_Y/N`), FUN = length)
colnames(title1) <- c("Y/N", "Title1.num")

#colors for bars
fill_bars <- c()
for(i in 1: length(title1$`Y/N`))
{
  if (title1$`Y/N`[i] == "YES")
  { 
    fill_bars <- c(fill_bars, "darkred")
  } else {
    fill_bars <- c(fill_bars, "moccasin")
  }
}

#actual barplot
par(mar = c(5,5,3,3))
barplot(title1$Title1.num, names.arg = title1$`Y/N`
        , col = fill_bars
        ,border = NA, 
        ,main = "Number of Title 1 Schools vs. Non Title 1 Schools"
        ,xlab = "Title 1 Yes/No"
        , ylim = c(0, 1200))

#barplot of charter schools vs. public schools
#count of charter schools
charter <- count(FL_schGrades$`Charter_School_Y/N`)
colnames(charter) <- c("charter_y/n", "charter.num")

#colors for bars
fill_bars <- c()
for(i in 1: length(charter$`charter_y/n`))
{
  if (charter$`charter_y/n`[i] == "YES")
  { 
    fill_bars <- c(fill_bars, "salmon4")
  } else {
    fill_bars <- c(fill_bars, "lightsalmon")
  }
}

par(mar = c(5,5,3,3))
barplot(charter$charter.num, names.arg = charter$`charter_y/n`
        , col = fill_bars
        ,border = NA, 
        ,main = "Number of Charter Schools vs. Public Schools"
        ,xlab = "Charter School Yes/No"
        , ylim = c(0, 1600))


#tree map of Economically disadvantaged students by district
#need colorRamps and treemap libraries
install.packages("treemap")
install.packages("colorRamps")
install.packages("rasterImage")
install.packages("grDevices")

library(colorRamps)
library(treemap)
library(rasterImage)
library(grDevices)

#aggregating %Economically disadvantaged students by district
econ.dis <- aggregate(FL_schGrades$Prct_Econ_Dis_Students, list(FL_schGrades$District)
                      , FUN = mean)
colnames(econ.dis) <- c("District", "avg.perc.ED")
econ.dis$avg.perc.ED <- round(econ.dis$avg.perc.Ecolnames(econ.dis) <- c("District", "avg.perc.ED"))

#plotting tree map for econ.dis students by district
my.palette <- rev(heat.colors(10, alpha = 1,))

par(mar = c(5,5,5,5))
treemap(econ.dis
        , index = "District"
        , vSize = "avg.perc.ED"
        , palette = my.palette
        , type = "index"
        , title = "Average School Percentage of Economically Disadvantaged Students by District")

#aggregating %Economically disadvantaged students by district
minority <- aggregate(FL_schGrades$Prct_Minority_Students, list(FL_schGrades$District)
                      , FUN = mean)
colnames(minority) <- c("District", "avg.perc.minority")
minority$avg.perc.minority <- round(minority$avg.perc.minority, digits = 0)

#plotting tree map for minority students by district
my.palette <- hcl.colors(10, palette = "reds", alpha = 1, rev = TRUE, fixup = TRUE)

par(mar = c(5,5,5,5))
treemap(minority
        , index = "District"
        , vSize = "avg.perc.minority"
        , palette = my.palette
        , type = "index"
        , title = "Average School Percentage of Minority Students by District")
legend <- 

#where are the highest/lowest performing schools in FL?
#Creating a bubble map of districts most proficient in math and ELA
library(devtools) 
library(dplyr) 
library(gdata) 
library(ggplot2) 
library(ggmap) 
library(mapdata) 
library(maps) 
library(openintro) 
library(sqldf) 
library(stringr) 
library(tidyverse)
library(zipcode)

#getting US zipcodes from zipcode package to merge it with zipcode data in FL_schGrades
data(zipcode)

#aggregating ELA proficiency by district
ELA.prof.by.dstr <- aggregate(FL_schGrades$ELA_Proficiency, list(FL_schGrades$Zip_Code), FUN = mean)
colnames(ELA.prof.by.dstr) <- c("District_zip", "ELA_prof")

#creating zipcode col from zipcode package and putting it in my ELA.prof.by.distr
ELA.prof.by.dstr$zip <- clean.zipcodes(ELA.prof.by.dstr$District_zip)
#merging zipcode, long, lat data with ELA.prof.by.distr
ELA.prof.by.dstr <- merge(ELA.prof.by.dstr, zipcode
                          , by.x = "zip"
                          , by.y = "zip")

#aggregating math proficiency by district
FL_schGrades$Math_Proficiency <- round(FL_schGrades$Math_Proficiency, digits = 0)
math.prof.by.dstr <- aggregate(FL_schGrades$Math_Proficiency, list(FL_schGrades$Zip_Code), FUN = mean)
colnames(math.prof.by.dstr) <- c("District_zip", "math_prof")

#creating zipcode col from zipcode package and putting it in my math.prof.by.distr
math.prof.by.dstr$zip <- clean.zipcodes(math.prof.by.dstr$District_zip)

#merging zipcode, long, lat data with math.prof.by.distr
math.prof.by.dstr <- merge(math.prof.by.dstr, zipcode
                          , by.x = "zip"
                          , by.y = "zip")

#librarying in needed packages
library(ggplot2)
library(maps)
library(dplyr)
library(leaflet)
library(plotly)

#bubble map of ELA proficiency by district
ggplot() +
  geom_polygon(data = FL, aes(x = long, y = lat, group = group)
               , fill = "grey"
               , alpha = 0.5) +
  geom_point(data = ELA.prof.by.dstr, aes(x = longitude
               , y = latitude
               , size = ELA_prof
               , color = ELA_prof
               , alpha = 0.3)) +
 scale_size_continuous(range = c(0.3, 6)) +
 theme_void() + coord_map() + theme(legend.position = "left")


#bubble map of math proficiency by district
ggplot() +
  geom_polygon(data = FL, aes(x = long, y = lat, group = group)
               , fill = "grey"
               , alpha = 0.5) +
  geom_point(data = math.prof.by.dstr, aes(x = longitude
                                          , y = latitude
                                          , size = math_prof
                                          , color = math_prof
                                          , alpha = 0.3)) +
  scale_size_continuous(range = c(0.3, 6)) +
  scale_color_viridis_c(option = "D"
                        , trans="log") + 
  scale_alpha_continuous(trans="log") +
  theme_void() + coord_map() + theme(legend.position = "left")

#Is student growth, or student proficiency more influential on overall school 
#performance?
#making scatter plots to answer this 

library(ggplot2)

plot(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$ELA_Gains
     , col = "red"
     , pch = 16)
cor.test(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$ELA_Gains
         , method = "pearson")
plot(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$Math_Gains, col = "blue")
cor.test(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$Math_Gains
         , method = "pearson")
plot(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$ELA_Proficiency, col = "green")
cor.test(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$ELA_Proficiency
         , method = "pearson")
plot(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$Math_Proficiency
     , col = "green3"
     , pch = 16
     , bty = "n"
     , main = " Relationship between Student Math Proficiency and Overall Student Performance"
     , ylab = "School Math Proficiency Percentage"
     , xlab = "School Overall Student Performance Percentage")
cor.test(FL_schGrades$Percent_TotalPossiblePoints, FL_schGrades$Math_Proficiency
         , method = "pearson")

#looking at my binary variables Title1 and Charter to compare the distributions
#of student performance by these variables

fill_bars <- c()
for(i in 1: length(title1$`Y/N`))
{
  if (title1$`Y/N`[i] == "YES")
  { 
    fill_bars <- c(fill_bars, "darkred")
  } else {
    fill_bars <- c(fill_bars, "moccasin")
  }
}


FL_schGrades$Charter <- FL_schGrades$`Charter_School_Y/N`
 
par(mfrow = c(1, 2), mar = c(7, 4, 7, 3))
boxplot(FL_schGrades$Percent_TotalPossiblePoints ~ FL_schGrades$Title.one
        , col = fill_bars
        , main = "Student Proficiency for Title 1 and Non-Title 1 Schools"
        , ylab = "Percentage of Total Points"
        , xlab = "Title 1 School (Y/N)")

fill_bars <- c()
for(i in 1: length(charter$`charter_y/n`))
{
  if (charter$`charter_y/n`[i] == "YES")
  { 
    fill_bars <- c(fill_bars, "red4")
  } else {
    fill_bars <- c(fill_bars, "lightsalmon")
  }
}
        
boxplot(FL_schGrades$Percent_TotalPossiblePoints ~ FL_schGrades$Charter
        , col = fill_bars
        , main = "Student Proficiency for Charter vs. Public Schools"
        , ylab = "Percentage of Total Points"
        , xlab = "Charter School (Y/N)")


#which factors have the strongest relationship with student performance?
st.prof.factors <- lm(formula = Percent_TotalPossiblePoints ~ Total_Students + 
                        Prct_Minority_Students + 
                        Prct_Econ_Dis_Students +
                        , data = FL_schGrades)

summary(st.prof.factors)
#Percent Economically disadvantaged students was the only factor
#with a significant relationship with student performance

#scatter plot showing this relationship
library(ggplot2)

options(scipen = 999)

theme_set(theme_bw())   FL_schGrades$Prct_Econ_Dis_Students

gg <- ggplot(FL_schGrades, aes(y = Percent_TotalPossiblePoints, x = Prct_Econ_Dis_Students)) +
  geom_point(aes(col = "red2", pch = 20)) +
  geom_smooth(method = "loess", se = F) +
  labs(y = "Student Performance"
       , x = "Percentage of Students Economically Disadvantaged"
       , title = "Relationship between Economically Disadvantaged Students and Academic Performance")

plot(gg)


              
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
