library(pracma)
library(readr)
library(tibble)
library(signal)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcyto)

setwd("C:/Users/Alaa/Desktop/Study 4/All raw ECGs/ECGs/raw ECG with colour/")
mydir = "ECGs"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)



setwd("C:/Users/Alaa/Desktop/Study 4/")
mydir = "data"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)


for (x in 1:length(ID))
{  


ColoursQuantifier <- read_csv(myfiles[f])


time <- ColoursQuantifier$time
ColouredTime <- ColoursQuantifier$newcolourVector

mv <-  ColoursQuantifier$mv

up <- ColoursQuantifier$UpperColourLimit
UpperColourLimit <- up[1]
low <- ColoursQuantifier$LowerColourlimit
LowerColourlimit <- low[1]


colorcodesValues <- NA
length(colorcodesValues) <- 9
colorcodesValues[1] <- LowerColourlimit # purple 
colorcodesValues[2] <- UpperColourLimit - (40*7) #blue
colorcodesValues[3] <- UpperColourLimit - (40*6) #lime
colorcodesValues[4] <- UpperColourLimit - (40*5) #green
colorcodesValues[5] <- UpperColourLimit - (40*4) #yellow
colorcodesValues[6] <- UpperColourLimit - (40*3) #orange
colorcodesValues[7] <- UpperColourLimit - (40*2) #dark orange, this QT nomogram
colorcodesValues[8] <- UpperColourLimit - (40*1) #red
colorcodesValues[9] <- UpperColourLimit #dark red



################################################################

#Calculating AUC using trapezoidal rule

Average_two_y_points <- 0
Delat_two_x_points <- 0
Area_of_trapezoid <- 0 

length(Average_two_y_points) <- length(ColouredTime) 
length(Delat_two_x_points) <- length(ColouredTime) 
length(Area_of_trapezoid) <- length(ColouredTime) 



for ( i in 1:length(ColouredTime))
{ 
  if (ColouredTime[i] < 800)
  { 
    Average_two_y_points[i] = (abs(mv[i]) + abs(mv[i+1]))/2
    Delat_two_x_points[i] = 1 # this alwasys equals to one
    Area_of_trapezoid[i] = Average_two_y_points[i] * Delat_two_x_points[i] #we can remove the delta as it's one
  }
  else Area_of_trapezoid[i] = 0
}


#create a new ECG file (coloured ECG signal)

mydata <- cbind(time,mv,ColouredTime,Area_of_trapezoid,UpperColourLimit,LowerColourlimit)
ECG_with_Area_of_trapezoid <- na.omit(mydata)
write.csv(ECG_with_Area_of_trapezoid,"C:/Users/Alaa/Desktop/Study 4/ECG_with_Area_of_trapezoid.csv")

ECG_with_Area_of_trapezoid <- read_csv("C:/Users/Alaa/Desktop/Study 4/ECG_with_Area_of_trapezoid.csv")
View(ECG_with_Area_of_trapezoid)



##############################################################################

#Quantifying area under each colour:

ColouredTime <- ECG_with_Area_of_trapezoid$ColouredTime


#area of purple colour
sum_of_purple_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[1]-40) && ColouredTime[x] < colorcodesValues[1])
  { 
    sum_of_purple_area = sum_of_purple_area + Area_of_trapezoid[x] 
  }
}

sum_of_purple_area

#area of blue colour
sum_of_blue_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[1]) && ColouredTime[x]<= colorcodesValues[2])
  { 
    sum_of_blue_area = sum_of_blue_area + Area_of_trapezoid[x] 
  }
}

sum_of_blue_area

#area of green colour
sum_of_green_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[2]) && ColouredTime[x]<= colorcodesValues[3])
  { 
    sum_of_green_area = sum_of_green_area + Area_of_trapezoid[x] 
  }
}

sum_of_green_area

#area of lime colour
sum_of_lime_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[3]) && ColouredTime[x]<= colorcodesValues[4])
  { 
    sum_of_lime_area = sum_of_lime_area + Area_of_trapezoid[x] 
  }
}

sum_of_lime_area

#area of yellow colour
sum_of_yellow_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[4]) && ColouredTime[x]<= colorcodesValues[5])
  { 
    sum_of_yellow_area = sum_of_yellow_area + Area_of_trapezoid[x] 
  }
}

sum_of_yellow_area

#area of orange colour
sum_of_orange_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[5]) && ColouredTime[x]<= colorcodesValues[6])
  { 
    sum_of_orange_area = sum_of_orange_area + Area_of_trapezoid[x] 
  }
}


#area of dark orange colour
sum_of_dark_orange_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[6]) && ColouredTime[x]<= colorcodesValues[7])
  { 
    sum_of_dark_orange_area = sum_of_dark_orange_area + Area_of_trapezoid[x] 
  }
}


#area of red colour
sum_of_red_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[7]) && ColouredTime[x]<= colorcodesValues[8])
  { 
    sum_of_red_area = sum_of_red_area + Area_of_trapezoid[x] 
    
  }
}

#area of dark red colour
sum_of_dark_red_area <- 0

for (x in 1:length(ColouredTime))
{ 
  if (ColouredTime[x] > (colorcodesValues[8]) && ColouredTime[x]<= (colorcodesValues[9]))
  { 
    sum_of_dark_red_area = sum_of_dark_red_area + Area_of_trapezoid[x] 
    
  }
}


##############################################################################

#Calculate the second derivative

sum_of_purple_area
sum_of_blue_area
sum_of_green_area
sum_of_lime_area
sum_of_yellow_area
sum_of_orange_area
sum_of_dark_orange_area
sum_of_red_area
sum_of_dark_red_area

First_derivative_1 = sum_of_blue_area - sum_of_purple_area
First_derivative_2 = sum_of_green_area - sum_of_blue_area
First_derivative_3 = sum_of_lime_area - sum_of_green_area
First_derivative_4 = sum_of_yellow_area - sum_of_lime_area
First_derivative_5 = sum_of_orange_area - sum_of_yellow_area
First_derivative_6 = sum_of_dark_orange_area - sum_of_orange_area
First_derivative_7 = sum_of_red_area - sum_of_dark_orange_area
First_derivative_8 = sum_of_dark_red_area - sum_of_red_area

Second_derivative_1 = First_derivative_2 - First_derivative_1
Second_derivative_2 = First_derivative_3 - First_derivative_2 
Second_derivative_3 = First_derivative_4 - First_derivative_3
Second_derivative_4 = First_derivative_5 - First_derivative_4
Second_derivative_5 = First_derivative_6 - First_derivative_5
Second_derivative_6 = First_derivative_7 - First_derivative_6
Second_derivative_7 = First_derivative_8 - First_derivative_7

##############################################################################

#Calculate the inflection point
#First locate the maximum concave down as the value that has the largest negative value (which is the minimum) 
#if the second derivative is positive, then the signal is convex; otherwise it's concave.

  Maximum_concave_down <- c(Second_derivative_1,Second_derivative_2,Second_derivative_3,Second_derivative_4,Second_derivative_5,Second_derivative_6,Second_derivative_7)
  Maximum_concave_down_index <- which.min(Maximum_concave_down)
  
  if(Maximum_concave_down_index == 1 && Second_derivative_2 > 0)
  {inflectionPoint <- 1.5
  }
  
  if(Maximum_concave_down_index == 1 && Second_derivative_3 > 0)
  {inflectionPoint <- 2.5
  }
  
  if(Maximum_concave_down_index == 1 && Second_derivative_4 > 0)
  {inflectionPoint <- 3.5
  }
  
  if(Maximum_concave_down_index == 1 && Second_derivative_5 > 0)
  {inflectionPoint <- 4.5
  }
  
  
  if(Maximum_concave_down_index == 2 && Second_derivative_3 > 0)
  {inflectionPoint <- 2.5
  }
  
  if(Maximum_concave_down_index == 2 && Second_derivative_4 > 0)
  {inflectionPoint <- 3.5
  }
  
  if(Maximum_concave_down_index == 2 && Second_derivative_5 > 0)
  {inflectionPoint <- 4.5
  next}
  
  if(Maximum_concave_down_index == 3 && Second_derivative_4 > 0)
  {inflectionPoint <- 3.5
  }
  
  if(Maximum_concave_down_index == 3 && Second_derivative_5 > 0)
  {inflectionPoint <- 4.5
  }
  
  
  if(Maximum_concave_down_index == 4 && Second_derivative_5 > 0)
  {inflectionPoint <- 4.5
  }
  
  if(Maximum_concave_down_index == 4 && Second_derivative_5 < 0 && Second_derivative_6 > 0)
  {inflectionPoint <- 5.5
  }
  
  if(Maximum_concave_down_index == 4 && Second_derivative_5 < 0 && Second_derivative_6 < 0 && Second_derivative_7 > 0)
  {inflectionPoint <- 6.5
  }
  
  if(Maximum_concave_down_index == 4 && Second_derivative_5 < 0 && Second_derivative_6 < 0 && Second_derivative_7 < 0)
  {inflectionPoint <- 7.5
  }
  
  if(Maximum_concave_down_index == 5 && Second_derivative_6 > 0)
  {inflectionPoint <- 5.5
  }
  
  if(Maximum_concave_down_index == 5 && Second_derivative_6 < 0 && Second_derivative_7 > 0)
  {inflectionPoint <- 6.5
  }
  
  if(Maximum_concave_down_index == 5 && Second_derivative_6 < 0 && Second_derivative_7 < 0)
  {inflectionPoint <- 7.5
  }
  
  if(Maximum_concave_down_index == 6 && Second_derivative_7 > 0)
  {inflectionPoint <- 6.5
  next}
  
  if(Maximum_concave_down_index == 6 && Second_derivative_7 < 0)
  {inflectionPoint <- 7.5
  }
  
  if(Maximum_concave_down_index == 7)
  {inflectionPoint <- 7.5
  }
  

##############################################################################
  
#Renaming the second derivatives based on their pseudo-colour time range
  
Purple_Blue_Green <- Second_derivative_1
Blue_Green_Lime <-  Second_derivative_2
Green_Lime_Yellow <- Second_derivative_3
Lime_Yellow_Orange <- Second_derivative_4
Yellow_Orange_DarkOrange <- Second_derivative_5
Orange_DarkOrange_Red <- Second_derivative_6
DarkOrange_Red_DarkRed <- Second_derivative_7
  
##############################################################################

#Save the data

All_ECGs_with_coloured_AUC_and_second_derivative <- cbind(myfiles[f],sum_of_purple_area,sum_of_blue_area,sum_of_green_area,sum_of_lime_area,sum_of_yellow_area,sum_of_orange_area,sum_of_dark_orange_area,sum_of_red_area,sum_of_dark_red_area,
                                                          First_derivative_1,First_derivative_2,First_derivative_3,First_derivative_4,First_derivative_5,First_derivative_6,First_derivative_7,First_derivative_8,
                                                          Purple_Blue_Green,Blue_Green_Lime,Green_Lime_Yellow,Lime_Yellow_Orange,Yellow_Orange_DarkOrange,Orange_DarkOrange_Red,DarkOrange_Red_DarkRed,
                                                          inflectionPoint)
write.table(All_ECGs_with_coloured_AUC_and_second_derivative, "C:/Users/Alaa/Desktop/Study 4/All_ECGs_with_coloured_AUC_and_second_derivative.csv", sep = ",", col.names = !file.exists("myDF.csv"), append = T)

}