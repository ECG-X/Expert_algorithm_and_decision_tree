library(readxl)

DataforAlgoirthm <- read_excel("Computers in Biology and Medicine/Human rules vs ML/UnbalancedData.xlsx", 
                               sheet = "All")

#inputs (ECG clinical info)
ECG_ID <- DataforAlgoirthm$ECG_ID
Drug <- DataforAlgoirthm$Drug
HR <- DataforAlgoirthm$HR
QT <- DataforAlgoirthm$QT
dif <- DataforAlgoirthm$QT_nomogram_difference_ms
QTNomogram <- DataforAlgoirthm$NomogramValue
Outcome <- DataforAlgoirthm$Outcome

#inputs (ECG pseudo-colour features)
InflectionPoint <- DataforAlgoirthm$InflectionPoint
Purple_Blue_Green <- DataforAlgoirthm$Purble_Blue_Green
Blue_Green_Lime <- DataforAlgoirthm$Blue_Green_Lime
Green_Lime_Yellow <- DataforAlgoirthm$Green_Lime_Yellow
Lime_Yellow_Orange <- DataforAlgoirthm$Lime_Yellow_Orange
Yellow_Orange_DarkOrange <- DataforAlgoirthm$Yellow_Orange_DarkOrange

#outputs
pred <- NA
confidence_rate <- NA
ExplainbleResult <- NA
QTRangeLow <- NA
QTRangeHigh <- NA

for (i in 1:length(ECG_ID)) #classify all ECGs 
{ 
  
###########################################################################################################################################################################################################      
  
#Every colour represents a 40 ms time window.
  
###########################################################################################################################################################################################################      
  
  #Rule 1: if inflection point is 1.5 (i.e. the maximum concave down (T-wave) is within purple_blue_green colours, T-wave probably ends within the green colour region)
  
  if(InflectionPoint[i] == 1.5)
  {pred[i] <- "Normal"
  confidence_rate[i] <- 1
  QTRangeHigh[i] <- QTNomogram[i] - 120
  QTRangeLow[i] <- QTNomogram[i] - 160
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is very likely normal, and the patient is not considered at risk for TdP. 
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are cool (purple to blue to green),
  with a greater amount of blue than green, indicating a normal QT-interval.
 
  The T-wave probably ends within the green colour region, which indicates that the QT/HR falls below the nomogram line 
  by approximately 120 ms or more, showing no risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################    
  
  #Rule 2: if inflection point is 2.5, and the maximum concave down (T-wave) is within both Purple_Blue_Green and Blue_Green_Lime,
  #but has more amounts of Purple_Blue_Green than Blue_Green_Lime, and then it probably ends within the green-lime colour region)
  
  if(InflectionPoint[i] == 2.5 & Purple_Blue_Green[i] < 0 & 
     (Purple_Blue_Green[i]/(Purple_Blue_Green[i]+Blue_Green_Lime[i]) > 0.5))
  {pred[i] <- "Normal"
  confidence_rate[i] <- 1
  QTRangeHigh[i] <- QTNomogram[i] - 120
  QTRangeLow[i] <- QTNomogram[i] - 160
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is very likely normal, and the patient is not considered at risk for TdP.  
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are cool (purple to blue to green),
  with a greater amount of blue than green, indicating a normal QT-interval.
 
  The T-wave probably ends within the green-lime colour region, which indicates that the QT/HR falls below the nomogram line 
  by approximately 120 ms or more, showing no risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################  
  
  #Rule 3: if inflection point is 2.5, then the T-wave probably ends within the lime colour region) 
  
  if(InflectionPoint[i] == 2.5)
  {pred[i] <- "Normal"
  confidence_rate[i] <- 2
  QTRangeHigh[i] <- QTNomogram[i] - 80
  QTRangeLow[i] <- QTNomogram[i] - 120
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is probably normal, and the patient is not considered at risk of TdP.
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are cool (purple to blue to green),
  with a greater amount of green than blue, indicating a normal QT-interval.
  
  The T-wave probably ends within the lime-yellow colour region, which indicates that the QT/HR falls below the nomogram line 
  by approximately 80 ms or more, showing no risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################  
  
  #Rule 4: if inflection point is 3.5, and the maximum concave down (T-wave) is within both Blue_Green_Lime and Green_Lime_Yellow,
  #but has more amounts of Blue_Green_Lime than Green_Lime_Yellow, and then it probably ends within the lime-yellow colour region)
  
  if(InflectionPoint[i] == 3.5 & Blue_Green_Lime[i] < 0 & 
  (Blue_Green_Lime[i]/(Blue_Green_Lime[i]+Green_Lime_Yellow[i]) > 0.5))
  {pred[i] <- "Normal"
  confidence_rate[i] <- 2
  QTRangeHigh[i] <- QTNomogram[i] - 80
  QTRangeLow[i] <- QTNomogram[i] - 120
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is probably normal, and the patient is not considered at risk of TdP.
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are cool (purple to blue to green),
  with a greater amount of green than blue, indicating a normal QT-interval.
  
  The T-wave probably ends within the lime-yellow colour region, which indicates that the QT/HR falls below the nomogram line 
  by approximately 80 ms or more, showing no risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################  
  
  #Rule 5: if inflection point is 3.5, then The T-wave probably ends within the yellow colour region)
  
  if(InflectionPoint[i] == 3.5)
  {pred[i] <- "Normal"
  confidence_rate[i] <- 3
  
  QTRangeHigh[i] <- QTNomogram[i] - 40
  QTRangeLow[i] <- QTNomogram[i] - 80
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is possibly normal, and the patient is not considered at risk of TdP.
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
  
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are blue to green to yellow,
  with a greater amount of green than yellow, indicating a normal QT-interval.
  
  The T-wave is probably ended within the yellow region, which indicates that the QT/HR falls
  below the nomogram line by approximately 40 ms or more, showing no risk for TdP.

  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################  
 
  #if there is an ST-elevation then a T-wave (i.e. convex to start the T-wave, then concave for the T-wave peak and end), we can determine that by the amount of rate the convex, 
  #which needs to be greather than or equall the T-wave concave to represesnt the start of the T-wave  
  #The patten is then (- + - -) 
  
  #Rule 6: if inflection point is 4.5, this means the maximum concave down (T-wave) is within Lime_Yellow_Orange showing risk for TdP,
  #but there is a concave in the Purple_Blue_Green (probably an ST-elevation), followed by a larger convex in the Blue_Green_Lime 
  #representing a start of an increasing T-wave, 
  
  if(InflectionPoint[i] == 4.5 & Purple_Blue_Green[i] < 0 & Blue_Green_Lime[i] > 0 &
  Blue_Green_Lime[i] > abs(Purple_Blue_Green[i]))
  {pred[i] <- "Abnormal"
  confidence_rate[i] <- 4
  
  QTRangeHigh[i] <- QTNomogram[i] 
  QTRangeLow[i] <- QTNomogram[i] - 40
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is possibly abnormal, and the patient is considered at risk of TdP. 
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green) 
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red). 
  
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are warm colours (green to yellow to orange), 
  with a greater amount of yellow than green, indicating an abnormal QT-interval. 
  
  It looks like there is an ST-elevation as there is a purple-blue wave before the T-wave. 
  
  The T-wave probably ends within the yellow-orange region, which indicates that the QT/HR falls on or very close to the nomogram line, showing risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}

###########################################################################################################################################################################################################    
 
  #Rule 7: if inflection point is 4.5, and the maximum concave down (T-wave) is within both Green_Lime_Yellow and Lime_Yellow_Orange,
  #but has more amounts of Green_Lime_Yellow than Lime_Yellow_Orange, and then it probably ends within the lime-yellow colour region), this case is probably borderline QT-interval
  
  if(InflectionPoint[i] == 4.5 & Green_Lime_Yellow[i] < 0 & 
  (Green_Lime_Yellow[i]/(Green_Lime_Yellow[i]+Lime_Yellow_Orange[i]) > 0.5))
  {pred[i] <- "Normal"
  confidence_rate[i] <- 3
  
  QTRangeHigh[i] <- QTNomogram[i] - 40
  QTRangeLow[i] <- QTNomogram[i] - 80
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is possibly normal, and the patient is not considered at risk of TdP. 
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are green to yellow,
  with a greater amount of green than yellow, indicating a normal QT-interval.
  
  The T-wave is probably ended within the yellow region, which indicates that the QT/HR falls
  below the nomogram line by approximately 40 ms or more, showing no risk for TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################    
  
  #Rule 8: if inflection point is 4.5, then The T-wave probably ends within the yellow-orange colour region)
  
  if(InflectionPoint[i] == 4.5)
  {pred[i] <- "Abnormal"
  confidence_rate[i] <- 4
  
  QTRangeHigh[i] <- QTNomogram[i] 
  QTRangeLow[i] <- QTNomogram[i] - 40
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is possibly abnormal, and the patient is considered at risk of TdP.
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are warm (green to yellow to orange),
  with a greater amount of yellow than green, indicating an abnormal QT-interval.
  
  The T-wave is probably ended within the yellow-orange region, which indicates that the QT/HR falls
  on or very close to the nomogram line, showing risk for TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}
  
###########################################################################################################################################################################################################    
  
  #Rule 9: if inflection point is 5.5, and the maximum concave down (T-wave) is within both Lime_Yellow_Orange and Yellow_Orange_DarkOrange,
  #but has more amounts of Lime_Yellow_Orange than Yellow_Orange_DarkOrange, and then it probably ends within the yellow-orange colour region)
  if(InflectionPoint[i] == 5.5 & Lime_Yellow_Orange[i] < 0 & 
  (Lime_Yellow_Orange[i]/(Lime_Yellow_Orange[i]+Yellow_Orange_DarkOrange[i]) > 0.5))
  {pred[i] <- "Abnormal"
  confidence_rate[i] <- 5
  
  QTRangeHigh[i] <- QTNomogram[i] + 40
  QTRangeLow[i] <- QTNomogram[i] 
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is probably abnormal, and the patient is considered at risk of TdP. 
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
  
  
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are warm (yellow to orange),
  with a greater amount of yellow than orange, indicating an abnormal QT-interval.
  
  The T-wave probably ends within the yellow-orange region, which indicates that the QT/HR falls above the nomogram line, showing risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i]))
  
  next}

###########################################################################################################################################################################################################    
 
  #Rule 10: if inflection point is 5.5, then The T-wave probably ends within the orange-red colour region)
  
  if(InflectionPoint[i] >= 5.5)
  {pred[i] <- "Abnormal"
  confidence_rate[i] <- 6
  QTRangeHigh[i] <- QTNomogram[i] + 80
  QTRangeLow[i] <- QTNomogram[i] + 40
  
  ExplainbleResult[i] <- paste("The QT-interval of this ECG is very likely abnormal, and the patient is considered at risk of TdP.
  
  This decision has been made based on the assumption that the QT-interval is considered normal when the area under the T-wave contains cool pseudo-colours (purple to blue to green),
  and prolonged with risk of TdP when it contains warm pseudo-colours (yellow to orange to red).
 
  The maximum concave down in the pseudo-coloured area was considered to be the T-wave, and most of its colours are warm colours (orange to red), indicating an abnormal QT-interval.
  
  The T-wave probably ends within the orange-red region, which indicates that the QT/HR falls above the nomogram line, showing risk of TdP.
  
  Based on the pseudo-colouring scale, the estimated value of the QT-interval ranges from", QTRangeLow[i], "to", QTRangeHigh[i] , "ms, and the HR is", round(HR[i])).
  
  next}
  
} 

###########################################################################################################################################################################################################    

#Save the results

Results <- cbind(ECG_ID,Drug,HR,QT,dif,Outcome,pred,confidence_rate,ExplainbleResult)

write.table(Results, "~/Computers in Biology and Medicine/Human rules vs ML/ExplainableResults.csv", sep = ",", col.names = !file.exists("myDF.csv"))

###########################################################################################################################################################################################################    

#Check if the QT value is within the estimated range
CheckQTEst <- NA
for ( i in 1:length(QT))
{if(QT[i]>= QTRangeLow[i] & QT[i]<= QTRangeHigh[i])
{CheckQTEst[i] <- "within"}
}

Results <- cbind(Outcome,pred,confidence_rate,HR,QT,QTRangeLow,QTRangeHigh,dif,ExplainbleResult)

write.table(Results, "~/Computers in Biology and Medicine/Human rules vs ML/ExplainableResults.csv", sep = ",", col.names = !file.exists("myDF.csv"))
