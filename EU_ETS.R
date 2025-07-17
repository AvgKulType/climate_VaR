rm(list = ls())

##
##
##  Data engineering
##
##


#importing data
library(readxl)
weekly <- read_excel("PhD/Weekly.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 6)

#Remove not-relevant columns
weekly <- data.frame(weekly)
weekly <- weekly[,-c(2,5,6,8)]
View(weekly)

#Define data frames
Date <- as.Date(weekly$Date)
weekly$Date <- Date
weekly_r <- log(weekly[2:1432,2:7]/weekly[1:1431,2:7])
weekly_r <- cbind(Date[1:1431], weekly_r)

#Create dummies for EU ETS
weekly$EU_ETS <- ifelse(format(weekly$Date, "%Y")<"2024", 0,1)
weekly_r$EU_ETS <- ifelse(format(weekly_r$Date, "%Y")<"2024", 0,1)


#Create data frames
BCIC3_23 <- na.omit(weekly_r$BCI.C3..Tubarao.Qingdao..160.000.or.170.000.mt[format(weekly_r$Date, "%Y")=="2023"])
BCIC3_24 <- na.omit(weekly_r$BCI.C3..Tubarao.Qingdao..160.000.or.170.000.mt[format(weekly_r$Date, "%Y")=="2024"])


BCIC2_23 <- na.omit(weekly_r$BCI.C2..Tubarao.Rotterdam.160.000.long.tons[format(weekly_r$Date, "%Y")=="2023"])
BCIC2_24 <- na.omit(weekly_r$BCI.C2..Tubarao.Rotterdam.160.000.long.tons[format(weekly_r$Date, "%Y")=="2024"])

BDTITD7_23 <- na.omit(weekly_r$BDTI.TD7..80.000mt..North.Sea.to.Continent[format(weekly_r$Date, "%Y")=="2023"])
BDTITD7_24 <- na.omit(weekly_r$BDTI.TD7..80.000mt..North.Sea.to.Continent[format(weekly_r$Date, "%Y")=="2024"])

BDTTD20_23 <- na.omit(weekly_r$BDTI.TD20..130.000mt..West.Africa...Continent[format(weekly_r$Date, "%Y")=="2023"])
BDTTD20_24 <- na.omit(weekly_r$BDTI.TD20..130.000mt..West.Africa...Continent[format(weekly_r$Date, "%Y")=="2024"])

#BSI63s1c <- weekly_r$BSI.63.Route.S1C..US.Gulf.trip.to.China.South.Japan
#BSI63S4A <- weekly_r$BSI.63.Route.S4A..US.Gulf.trip.to.Skaw.Passero


##
##
##  Visualisation
##
##
#par(mfrow = c(2, 1))
plot(weekly$Date[1232:1432], 
     weekly$BCI.C2..Tubarao.Rotterdam.160.000.long.tons[1232:1432],
     type = "l",
     col = "blue",
     ylab = "BCI ($/mt)",
     xlab = "Date",
     main = "Baltic Capsize Index, route C2 (blue), C3 (red)",
     caption = "Baltic index of capsize vessels for the routes C2 (blue) and C3 (red).",
     ylim = c(7,38))
lines(weekly$Date[1232:1432],
      weekly$BCI.C3..Tubarao.Qingdao..160.000.or.170.000.mt[1232:1432],
      col = "red")

#Boxplot
boxplot(BCIC3_23, BCIC3_24,BCIC2_23, BCIC2_24)
boxplot(BCIC2_23, BCIC2_24)


##
##
##  Analysis
##
##



#T-test
t.test(BCIC3_23, BCIC3_24)
t.test(BCIC2_23, BCIC2_24)
t.test(BDTITD7_23, BDTITD7_24)
t.test(BDTTD20_23, BDTTD20_24)

#F-test
#This is capsize and shows some promising results
var.test(BCIC3_23, BCIC3_24)  #This is Turarao - Qungdao
var.test(BCIC2_24, BCIC2_23)  #This is Turarao - Rotterdam

var.test(BDTITD7_23, BDTITD7_24)
var.test(BDTTD20_23, BDTTD20_24)


#Diff-in-Diff


BCI_C3 <- na.omit(data.frame(Date, weekly$`BCI C3: Tubarao/Qingdao, 160,000 or 170,000 mt`))
BCI_C2 <- na.omit(data.frame(Date, weekly$`BCI C2: Tubarao/Rotterdam 160,000 long tons`))

BCI_C2_2024 <-BCI_C2[format(BCI_C2$Date, "%Y")=="2024",]
BCI_C2_2023 <-BCI_C2[format(BCI_C2$Date, "%Y")=="2023",]
BCI_C2_2024_r <- log(BCI_C2_2024$weekly..BCI.C2..Tubarao.Rotterdam.160.000.long.tons.[2:52]/BCI_C2_2024$weekly..BCI.C2..Tubarao.Rotterdam.160.000.long.tons.[1:51])
BCI_C2_2023_r <- log(BCI_C2_2023$weekly..BCI.C2..Tubarao.Rotterdam.160.000.long.tons.[2:51]/BCI_C2_2023$weekly..BCI.C2..Tubarao.Rotterdam.160.000.long.tons.[1:50])


BSI_S1C <- na.omit(data.frame(Date, Weekly$`BSI 63 Route S1C: US Gulf trip to China-South Japan`))
BSI_S4A <- na.omit(data.frame(Date, Weekly$`BSI 63 Route S4A: US Gulf trip to Skaw-Passero`))

