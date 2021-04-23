################################################################################
################################################################################
####################       Economic  Crisis     ################################
####################       Impact on Rates      ################################
####################              and           ################################
####################       Internal Debt        ################################
####################     Data Visualization     ################################
################################################################################
################################################################################
###################     Anna Karina Pérez Peña      ############################
###################    Sebastián Ocampo Palacios    ############################
################################################################################
--------------------------------------------------------------------------------
  

# 1. Libraries & data -----------------------------------------------------

library(readxl)
library(xts)
library(dplyr)
library(dygraphs)


setwd("C:/Users/socap/OneDrive/Documentos/CIDE/6 - Sexto Semestre/Money/data-vis")
DATA=read_xlsx("DEBT85-20.xlsx", col_names=FALSE, na="NA")
DATA=DATA[2:434,] #Removes headers

colnames(DATA)=DATA[1,] 
colnames(DATA)     #Renames columns
DATA=DATA[2:433,] #Removes colname row


# TROUBLESHOOTING

#Converting columns into numeric type.
DATA$INTERNAL_DEBT=as.numeric(as.character(DATA$INTERNAL_DEBT))
DATA$AVG_TIIE28=as.numeric(as.character(DATA$AVG_TIIE28))
DATA$AVG_TIIE91=as.numeric(as.character(DATA$AVG_TIIE91))
DATA$CETES_28=as.numeric(as.character(DATA$CETES_28))
DATA$INPC=as.numeric(as.character(DATA$INPC))

#We will ignore NET_DEBT

#Date data is not being read properly.
dates=seq(from=as.Date("1985-01-01"), to=as.Date("2020-12-01"), by="month")
DATA$Dates=dates #Redifines date data.

summary(DATA)

#Obtaining real values for debt.
  base= 109.27100 #INPC, December 2015 as base year.
  DATA$INPC=DATA$INPC/base *100 #We create the deflator coefficient

  DATA$INTERNAL_DEBT=DATA$INTERNAL_DEBT/DATA$INPC #Deflated Debt


# 2. Time Series  ---------------------------------------------------------
DATA_TS=xts(x=DATA, order.by = dates) #xts object
DATA_TS=DATA_TS[,2:ncol(DATA_TS)] #Removes redundant columns (dates)

plot(x=dates, y=DATA_TS$INTERNAL_DEBT, col="red")
par(new=TRUE)
plot(x=dates, y=DATA_TS$TIIE91, col="blue")
par(new=TRUE)
plot(x=dates, y=DATA_TS$CETES_28 col="green")
par(new=TRUE)
plot(x=dates, y=DATA_TS$TIIE_28 col="brown")

#We need to deseasonlize our data using the package "seasonal".

# 3. Graphs ---------------------------------------------------------------

# Finally the plot
dygraph(DATA_TS[,1:3], main= "Debt!") %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)


##Ejemplos Erik-Luis

dygraph(FE, main = "Total mexican internal debt: nominal amount in pesos by category")%>%
  dySeries("Total_Debt",strokeWidth = 3)%>%
  dySeries("Other_Values", label = "Intern Debt in dollars", strokeWidth = 2)%>%
  dyAxis("y", label = "Debt (thousands of millions)")%>%
  dyAxis("x", label = "Dates" )%>%
  dyOptions(axisLineWidth = 1.5)%>%
  dyEvent("1994-1-1", "1994-Begining of the Debt Crisis", labelLoc = "top")%>%
  dyEvent("1997-1-1", "1997-End of the Debt Crisis", labelLoc = "top")


dygraph(FF, main = "Tesobonos and other types of debt during tequila crisis")%>%
  dySeries("Total_Debt",strokeWidth = 4)%>%
  dySeries("Tesobonos", strokeWidth = 4)%>%
  dyAnnotation("1993-05-01", text = "This series represnt intern debt in dollar, more
                  than half of the intern debt was in dollars. Tesobonos is part of it")%>%
  dyAxis("y", label = "Debt (thousands of millions)")%>%
  dyAxis("x", label = "Dates" )%>%
  dyOptions(axisLineWidth = 1.5)%>%
  dyRangeSelector(dateWindow = c("1993-01-01", "1997-12-01"))


# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dygraphs318.html"))


