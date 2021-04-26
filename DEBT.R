################################################################################
################################################################################
####################       Economic  Crisis     ################################
####################       Impact on Rates      ################################
####################              and           ################################
####################       Internal Debt        ################################
####################     Data Visualization     ################################
################################################################################
################################################################################
###################     Anna Karina P√©rez Pe√±a      ##########################
###################    Sebasti√°n Ocampo Palacios    ###########################
################################################################################
--------------------------------------------------------------------------------

# CODE -------------------------------------------------------------------

# 1. Libraries & data -----------------------------------------------------

library(readxl)
library(xts)
library(dplyr)
library(dygraphs)
library(seasonal)
library(ggplot2)


setwd("C:/Users/socap/OneDrive/Documentos/GitHub/crisis-debt-rates")

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
                  # Alternative is 99.90910
  DATA$INPC=DATA$INPC/base *100 #We create the deflator coefficient

  DATA$INTERNAL_DEBT=DATA$INTERNAL_DEBT/DATA$INPC #Deflated Debt
  
#Obtaining real values for the Interest Rates. 
  #data$AVG_TIIE91=data$AVG_TIIE91/data$INPC

  
 
# 2. Time Series  ---------------------------------------------------------
  
  # 1997-2020. 
  dates=dates[145:432]
  
  #We need to deseasonalize our data using the package "seasonal".
  DATA=DATA[145:432,] # Timeframe 1997-2020.
  DATA=DATA[c("INTERNAL_DEBT", "AVG_TIIE91")] #Reduces variables
  
  #Deseasonalize
  Des_data <- ts(coredata(DATA),frequency=12,start=c(1997,1), end = c(2020, 12))
  inData <- seas(Des_data) #seas function.
  Des_data=as.data.frame(inData)

#?
#DATA<-cbind(DATA$INTERNAL_DEBT,DATA$AVG_TIIE91) 
#colnames(DATA) <-c("internal debt","interest rate") 
#plot(DATA, main="Internal rate and interest rate",xlab="AÒo",ylab="$")
#Data = as.data.frame(DATA)


#We add the seasonal adjusted data to our data.
DATA<-cbind(dates, DATA, Des_data$INTERNAL_DEBT.seasonaladj, Des_data$AVG_TIIE91.seasonaladj) 
colnames(DATA)= c("dates", "INTERNAL_DEBT", "AVG_TIIE91", "INTERNAL_DEBT_SA", "AVG_TIIE91_SA")
  
  
  
# 3. Graphs ---------------------------------------------------------------

#ggplot2 graph----------------------------

# Start with a usual ggplot2 call:
# ggplot(db, aes(x=date, y=as.numeric(`AVG_TIIE28`))) +

#   #Data= DATA

plot(x=DATA$date,y=DATA$INTERNAL_DEBT,type="l") 
plot(x=DATA$date,y=DATA$AVG_TIIE91,type="l")
plot(x=DATA$date,y=DATA$INTERNAL_DEBT_SA,type="l") #Seasonal Adjusted 
plot(x=DATA$date,y=DATA$AVG_TIIE91_SA,type="l") #Seasonal Adjusted

#   # Custom the Y scales:
#   scale_y_continuous(
#     
#     # Features of the first axis
#     name = "First Axis",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis( trans=~.*100, name="Second Axis")
#   ) +
#   
#   theme_ipsum()

coeff <- as.numeric(100000000)
rateColor <- "#69b3a2"
debtColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(DATA, aes(x=dates))+
  geom_line( aes(y=as.numeric(AVG_TIIE91)), size=1, color=rateColor) + 
  geom_line( aes(y=INTERNAL_DEBT/coeff), size=1, color=debtColor) +  # Divide by 10 to get the same range
  
  scale_y_continuous(
    # Features of the first axis
    name = "Interest Rate",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Internal Debt")
  ) +
  
  #theme_ipsum() +  No est· funcionando esta lÌnea :( 
  
  theme(
    axis.title.y = element_text(color = rateColor, size=13),
    axis.title.y.right = element_text(color = debtColor, size=13)
  ) +
  
  ggtitle("Title") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year") +
  theme(axis.text.x=element_text(angle=60, hjust=1))



#Graph as Time Series.
DATA_TS=xts(x=DATA, order.by = dates) #xts object for relevant period (1997-2020)
DATA_TS=DATA_TS[,2:ncol(DATA_TS)] #Removes redundant column (dates)

# Another plot
dygraph(DATA_TS, main= "Debt!") %>%
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


