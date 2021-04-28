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

#ggplot2 - General graph----------------------------

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

coeff <- as.numeric(1000000)
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

# Another plot (USELESS)
dygraph(DATA_TS[,3:4], main= "Debt!") %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)




# 4. Event Studies --------------------------------------------------------

#We subset by years.
crisis_08=DATA_TS[c("2007","2008", "2009", "2010", "2011")]
  dates_08=seq(from=as.Date("2007-01-01"), to=as.Date("2011-12-01"), by="month")

crisis_20=DATA_TS[c("2019", "2020")]
  dates_20=seq(from=as.Date("2019-01-01"), to=as.Date("2020-12-01"), by="month")

  #We want to plot the year before the beginning of each crisis and the three years afterwards.
T=1:60 #5 years= 60 months.

#We pair dates to each month's position.
crisis_08$T=T
crisis_20$T=T[1:nrow(crisis_20)] #24 obs. for the 2020 crisis.

#Disband xts
crisis_08=(as.data.frame(crisis_08))
crisis_20=(as.data.frame(crisis_20))

#Compensating shorter data frame
crisis_20=rbind(crisis_20, crisis_20[25:60, ])
crisis_20$T=T #60 obs. for the 2020 crisis.

#Graph
<<<<<<< Updated upstream
coeff <- as.numeric(1000000)
rateColor <- "#69b3a2"
=======
coeff <- as.numeric(100000000)#100000000
rateColor <- "#69B3A2"
>>>>>>> Stashed changes
debtColor <- rgb(0.2, 0.6, 0.9, 1)



#2008
<<<<<<< Updated upstream
ggplot(crisis_08, aes(x=T))+
  geom_line( aes(y=as.numeric(AVG_TIIE91_SA)), size=1, color=rateColor) + #2008 TIIE
  geom_line( aes(y=INTERNAL_DEBT_SA/coeff), size=1, color=debtColor) +  #2008 DEBT
  
=======
financial=ggplot(crisis_08, aes(x=Date))+
  geom_line( aes(y=as.numeric(AVG_TIIE91_EE)), size=1, color=rateColor) + #2008 TIIE
  geom_line( aes(y=INTERNAL_DEBT_EE/coeff), size=1, color=debtColor) +  #2008 DEBT

 #Scales
>>>>>>> Stashed changes
  scale_y_continuous(
    
    # Features of the first axis
<<<<<<< Updated upstream
    name = "Interest Rate",
=======
    
    name="Change in values", breaks = seq(-10,10,1.5), n.breaks =2, minor_breaks = waiver(),
>>>>>>> Stashed changes
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Internal Debt")
  ) +
  
<<<<<<< Updated upstream
  #theme_ipsum() +  No est· funcionando esta lÌnea :( 
  
  theme(
    axis.title.y = element_text(color = rateColor, size=13),
    axis.title.y.right = element_text(color = debtColor, size=13)
  ) +
  
  ggtitle("Title") +
 # scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

=======
  scale_x_date(name=NULL, date_labels = "%Y-%m", date_breaks = "4 month", minor_breaks = NULL
               )+
  
  geom_vline(xintercept=as.Date("2008-08-01"), color="black", size=1, linetype="dashed")+
  
  theme(
    axis.title.y = element_text(color = "black", size=13)
  ) +
  
  annotate(geom="text", x=as.Date("2008-12-01"), y=7, 
          label="Financial crisis \nstarts in Mexico", size=4)+
  labs(title="Global Financial Crisis (2008)")+ theme_ipsum_rc()+
  theme(axis.text.x=element_text(angle=60, hjust=1), axis.title.x = element_text(size=13))

financial
>>>>>>> Stashed changes



#2020
ggplot(crisis_20, aes(x=T))+
  geom_line( aes(y=as.numeric(AVG_TIIE91_SA)), size=1, color=rateColor) + #2008 TIIE
  geom_line( aes(y=INTERNAL_DEBT_SA/coeff), size=1, color=debtColor) +  #2008 DEBT
  
  scale_y_continuous(
    # Features of the first axis
<<<<<<< Updated upstream
    name = "Interest Rate",
    
=======
    name=NULL, breaks = seq(-10,10,1.5), n.breaks =8, minor_breaks = NULL,
>>>>>>> Stashed changes
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Internal Debt")
  ) +
  
<<<<<<< Updated upstream
  #theme_ipsum() +  No est· funcionando esta lÌnea :( 
=======
  theme_ipsum() +  #No est· funcionando esta lÌnea :( 
>>>>>>> Stashed changes
  
  theme(
    axis.title.y = element_text(color = rateColor, size=13),
    axis.title.y.right = element_text(color = debtColor, size=13)
  ) +
  
<<<<<<< Updated upstream
  ggtitle("Title") +
  # scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year") +
  theme(axis.text.x=element_text(angle=60, hjust=1))


















=======
  annotate(geom="text", x=as.Date("2020-05-01"), y=5, 
           label="COVID-19 \ncrisis starts", size=4)+
  
  #ggtitle("Title") +
  # scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year") +
  scale_x_date(name=NULL, date_labels = "%Y-%m", date_breaks = "4 month", minor_breaks = NULL) +
  labs(title="The Great Lockdown (2020)")+
  theme(axis.text.x=element_text(angle=60, hjust=1), axis.title.x = element_text(size=13))
#scale_colour_manual(values = colors)
covid

#Gr·fica conjunta
crisis= financial+covid 
crisis
>>>>>>> Stashed changes


# 5. Same Graph -----------------------------------------------------------

#19 months before, 40 months after the cero period in the financial crisis of august, 2008.
two_gether=full_join(crisis_08, crisis_20, by = "T")
two_gether=two_gether[1:40,]



two_gether=ggplot(two_gether, aes(x=T))+
  geom_line( aes(y=as.numeric(AVG_TIIE91_EE.x), color='2008 Interbank Rate'), size=1, show.legend = TRUE) +  #2008 TIIE
  geom_line( aes(y=INTERNAL_DEBT_EE.x/coeff, color="#718EB6"), size=1, show.legend = TRUE) + #2008 DEBT
  geom_line( aes(y=as.numeric(AVG_TIIE91_EE.y), color="#277E6B"), size=1, show.legend = TRUE) + #2020 TIIE
  geom_line( aes(y=INTERNAL_DEBT_EE.y/coeff, color="#305280"), size=1, show.legend = TRUE) + #2020 DEBT
  
  #Legend
  scale_color_manual(values = c(
    '2008 Interbank Rate' <- "#69B3A2",
    '2008 Public Debt' <- "#718EB6",
    '2020 Interbank Rate' = "#277E6B",
   ' 2020 Public Debt' = "#305280")) +
  labs(color = 'Variables')+


  scale_y_continuous(
    # Features of the first axis
    name=NULL, breaks = seq(-10,10,1.5), n.breaks =8, minor_breaks = NULL,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff,  breaks = seq(-1000000000,1000000000,150000000))
  ) +
  theme_ipsum() +  
  
  geom_vline(xintercept=0, color="grey", size=1, linetype="dashed")+
  annotate(geom="text", x=0, y=-15, 
       label="Crisis starts", size=4)+
  
  
  theme(
    axis.title.y = element_text(color = "black", size=13)
    #axis.title.y.right = element_text(color = debtColor, size=13) 
  ) +
  
  #ggtitle("Title") +
  scale_x_discrete() +
  labs(title="Title")+
  theme(axis.text.x=element_text(angle=60, hjust=1), axis.title.x = element_text(size=13), 
        legend.position = "bottom")
#s
two_gether





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


