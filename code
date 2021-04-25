library(readxl)
library(ggplot2)
library(dplyr)
#install.packages("lubridate")
library(lubridate)
library(hrbrthemes)
library(seasonal)

#Data---------------------------------
setwd("C:/Users/secun/OneDrive/Documentos/6to/Money/Data Vizualization Project")
data=read_excel("DEBT85-20.xlsx", range = "A147:G434", col_names = FALSE)
#db <- xts(x = db, order.by= seq(as.Date("1995-03-01", "%Y-%m-%d"), length=286, by="months"))

colnames(data)=c("date", "internal debt","total net debt", "AVG_TIIE28", "AVG_TIIE91", "cetes_28", "INPC")

plot(x=data$date,y=data$`internal debt`,type="l")
plot(x=data$date,y=data$AVG_TIIE91,type="l")

data=data[,-c(3, 4, 6)]

#Deflactando------------------------

data[,5]=data$INPC/99.90910
data$`internal debt`=data$`internal debt`/data$...5
data$AVG_TIIE91=data$AVG_TIIE91/data$...5
data=data[,-c(1,4,5)]

#Desestacionalizar
Data <- ts(coredata(data),frequency=12,start=c(1997,1))
Data <- seas(as.ts(Data))
Data<-cbind(Data$internaldebt$data[,3], Data$AVG_TIIE91$data[,3])
colnames(Data) <-c("internal debt","interest rate") 
plot(Data, main="Internal rate and interest rate",xlab="Año",ylab="$")
Data<-as.data.frame(Data)

#Gráfica----------------------------
Date<-seq(as.Date("1997-01-01"), as.Date("2020-12-01"), by="month")
Data<-cbind(Date, Data)

# Start with a usual ggplot2 call:
# ggplot(db, aes(x=date, y=as.numeric(`AVG_TIIE28`))) +
#   
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

ggplot(Data, aes(x=Date))+
  geom_line( aes(y=as.numeric(`interest rate`)), size=1, color=rateColor) + 
  geom_line( aes(y=`internal debt`/coeff), size=1, color=debtColor) +  # Divide by 10 to get the same range than the temperature

  scale_y_continuous(
     # Features of the first axis
    name = "Interest Rate",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Internal Debt")
  ) +
  
theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = rateColor, size=13),
    axis.title.y.right = element_text(color = debtColor, size=13)
  ) +
  
  ggtitle("Title") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
  
