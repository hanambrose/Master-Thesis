library(dplyr)
library(ggplot2)
library(rugarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)

seq_change <- function(data, kappa, sd) {
  
  df2 <- data %>% mutate(cumsum = cumsum(data$x),
                         ID = row_number(), #ID is reset every sequence for calculation but k stays the same to track change point
                         total =sum(data$x)) %>%
    mutate(length_df = length(ID)) %>%
    mutate(frac = ID/length(ID)) %>%
    mutate(temp1 = frac * total) %>%
    mutate(absolute = abs(cumsum - temp1)) %>%
    mutate(T1 = absolute / (sqrt(length(ID)))) %>%
    mutate(T2 = T1/((frac * (1-frac))^kappa)) %>%
    mutate (T = T2/sd)
  
  df2$T[is.na(df2$T)] <- 0
  print(df2)
  
  #Find the max value of T and return k where the max value is, this is the cutoff point
  #if it is tie return the smaller k
  maxk <- df2$k[max.col(t(df2$T), ties.method = "first")]
  maxT <- df2$T[max.col(t(df2$T), ties.method = "first")]
  
  maxk_plot <- paste("Max_k = ", maxk,",")
  maxT_plot <- paste("Max_T = ", round(maxT,2))
  kp <- paste("Kappa = ", kappa)
  
  maxplot <- ggplot(df2, aes(x = k, y = T)) + 
    geom_col(width = 0.5, position = position_dodge(0.1),fill="black")+
    theme_light()+
    ggtitle("Weighted CUSUM process with kappa =0.45 using log return of GME data")
  
  print(maxplot)
  
  return (list(maxT=maxT,maxk=maxk))
}

#### Running ###

kappa <- 0.45
#gold <- read.table("gold in sterling 1516.csv", header = TRUE, sep = ',')
gold <- read.table("GME.csv", header = TRUE, sep = ',')
goldts <- zoo(gold$price, as.Date(as.character(gold$Date), format = c("%d-%b-%y")))
# gold_num <- coredata(goldts)
# x<- gold_num

#log_return 
gold_rets <- log(goldts/lag(goldts, -1))
gold_ret_num <- coredata(gold_rets)
#x <- gold
x<- gold_ret_num

#create a dataframe
df <- data.frame(x) %>% mutate(k = row_number())
#colnames(df)[2] <- "x"

#sample mean
y_bar <- mean(gold_ret_num)
#sample variance (population variance)
s <- mean((gold_ret_num - y_bar)^2)
#sample sd
sd <- sqrt(s)

seq_change(df, kappa, sd)