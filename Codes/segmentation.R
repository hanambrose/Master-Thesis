library(dplyr)
library(ggplot2)
library(rugarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)

#function that return the change point given dataframe
seq_change <- function(data) {
  df2 <- data %>% mutate(cumsum = cumsum(data$x),
                         ID = row_number(), #ID is reset every sequence for calculation but k stays the same to track change point
                         total =sum(data$x)) %>% 
    mutate(length_df = length(ID)) %>% 
    mutate(frac = ID/length(ID)) %>% 
    mutate(temp1 = frac * total) %>% 
    mutate(absolute = abs(cumsum - temp1)) %>% 
    mutate(T = absolute / (sqrt(length(ID))))
  #print(df2)
  #maxplot <- ggplot(df2, aes(x = k, y = T)) + geom_col()
  #print(maxplot)
  
  #Find the max value of T and return k where the max value is, this is the cutoff point
  #if it is tie return the smaller k
  maxk <- df2$k[max.col(t(df2$T), ties.method = "first")]
  maxT <- df2$T[max.col(t(df2$T), ties.method = "first")]
  
  return (list(maxT=maxT,maxk=maxk))
}


recurs_seq <- function(data){
  change_point <- seq_change(data)
  #print(length(data$k))
  #if (change_point$maxT >= 1.23/length(data$k)) { # Kolmogorov critical values
  if (change_point$maxT >= 1.23) { # Kolmogorov critical values
    #record the change point location
    locations <<- c(locations,change_point$maxk)
    left_cut <- subset(data, k<=change_point$maxk) %>% select(k, x)
    #print(left_cut)
    recurs_seq(left_cut)
    right_cut <- subset(data, k>change_point$maxk) %>% select(k, x)
    #print(right_cut)
    recurs_seq(right_cut)
  }
}

#####--------- Running the expirement m times ----------
# array that stores the location of change
locations <- c()

# N <- 50
# var <- 0.1
# sd = sqrt(var)
# N1 <- floor(N/3)
# N2 <- floor(2*N/3)-floor(N/3)
# N3 <- N - floor(2*N/3)

gold <- read.table("gold in sterling 1516.csv", header = TRUE, sep = ',')
goldts <- zoo(gold$price, as.Date(as.character(gold$Date), format = c("%d-%b-%y")))
gold_num <- coredata(goldts)

iter = 50
for (i in 1:iter){
  print(paste("i = ", i))
  # #generate n independent normal random variables
  # #first sequence
  # x1 <- rnorm(N1, mean = 2, sd = sd)
  # #second sequence
  # x2 <- rnorm(N2, mean = 1, sd = sd)
  # #third sequence
  # x3 <- rnorm(N3, mean = 0, sd = sd)
  # #combine 3 sequences
  x<- gold_num
  #create a dataframe
  df <- data.frame(x) %>% mutate(k = row_number())
  
  recurs_seq(df)
}

locations.freq = table(locations)
loc_df <- as.data.frame(locations)

iterations <- paste("Iterations = ", iter) 
variables_num <- paste("Sample_Size = ", N)
variance <- paste("Variance = ", var)
ggplot(loc_df,aes(x = locations)) + geom_histogram(aes(x = locations,y = ..count..), binwidth = 10, alpha =0.5)+
  #geom_density(aes(x = locations, y = ..count..), bw = 1, adjust=1, color = 'black')+
  #ggtitle(paste0(iterations,"\n",variables_num,"\n",variance)) +
  xlab("Location") + ylab("Frequency") + theme_light()