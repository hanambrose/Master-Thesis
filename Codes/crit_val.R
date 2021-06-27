library(dplyr)
library(ggplot2)

seq_change <- function(data, kappa) {
  df2 <- data %>% mutate(cumsum = cumsum(data$x),
                         ID = row_number(), #ID is reset every sequence for calculation but k stays the same to track change point
                         total =sum(data$x)) %>%
    mutate(length_df = length(ID)) %>%
    mutate(frac = ID/length(ID)) %>%
    mutate(temp1 = frac * total) %>%
    mutate(absolute = abs(cumsum - temp1)) %>%
    mutate(T1 = absolute / (sqrt(length(ID)))) %>%
    mutate(T = T1/((frac * (1-frac))^kappa)) # kappa = 0.1, 0.45
  
  df2$T[is.na(df2$T)] <- 0
  
  #print(df2)
  #print(df2[1:5,])
  
  #Find the max value of T and return k where the max value is, this is the cutoff point
  #if it is tie return the smaller k
  maxk <- df2$k[max.col(t(df2$T), ties.method = "first")]
  maxT <- df2$T[max.col(t(df2$T), ties.method = "first")]
  
  return (list(maxT=maxT,maxk=maxk))
}

N <- 500
var <- 1
sd <- sqrt(var)
kappa = 0

max_k <- c()
max_T <- c()

iter = 1000
for (i in 1:iter){
  
  print(paste("i = ", i))
  
  x <- rnorm(N, mean = 0, sd = 1)
  #create a dataframe
  df <- data.frame(x) %>% mutate(k = row_number())
  change_point <- seq_change(df, kappa)
  max_k <<- c(max_k,change_point$maxk)
  max_T <<- c(max_T,change_point$maxT)
}

iterations <- paste("Iterations = ", iter)
sample_size <- paste("Sample Size = ", N)
variance <- paste("Variance = ", var)
kp <- paste("Kappa = ", kappa)

max_T <- as.data.frame(max_T)

#hist(max_T$max_T)

max_T.q <- round(quantile(max_T$max_T, probs = c(0.99, 0.95, 0.90)),3)

ggplot(max_T, aes(max_T)) + stat_ecdf(geom = "step")+
  ggtitle(paste0("Empirical Cumulative Distribution Function ","\n",iterations,"\n",sample_size,"\n",kp))+
  #geom_vline(aes(xintercept=max_T.q),linetype = "dashed")+
  geom_hline(yintercept=c(0.99, 0.95, 0.90), linetype='dashed')+
  geom_vline(xintercept=max_T.q,linetype = "dashed")+
  scale_x_continuous(breaks = max_T.q,labels = max_T.q)+
  scale_y_continuous(breaks = c(0.25, 0.50, 0.75,0.99, 0.95, 0.90),labels = c(0.25, 0.50, 0.75,0.99, 0.95, 0.90))+
  labs(x= "Maximum of T",y = "Fn(Maximum of T)")+
  theme(axis.text.x=element_text(angle=45,hjust=1), panel.background = element_rect(fill = 'white', colour = 'black'))

library(EnvStats)

crits <- qemp(p = c(0.990, 0.95, 0.90), obs = max_T$max_T)