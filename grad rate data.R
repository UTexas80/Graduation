Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201') #fix RT issue with java
options(java.parameters = "-Xmx8192m") # needed to prevent java out of memory errors


library(ggplot2)
library(ggExtra)
library(ggforce)
library(xlsx)
library(rJava)
library(dplyr)
library(summarytools)
library(forcats)
library(data.table)
library(stringr)
library(tidyr)


grad_data<-nullfile()
grad_data<- read.csv("Raw_Data_01_17_2020.csv") #, stringsAsFactors=FALSE)
grad_data$UNMET_NEED <-as.numeric(as.character(grad_data$UNMET_NEED))
grad_data$REMAINING_NEED <-as.numeric(as.character(grad_data$REMAINING_NEED))
grad_data$GRADUATED <- as.numeric(grad_data$GRADUATED) - 1 
grad_data$MINORITY <- as.numeric(grad_data$MINORITY) - 1 


str(grad_data)

grad_graduated_by_family_AGI <- lm (GRADUATED  ~ FAMILY_AGI, data = grad_data)
grad_graduated_by_family_AGI

plot(x=grad_data$FAMILY_AGI, y=grad_data$GRADUATED,
     main = "Title",
     xlab = "Family AGI",
     ylab = "Graduated",
     pch = 20,
  #   xlim = c(-100000,200000),
  #   ylim = c(-0.4, 1.4),
     cex.main = 0.8)

abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Graduated")
text(2.5, -0.1, cex= 0.8, "Has not Graduated")

abline(grad_graduated_by_family_AGI, 
       lwd = 1.8, 
       col = "steelblue")


