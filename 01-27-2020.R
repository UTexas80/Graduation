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
library("FactoMineR")
library("factoextra")

setwd("O:\\DEPART~1\\Research\\PROFIL~1")


need_data<-nullfile()

major_data<-nullfile()

need_data<- read.csv("1819UnmetNeed.csv", stringsAsFactors=FALSE)


need_data$Rural_vs_Urban<-as.factor(need_data$Rural_vs_Urban) 
need_data$ISIR_First_Gen<-as.factor(need_data$ISIR_First_Gen) 
need_data$Pell_Grant_Eligibility_Flag<-as.factor(need_data$Pell_Grant_Eligibility_Flag) 
need_data$UGA_Gender<-as.factor(need_data$UGA_Gender) 
need_data$Dependency_Status<-as.factor(need_data$Dependency_Status) 
need_data$Admissions_Student_Type<-as.factor(need_data$Admissions_Student_Type) 


need_data.active <- need_data[1:20000,c(50,17,19,22,27,56 )   ]

head(need_data.active)
str(need_data.active)
for (i in 1:5) {
  plot(need_data.active[,i], main=colnames(need_data.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}

res.mca <- MCA(need_data.active, graph = FALSE)
print(res.mca$var)
for (i in 1:13) {
  print(res.mca[i])
}



fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

# get the results for variable categories
var <- get_mca_var(res.mca)
var
var$contrib


fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


# Color by cos2 values: quality on the factor map - closer to 1 = better represented
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(res.mca, choice = "var", axes = 1:2)


# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)


fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)


res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]


head(var$cos2, 5)
