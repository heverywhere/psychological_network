library(qgraph)
library(bootnet)
library(dplyr)
library(readxl)


setwd("C:\\Users\\jhe\\OneDrive - Aegon\\Survey Data\\GES19 raw data")

data19 <- read.csv("2021-05-21T14-21-30_Global_Employee_Survey_2019_survey_results.csv")
que_dic <- read_excel("2019 network question code.xlsx", sheet = "2019")

#clean up survey data
data19 <- select(data19, -contains("comment"))
data19 <- select(data19, -contains("9913.za"))
data19 <- select(data19, -contains("9918.so"))
data19 <- select(data19, -contains("9922.vh"))
data19 <- data19[,-1]

#replace question with short code names
names(data19) <- que_dic$network_var_name[match(names(data19), que_dic$question_id_in_raw_data)]
data19 <- data19[ -(1:4) , 1:length(que_dic$network_var_name)]

#replace missing value -1 with na
data19[data19 == -1] <- NA

#transform columns from chr to numeric
data19[] <- lapply(data19, function(x) as.numeric(as.character(x)))

#excluding future fit questions from the data19
ff_que <- que_dic[ !(que_dic$factor %in% c("Customer Centricity", "Agility", "Acting as One", "Accountability")), "network_var_name"]
data19 <- data19[, which(names(data19) %in% ff_que$network_var_name)]

#Estimate the Network GES19, for all questions excluding future fit questions
Type <- c(rep("g", 36))
Level <- c(rep(1, 36))
Network19 <- estimateNetwork(
  data19, 
  default = "mgm",
  type = Type,
  level = Level,
  criterion = "CV",
  nFolds = 10)

#extract the network graph as a pdf
Labels <- variable.names(data19)

pdf("Network_full_2019-2.pdf")
Qgraph19 <- qgraph(Network19$results$pairwise$wadj,
                 labels = Labels,
                 #groups = Group,
                 layout = "spring", minimum = 0.05, maximum = 0.8,
                 title = "GES 2019 / Full population / Min = 0.05 /Max = 0.8")
dev.off()

#+++++++++++++++++GES2018 data as conparison++++++++++++++++++++++++++++++++++++++++++++++++++

data18 <- read.csv("C:\\Users\\jhe\\OneDrive - Aegon\\RStudio\\GES\\Network GES18\\GES_2018_rdata_network.csv")
data18 <- data18[, -1]
data18 <- data18[, which(names(data18) %in% ff_que$network_var_name)]

#clean rows without any scores
data18['missing'] = apply(data18, 1, function(x) sum(is.na(x)))
data18 <- data18[data18$missing <36, ]
data18 <- data18[, 1:36]


#estimate network
Labels <- variable.names(data18)


#Network GES18, all questions
Type <- c(rep("g", 36))
Level <- c(rep(1, 36))
Network18 <- estimateNetwork(
                    data18, 
                    default = "mgm",
                    type = Type,
                    level = Level,
                    criterion = "CV",
                    nFolds = 10)


pdf("Network_full_2018.pdf")
Qgraph18 <- qgraph(Network$results$pairwise$wadj,
       labels = Labels,
       groups = Group,
       layout = "spring", minimum = 0.05, maximum = 0.8,
       title = "GES 2018 / Full population / Min = 0.05 /Max = 0.8")
dev.off()


Layout <- Qgraph$layout
Color <- Qgraph$graphAttributes$Graph$color


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#threshold & estimation
Network.bonf <- estimateNetwork(
                   rdata, 
                   default = "pcor",
                   threshold = "bonferroni")

Network.glasso <- estimateNetwork(
                  rdata, 
                  default = "EBICglasso",
                  threshold = TRUE,
                  tuning = 0.5)

Layout1 <- Qgraph$layout
layout(t(1:2))
plot(Network.bonf, layout = Layout1, cut = 0)
plot(Network.glasso, layout = Layout1, cut = 0)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Stability & accuracy

boot_acc <- bootnet(Network, nBoot = 100, default = "EBICglasso", nCores = 8)                



boot_sta <- bootnet(Network, nCores = 8, nBoots = 100, type = "case", 
                        statistics = c("strength", "closeness", "betweenness"))

pdf("Network_acc.pdf")
plot(boot_acc, order = "sample", plot = "interval", split0 = TRUE)

dev.off()

plot(boot_sta, statistics = c("strength", "closeness", "betweenness"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    Network GES18, questions excluding FF 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rdata_woff <- rdata
rdata_woff[,5:15] <- NULL

Type_woff <- c(rep("g", 34))
Level_woff <- c(rep(1, 34))
Network_woff <- estimateNetwork(
  rdata_woff, 
  default = "mgm",
  type = Type_woff,
  level = Level_woff,
  criterion = "CV",
  nFolds = 10)




Engagement2 <- c(1,2,3,4)
Management2 <- c(6,7,8,9,10,11,25,26,27,28,29,30)
Leadership2 <- c(12,13,14,15)
ControlEnvironment2 <- c(16,17,18)
Diversity2 <- c(19)
Empowerment2 <- c(20,21,22,23)
Inclusion2 <- c(24)
SocialResponsibility2 <- c(31)
Compensation2 <- c(32)
WLblend2 <- c(33,34)
Advocacy2 <- c(5)

Group_woff <- list(Engagement2, Advocacy2,Management2, Leadership2, 
              ControlEnvironment2, Empowerment2, SocialResponsibility2, 
              Compensation2, WLblend2, Diversity2, Inclusion2)

Labels_woff <- Labels[-(5:15)] 
Layout_woff <- Layout[-(5:15),] 
Color_woff <- Color[-3]


pdf("Network_full_2018_woff.pdf")
Qgraph_woff <- qgraph(Network_woff$results$pairwise$wadj,
                 labels = Labels_woff,
                 layout = Layout_woff,
                 groups = Group_woff,
                 color = Color_woff,
                 minimum = 0.05,
                 maximum = 0.8,
                 title = "GES 2018 / Full population / Min = 0.05 / Max = 0.8")
dev.off()

pdf("Network_full_2018_woff0.pdf")
Qgraph_woff <- qgraph(Network_woff$results$pairwise$wadj,
                      labels = Labels_woff,
                      layout = "spring",
                      groups = Group_woff,
                      color = Color_woff,
                      minimum = 0.05,
                      maximum = 0.8,
                      title = "GES 2018 / Full population / Min = 0.05 / Max = 0.8")
dev.off()


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    Network GES17, ALL questions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data17 <- read.csv("GES_2017_rdata_network.csv")
rdata17 <- data17[,2:43]


#estimate network
Type17 <- c(rep("g", 42))
Level17 <- c(rep(1, 42))
Network17 <- estimateNetwork(
  rdata17, 
  default = "mgm",
  type = Type17,
  level = Level17,
  criterion = "CV",
  nFolds = 10)

#Preparation for ploting the network: getting the groups, layout and color from GES18 network
Labels17 <- variable.names(rdata17)

Engagement17 <- c(1,2,3,4)
FF17 <- c(5,6,7,8,9,10,11,12,13,14,15)
Management17 <- c(17,18,19,20,21,30,31,32,33,34,35)
Leadership17 <- c(22,40)
ControlEnvironment17 <- c(23,24,25)
Empowerment17 <- c(26,27,28,29)
SocialResponsibility17 <- c(36)
Compensation17 <- c(37)
WLblend17 <- c(38,39)
Advocacy17 <- c(16)
FFAwareness17 <- c(41)
Other17 <- c(42)

Group17 <- list(Engagement17, Advocacy17, FF17, Management17, Leadership17, 
              ControlEnvironment17, Empowerment17, SocialResponsibility17, 
              Compensation17, WLblend17, FFAwareness17, Other17)


Color17 <- Color
Layout17 <- Layout[1:42,]
Layout17[20:22,] <- Layout[21:23,]
Layout17[23:25,] <- Layout[27:29,]
Layout17[26:29,] <- Layout[31:34,]
Layout17[30:39,] <- Layout[36:45,]
Layout17[40,] <- Layout[25,]
Layout17[41,] <- Layout[24,]
Layout17[41,1] <- 0.36
Layout17[41,2] <- (-0.01)
Layout17[42,] <- Layout[35,] # get the approximate value for the positiion
Layout17[42,1] <- (-0.40) #adjust the position a bit
Layout17[42,2] <- 0.04 #adjust the position a bit



pdf("Network_full_2017.pdf")
Qgraph17 <- qgraph(Network17$results$pairwise$wadj,
                      labels = Labels17,
                      layout = Layout17,
                      groups = Group17,
                      color = Color17,
                      minimum = 0.05,
                      maximum = 0.8,
                      title = "GES 2017 / Full population / Min = 0.05 / Max = 0.8")
dev.off()



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    Network GES17, questions excluding FF 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rdata17_woff <- rdata17
rdata17_woff[,5:15] <- NULL

Type17_woff <- c(rep("g", 31))
Level17_woff <- c(rep(1, 31))
Network17_woff <- estimateNetwork(
  rdata17_woff, 
  default = "mgm",
  type = Type17_woff,
  level = Level17_woff,
  criterion = "CV",
  nFolds = 10)


Engagement17_woff <- c(1,2,3,4)
Management17_woff <- c(6,7,8,9,10,19,20,21,22,23,24)
Leadership17_woff <- c(11,29)
ControlEnvironment17_woff <- c(12,13,14)
Empowerment17_woff <- c(15,16,17,18)
SocialResponsibility17_woff <- c(25)
Compensation17_woff <- c(26)
WLblend17_woff <- c(27,28)
Advocacy17_woff <- c(5)
FFAwareness17_woff <- c(30)
Other17_woff <- c(31)

Group17_woff <- list(Engagement17_woff, Advocacy17_woff, Management17_woff, Leadership17_woff, 
                 ControlEnvironment17_woff, Empowerment17_woff, SocialResponsibility17_woff, 
                 Compensation17_woff, WLblend17_woff, FFAwareness17_woff, Other17_woff)


Labels17_woff <- Labels17[-(5:15)] 
Layout17_woff <- Layout17[-(5:15),] 

Color17_woff <- Color17[-3]


pdf("Network_full_2017_woff.pdf")
Qgraph17_woff <- qgraph(Network17_woff$results$pairwise$wadj,
                      labels = Labels17_woff,
                      layout = Layout17_woff,
                      groups = Group17_woff,
                      color = Color17_woff,
                      minimum = 0.05,
                      maximum = 0.8,
                      title = "GES 2018 / Full population / Min = 0.05 / Max = 0.8")
dev.off()


pdf("Network_full_2017_woff0.pdf")
Qgraph17_woff <- qgraph(Network17_woff$results$pairwise$wadj,
                        labels = Labels17_woff,
                        layout = "spring",
                        groups = Group17_woff,
                        color = Color17_woff,
                        minimum = 0.05,
                        maximum = 0.8,
                        title = "GES 2018 / Full population / Min = 0.05 / Max = 0.8")
dev.off()

