

library(qgraph)
library(bootnet)
library(dplyr)
library(readxl)


setwd("C:\\Users\\jhe\\OneDrive - Aegon\\Survey Data\\2021-06 GES June Pulse Survey")

data21 <- read.csv("2021-07-05_GES_Pulse_II_-_June_2021_survey_results.csv")
que_dic <- read_excel("2021 network question code.xlsx", sheet = "2021")

#clean up survey data
data21 <- select(data21, -contains("comment"))

#replace question with short code names
names(data21) <- que_dic$network_var_name[match(names(data21), que_dic$question_id_in_raw_data)]

#remove columns that doesn't have a column name / do not needed in network analysis
keep.cols <- is.na(names(data21))
                 
clean.crs <- data21[!keep.cols] 

data21 <- select(clean.crs, -contains("text"))
data21 <- select(data21, -contains("Unit"))

data21 <- data21[-c(1, 2, 3, 4), ]

#replace missing value -1 with na
#data21[data21 == ""] <- NA

#transform columns from chr to numeric
data21[] <- lapply(data21, function(x) as.numeric(as.character(x)))

#excluding future fit questions from the data19
#ff_que <- que_dic[ !(que_dic$factor %in% c("Customer Centricity", "Agility", "Acting as One", "Accountability")), "network_var_name"]
#data19 <- data19[, which(names(data19) %in% ff_que$network_var_name)]

#Estimate the Network GES19, for all questions excluding future fit questions
Type <- c(rep("g", 12))
Level <- c(rep(1, 12))
Network21 <- estimateNetwork(
  data21, 
  default = "mgm",
  type = Type,
  level = Level,
  criterion = "CV",
  nFolds = 10)

#extract the network graph as a pdf
Labels <- variable.names(data21)

group <- list("Engagement" = c(1, 4, 7, 10), "Wellbeing" = c(2, 5, 8, 11), "Perform & Develop" = c(3, 6, 9, 12))

pdf("Network_full_2021June (0.03-1)-test.pdf")
Qgraph21 <- qgraph(Network21$results$pairwise$wadj,
                 labels = Labels,
                 groups = group,
                 layout = "spring", #"circular", 
                 minimum = 0.03, 
                 maximum = 1,
                 #legend = TRUE,
                 posCol = "blue", 
                 negCol = "purple",
                 title = "GES 2021June / Full population / Min = 0.03 /Max = 1")
dev.off()


qgraph(Qgraph21, overlay = TRUE, posCol = "blue", negCol = "purple", graph = "sig")


# Create edgelist:
dat.3 <- matrix(c(1:8 * 2 - 1, 1:8 * 2), , 2)
dat.3 <- cbind(dat.3, round(seq(0.03, 1, length = 15), 1))

# Create grid layout:
L.3 <- matrix(1:30, nrow = 2)

# Different esize:
qgraph(dat.3, layout = L.3, directed = FALSE, edge.labels = TRUE, esize = 14)

#+++++++++++++++++GES2021 June two groups++++++++++++++++++++++++++++++++++++++++++++++++++




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



