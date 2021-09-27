rm(list=ls()) 
library(openxlsx)  
getwd()
work_dir <- "D:Dados/Dropbox/RPI/first semester/ECON 4570/Lab3"
setwd(work_dir)

setwd(choose.dir(work_dir))
# csp <- read.csv(file.choose(), header=T)
cps <- read.xlsx(sprintf("./CPS96_15.xlsx"),sheet=1,startRow=1,colNames=TRUE,rowNames=FALSE)

View(cps)
str(cps)


## loads
cps_96 <- subset(cps, year==1996)
cps_15 <- subset(cps, year==2015)
ahe_96 <- cps_96$ahe
ahe_15 <- cps_15$ahe

sig_96 <- sd(ahe_96)
sig_15 <- sd(ahe_15)
n_96 <- length(ahe_96)
n_15 <- length(ahe_15)


#computing the summary
yb_96 <- mean(ahe_96)
yb_15 <- mean(ahe_15)
sd(ahe_15)
sd(ahe_96)
yb_15
yb_96


## Confidence interval

se_96 <- sd(ahe_96)/sqrt(n_96)
se_15 <- sd(ahe_15)/sqrt(n_15)
se_96
se_15

z <- qnorm(0.05/2)
print(z)

# c <- ("yb_96+z*se_96", "yb_96-z*se_96")

yb_96+z*se_96
yb_96-z*se_96

yb_15-z*se_15
yb_15+z*se_15

# differences between means
se_dif <- sqrt(se_96^2 + se_15^2)  # SE(difference in sample means)
(yb_15-yb_96)+z*se_dif
(yb_15-yb_96)-z*se_dif

difference <- yb_96 - yb_15
difference


#INFLARION
CPI_15= 237 
CPI_96= 156.9
inflation= ((CPI_15-CPI_96)/CPI_96)*100
inflation
inflation_rate= (inflation+100)/100
inflation_rate



#subset
mean(cps$ahe[cps$bachelor==0])


mean(cps$ahe[cps$bachelor==0 & cps$year==1996])
mean(cps$ahe[cps$bachelor==0 & cps$year==2015])


## Confidence interval

se__96 <- sd(ahe_96*inflation_rate)/sqrt(n_96)
se__96

# if the CI is 95%, it consider default and we need only one function.

t.test(ahe_96*inflation_rate)$conf.int

se_dif <- sqrt(se__96^2 + se_15^2)

t.test(ahe_15, ahe_96*inflation_rate)$conf.int

mean(ahe_96*inflation_rate)



#Question d

mean(cps$ahe[cps$bachelor==0 & cps$year==2015])
mean(cps$ahe[cps$bachelor==1 & cps$year==2015])
t.test(cps$ahe[cps$bachelor==0 & cps$year==2015])$conf.int
t.test(cps$ahe[cps$bachelor==1 & cps$year==2015])$conf.int

t.test(cps$ahe[cps$bachelor==1 & cps$year==2015],cps$ahe[cps$bachelor==0 & cps$year==2015])$conf.int



#### e
mean(cps$ahe[cps$bachelor==0 & cps$year==1996]*inflation_rate)
mean(cps$ahe[cps$bachelor==1 & cps$year==1996]*inflation_rate)
t.test(cps$ahe[cps$bachelor==0 & cps$year==1996]*inflation_rate)$conf.int
t.test(cps$ahe[cps$bachelor==1 & cps$year==1996]*inflation_rate)$conf.int

t.test(cps$ahe[cps$bachelor==1 & cps$year==1996]*inflation_rate,cps$ahe[cps$bachelor==0 & cps$year==1996]*inflation_rate)$conf.int

#### F
t.test(cps$ahe[cps$bachelor==0 & cps$year==1996]*inflation_rate,cps$ahe[cps$bachelor==0 & cps$year==2015])$conf.int


t.test(cps$ahe[cps$bachelor==1 & cps$year==1996]*inflation_rate,cps$ahe[cps$bachelor==1 & cps$year==2015],alternative="greater")$conf.int

t.test(cps$ahe[cps$bachelor==0 & cps$year==1996]*inflation_rate, cps$ahe[cps$bachelor==1 & cps$year==1996]*inflation_rate)



### G
t.test(cps$ahe[cps$bachelor==0 & cps$year==1996 & cps$female==1],
       cps$ahe[cps$bachelor==0 & cps$year==1996 & cps$female==0]) # female and male high 1996

t.test(cps$ahe[cps$bachelor==1 & cps$year==1996 & cps$female==1],
       cps$ahe[cps$bachelor==1 & cps$year==1996 & cps$female==0]) # female and male College 1996

t.test(cps$ahe[cps$bachelor==0 & cps$year==2015 & cps$female==1]*inflation_rate,
       cps$ahe[cps$bachelor==0 & cps$year==2015 & cps$female==0]*inflation_rate) # female and male high

t.test(cps$ahe[cps$bachelor==0 & cps$year==1996 & cps$female==1]*inflation_rate,
       cps$ahe[cps$bachelor==0 & cps$year==1996 & cps$female==0]*inflation_rate, conf.level=0.99) # female and male high 1996

t.test(cps$ahe[cps$bachelor==1 & cps$year==2015 & cps$female==1],
       cps$ahe[cps$bachelor==1 & cps$year==2015 & cps$female==0]) # female and male high 2015

t.test(cps$ahe[cps$bachelor==0 & cps$year==1996 & cps$female==1]*inflation_rate,
       cps$ahe[cps$bachelor==1 & cps$year==1996 & cps$female==1]*inflation_rate, conf.level=0.99) # female high and bachelor

t.test(cps$ahe[cps$bachelor==0 & cps$year==1996 & cps$female==0]*inflation_rate,
       cps$ahe[cps$bachelor==1 & cps$year==1996 & cps$female==0]*inflation_rate, conf.level=0.99) # male high and bachelor


### Alternative


cpsb_96 <- subset(cps, bachelor==0)
cpsb_15 <- subset(cps, bachelor==1)


mean(ahe_96,bachelor= 0)
mean(ahe_15, bachelor= 0)
mean(ahe_96, bachelor= 1)
mean(ahe_15, bachelor= 1)

cps_hs_96 <- subset(cps, bachelor==0 & year==1996)
str(cps_hs_96)
mean(ahe_96,bachelor= 1)

ahe_96 <- cps_96$bachelor==0
ahe_96 <- cps_96$ahe
mean(ahe_if_bachelor_96==0)
mean(cps$ahe[cps$bachelor==0 & cps$year==1996])
mean(cps$ahe[cps$bachelor==0 & cps$year==2015])
mean(cps$ahe[cps$bachelor==1 & cps$year==2015])
sd (cps$ahe[cps$bachelor==0 & cps$year==2015])



diff(t.test(cps$ahe[cps$bachelor==1 & cps$year==2015])$conf.int
     -t.test(cps$ahe[cps$bachelor==0 & cps$year==2015])$conf.int)







    


