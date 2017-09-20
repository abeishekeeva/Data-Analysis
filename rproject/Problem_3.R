
local_budget<-c(6919794.6, 8848332.4, 12575313.8, 13973300.5, 15250955.4, 21604781.1, 23699032.6, 21236908.6, 16108534.2)

export<-c(891100, 1321000, 1855600, 1673000, 1755900, 2242200, 1927600, 2006800, 1883700)
fdi<-c(891083.1, 1321059, 1855566.4, 1673040.5, 1755924.5, 2242166.4, 1927623.3, 2006849.7, 1883733.5)
employed_population<-c(2096.1, 2152.7, 2184.3, 2216.4, 2243.7, 2277.7, 2286.4, 2263.0, 2302.7)
industrial_volume<-c(54423900, 59823300, 89235700, 99031500, 126588100, 164623900, 136967600, 169520000, 171108900)
microcrediting<-c(3959530.3, 6167143.5, 10206543.3, 11821298.2, 15430613.2, 22755380.2, 23356956.6, 27533900.5, 30523409.3) 
avg_month_salary<-c(3.27, 3.866, 5.378, 6.161, 7.189, 9.304, 10.726, 11.341, 12.285)
commissioning<-c(579.8, 691.2, 828.7, 875.8, 735, 865.2, 850.5, 937, 1082.3)
cpi<-c(105.6, 110.2, 104.3, 105.6, 110.2, 124.5, 106.8, 108, 116.6, 102.8, 106.6, 107.5)
import<-c(1931100, 2788600, 4072400, 3040200, 3222800, 4261200, 5576300, 5987000, 5734700)
crimes<-c(31.392, 29.151, 29.519, 29.715, 35.528, 30.520, 28.847, 27.217, 27.070) 
unemployment<-c(8.3, 8.2, 8.2, 8.4, 8.6, 8.5, 8.4, 8.3, 8)
educ_inst<-c(4.999, 5.052, 5.129, 5.250, 5.372, 5.438, 5.517, 5.637, 5.774)
retail_trade_to<-c(70875600, 89866900, 132285300, 129697400, 136087500, 177420800, 202415300, 23375600, 277722900)
output_agric<-c(71997.3, 88830.6, 111567.6, 110681.0, 114781.9, 149004.9, 167114.8, 171412.4, 195236.3)

#Building a model

budget.lm<-lm(local_budget~microcrediting+employed_population+industrial_volume+output_agric+fdi+import)
summary(budget.lm)

#Testing presence of multicollinearity 
titles <- c("loc.budg","microcred.","emp.pop","ind.v", "out.aggr","fdi","import")
mtrx <- matrix(c(local_budget, microcrediting, employed_population, industrial_volume, output_agric, fdi, import)
               , nrow=7, ncol=9, byrow = T)
df <- data.frame()
for (i in 1:length(titles)){
  for (j in 1:length(titles)){
    if (i == j){
      df[titles[i], titles[j]] = 1
    }
    else{
      res <- cor.test(mtrx[i,], mtrx[j,])
      df[titles[i], titles[j]] = round(as.numeric(res$estimate), 2)
      df[titles[j], titles[i]] = round(as.numeric(res$estimate), 2)
    }
  }
}
df
#Prediction interval (10,946,586, 32,650,452.0), real budget=21,236,908.6
newdata=data.frame(microcrediting=27533900.5, employed_population=2263.0, industrial_volume=169520000, output_agric=171412.4, fdi=2006849.7, import=5987000)
predict(budget.lm, newdata, interval="predict", level=0.9)

#reduced model
budget.lm_reduced <- lm(local_budget~fdi)
#testing significance
summary(budget.lm)
summary(budget.lm_reduced)