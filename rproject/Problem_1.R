data <- matrix(
  c(2001,2046,35916,9167.7,90.1,1278.7,15684.7,66.9,202.8,2002,1784,36168,9464.1,115.7,1657.3,14447.7,114.2,234,2003,1780,40430,10397,147,1618.5,17887.4,144.6,331.6,2004,1895,47784,11901.4,175.6,1346.9,24176.6,213.5,435.9,2005,1822,46424,0,210.3,3736.1,18786.6,251.2,592.6,2006,1882,59911,0,335.6,8638.8,18282.4,344.4,1027.2,2007,2167,62941,0,436.8,8517,19250.5,537.2,1351,2008,2721,72015.3,0,866.2,15879.4,30890.4,612.4,1736.5,2009,2679,64703,0,660.9,19398.5,34942.8,316.1,1226.1,2010,2611,60584,0,666.1,23313.3,46726.4,364.8,1520,2011,2586,56721,0,849.2,18934.1,60025.4,468.4,1833.2,2012,2601,58580,0,590.7,38641.6,45333.2,581,2332.4,2013,2767,59966,0,964.5,44696.6,61038.2,519.7,2394,2014,3022,59800,0,727.1,59439.2,64122.6,438,2209.2)
,nrow = 9, ncol = 14, byrow=F)
data

#testing correlation between inflow and export
cor.test(data[5,], data[8,])

#other indicators with export
res <- cor.test(data[2,], data[8,])
res$p.value
res$estimate
res <- cor.test(data[3,], data[8,])
res$p.value
res$estimate
res <- cor.test(data[4,], data[8,])
res$p.value
res$estimate
res <- cor.test(data[5,], data[8,])
res$p.value
res$estimate
res <- cor.test(data[6,], data[8,])
res$p.value
res$estimate
res <- cor.test(data[7,], data[8,])
res$p.value
res$estimate
res <- cor.test(data[9,], data[8,])
res$p.value
res$estimate

#anova difference in means
df <- data.frame(
  indicators = c(data[2,],data[3,],data[4,],data[5,],data[6,],data[7,],data[8,],data[9,])
  ,levels = gl(8, 14, 8*14)
)
summary(aov(indicators~levels, data = df))

#least squares line
export = data[8,]
inflow = data[6,]
least_sq_line <- lm (export~inflow) # inflow and import
least_sq_line

#graph 
plot(export~inflow,
     xlab = "inflow",
     ylab = "export")
abline(least_sq_line)

#testing the normality of residuals
inflow_predicted <- coefficients(least_sq_line)[1] + coefficients(least_sq_line)[2]*inflow
residuals = inflow - inflow_predicted
shapiro.test(residuals)

# Confidence interval for the export
predict(least_sq_line,data.frame(inflow=800),interval="confidence",level=0.96)

# Time series (*)
years <- 1:length(export)
least_sq_line2 <- lm(export~years)
inflow_pr<-coefficients(least_sq_line2)[1]+coefficients(least_sq_line2)[2]*(length(export)+1)
inflow_pr
# Confidence interval
predict(least_sq_line,data.frame(inflow=inflow_pr),interval="confidence",level=0.96)
