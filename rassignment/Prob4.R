




production <-c (5,676.6, 136,876.2, 23,211.9, 1,474.1) 
employees <-c (7568, 45784, 14249, 2418)
data <- matrix (c(production, employees), nrow=4, ncol=1) 

chisq.test(data)