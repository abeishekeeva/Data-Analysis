


data <- matrix(c(296.4,7187.3,3110.8,126.1,267.8,9138.5,3046.2,111.4,286,7921.1,2610.1,120.5,492.1,8042,1715.6,114.5,366,8299.1,1130,112.1,307.2,9899,1032.2,119.3,433.3,8157.5,1155.5,131,480.7,9084.6,1070,128,686.6,12421.2,1223.9,125,604.8,16115.2,1486.4,123.6,647.6,19458.5,2445.1,128.7,808.1,21152.2,3186.1,133.9,845.6,12991.6,4012.8,123.9,237.2,12040.6,3966.9,118.6,683.6,12216.9,3253.4,130.4,473.7,10675.4,2180.8,127.3,627.5,10549.9,1498.5,117.8,761.6,10413.9,1471.9,130.7,503.2,10496.6,1583.1,131.6,745.2,10399.8,1552.7,131.7,776.8,9011.8,1634.1,136.7,516.8,11263.9,2097.7,133.7)
,nrow=4, ncol=22)
total <- data[1,] + data[2,] + data[3,] + data[4,]
mining <- data[1,]
manufacture <- data[2,]

manufacture <- manufacture/total
mining <- mining/total


months <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)

t.test(mining, manufacture, conf.level = .95, mu=0.735, alternative="two.sided")

plot(mining, dnorm(mining, mean(mining), sd(mining)))
title(main="Graph of mining production share with respect to the total volume")

plot(manufacture, dnorm(manufacture, mean(manufacture), sd(manufacture)))
title(main="Graph of manufacture production share with respect to the total volume")