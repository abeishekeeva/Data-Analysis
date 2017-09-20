#We create a matrix for given years and number of crimes in each region 

crimes<-matrix(c(1064, 3260, 3541, 833, 2749, 960, 6911, 9297, 2777, 909, 3114, 2974, 810, 2565, 912, 6280, 9168, 2419, 750, 2922, 2954, 756, 2245, 1004, 6176, 10287, 2425, 818, 3104, 2630, 727, 2417, 975, 5801, 10923, 2320, 698, 3937, 2822, 682, 3684, 918, 6007, 11725, 5055, 713, 2797, 2589, 721, 2196, 862, 6201, 12109, 2332, 729, 2744, 2407, 613, 2136, 803, 5632, 11441, 2342, 799, 2524, 2263, 568, 2318, 781, 5308, 10480, 2176, 839, 2622, 2232, 569, 2294, 771, 5454, 9782, 2507)
               , nrow=9, ncol=9, byrow=FALSE)
#crimes
df <- data.frame(regions = c(crimes[1,],crimes[2,],crimes[3,],crimes[4,],crimes[5,],crimes[6,],crimes[7,],crimes[8,],crimes[9,]),
levels = gl(9, 9, 81))

summary(aov(regions~levels, data=df))

#multiple comparison test
t.test(crimes[1,], crimes[2,]) #batken and jalal-abad
t.test(crimes[1,], crimes[3,]) #batken and yssyj-kul
t.test(crimes[2,], crimes[4,]) #jalal-abad and naryn
