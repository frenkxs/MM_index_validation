dis <- c("Coronary heart disease + heart failure",
"stroke",
"anemia",
"COPD + asthma",
"diabetes",
"cancer",
"dementia",
"heart failure",
"MISSING",
"COPD + asthma",
"Cancer (solid tumor, localized)",
"Cancer (metastatic)",
"Lymphoma/leukemia",
"major stroke",
"MISSING",
"chronic kidney disease",
"diabetes",
"diabetes",
"Cancer",
"COPD + asthma",
"heart failure",
"Myocardial infarction ",
"COPD + asthma",
"MISSING",
"stroke",
"diabetes",
"cancer",
"heart failure",
"smoking",
"heart failure",
"dementia",
"COPD",
"MISSING",
"MISSING",
"diabetes",
"major stroke",
"chronic kidney disease",
"cancer excluding metatatic solid tumor",
"MISSING",
"Metastatic solid tumor",
"MISSING"
)


dis <- tolower(dis)
table(dis)


dis <- 
c("heart0", "heart", 1,
"stroke0", "stroke", 1,
"anemia0", "kidney", 1,
"lung0", "lung", 1,
"diabetes", "diabetes", 1,
"cancer0", "cancer", 1,
"dementia", "dementia", 1,
"heart failure", "heart", 2,
"lung", "lung", 2,
"cancer1", "cancer", 2,
"cancer2", "cancer", 2,
"cancer3", "cancer", 2,
"stroke", "stroke", 2,
"kidney", "kidney", 2,
"diabetes1", "diabetes", 2,
"diabetes", "diabetes", 3,
"cancer0", "cancer", 3,
"lung0", "lung", 3,
"heart failur0e", "heart", 3,
"coronary hear disease1", "heart", 4,
"lung", "lung", 4,
"stroke1", "stroke", 4,
"diabetes", "diabetes", 4,
"cancer0", "cancer", 4,
"heart failure", "heart", 4,
"heart failure", "heart", 5,
"dementia", "dementia", 5,
"lung", "lung", 5,
"diabetes", "diabetes", 5,
"stroke1", "stroke", 5,
"kidney", "kidney", 5,
"cancer2", "cancer", 5,
"cancer4", "cancer", 5
) 

dis_l1 <- dis[seq(1, 99, by = 3)]
dis_l2 <- dis[seq(2, 99, by = 3)]
index <- dis[seq(3, 99, by = 3)]

dis <- data.frame(level1 = dis_l1, level2 = dis_l2, ind = index)
table(dis[, 2])
table(dis[, c(2, 3)])
