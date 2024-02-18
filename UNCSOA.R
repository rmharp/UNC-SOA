data = read.csv("/Users/henry/2024-srcsc-superlife-inforce-dataset.csv")
smokers = subset(data, X.5=="S")
nonsmokers = subset(data, X.5=="NS")
deadsmokers = subset(smokers, X.10==1)
deadnonsmokers = subset(nonsmokers, X.10==1)

dead = subset(data, X.10==1)

#barplot of cause of death throughout lumaria
barplot(prop.table(table(dead$X.14)))


#stacked barplot of the cause of death by each region
region1 = subset(dead, X.8==1)
region2 = subset(dead, X.8==2)
region3 = subset(dead, X.8==3)
region4 = subset(dead, X.8==4)
region5 = subset(dead, X.8==5)
region6 = subset(dead, X.8==6)
matrix = matrix(c(prop.table(table(region1$X.14)), 
                  prop.table(table(region2$X.14)), 
                  prop.table(table(region3$X.14)), 
                  prop.table(table(region4$X.14)), 
                  prop.table(table(region5$X.14)), 
                  prop.table(table(region6$X.14))), ncol=6, byrow=TRUE)
matrix = t(matrix)
barplot(matrix, beside = FALSE, col = c("blue", 
                                       "orange", 
                                       "red", 
                                       "yellow", 
                                       "purple", 
                                       "pink"))
        