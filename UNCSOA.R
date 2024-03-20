data = read.csv("/Users/henry/2024-srcsc-superlife-inforce-dataset.csv")
inflationrates = read_excel("/Users/henry/srcsc-2024-lumaria-economic-data.xlsx")
applicablerates = subset(inflationrates, ...1==2001 | ...1==2002 | ...1==2003 |...1==2004 |...1==2005 |...1==2006 |...1==2007 |...1==2008 |...1==2009 |...1==2010 |...1==2011 |...1==2012 |...1==2013 |...1==2014 |...1==2015 |...1==2016 |...1==2017 |...1==2018 |...1==2019 |...1==2020 |...1==2021 |...1==2022 | ...1==2023)[,2]
applicablerates = as.numeric(unlist(applicablerates))+1
data2023 = subset(data, X.11==2023)
smokers = subset(data, X.5=="S")
nonsmokers = subset(data, X.5=="NS")
deadsmokers = subset(smokers, X.10==1)
deadnonsmokers = subset(nonsmokers, X.10==1)
smokers2023 = subset(data2023, X.5=="S")
nonsmokers2023 = subset(data2023, X.5=="NS")

dead = subset(data, X.10==1)
nocause = subset(dead, X.14=='')

#adjust payments for inflation to 2023
dead$X.15 = 2023-as.integer(dead$X.11)
for (i in 1:nrow(dead)) {
  dead$X.16[i] = as.integer(dead$X.4[i]) * prod(applicablerates[(22-dead$X.15[i]):22])
}


#total amount of payouts every year, adjusted for inflation
inflationsum_by_year <- aggregate(X.16 ~ X.11, data = dead, sum)
plot(inflationsum_by_year, xlab='Year', ylab='Total Amount (Č)')
ggplot(data=inflationsum_by_year, mapping=aes(x=X.11, y=X.16)) + 
  geom_point() + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
  xlab('Year') +
  ylab('Total Amount (Č)')
###We can see from this graph that the total adjusted claims abnormally spikes in 2020, but using
###the graph below of the number of new policies each year we can see that it's not caused
###by a surge of policy members. We can also see from the barplot of causes of death since 2020
###the proportions are similar to that of all causes of death since 2001 meaning that no surge
###of unrelated causes (such as an epidemic or war) caused the spike of total claims


plot(table(data$X.1), las=2)
recentlydead = subset(data, X.11==2020 | X.11==2021 | X.11==2022 | X.11==2023)
barplot(prop.table(table(recentlydead$X.14)), cex.names = 0.7, las=2)

#total amount of payouts every year, not adjusted for inflation
totalsum_by_year <- aggregate(as.integer(X.4) ~ X.11, data = dead, sum)
plot(totalsum_by_year, xlab='Year', ylab='Total Amount ($)')

#total amount of payouts for each cause of death, adjusted for inflation
inflationsum_by_cause <- aggregate(X.16 ~ X.14, data = dead, sum)
barplot(unlist(inflationsum_by_cause['X.16']), names.arg=names(table(dead$X.14)), cex.names = 0.6, cex.axis=0.6, las=2, xlab='Cause of Death', ylab='Total Amount ($)')

#barplot of cause of death throughout lumaria
barplot(prop.table(table(dead$X.14)), cex.names = 0.7, las=2)

#barplot of cause of death for smokers in lumaria
barplot(prop.table(table(deadsmokers$X.14)), main="Smokers Cause of Death", cex.names = 0.7, las=2)

#barplot of cause of death for nonsmokers in lumaria
barplot(prop.table(table(deadnonsmokers$X.14)), main="Nonsmokers Cause of Death", cex.names = 0.7, las=2)

#barplot of smokers by region
barplot(prop.table(table(smokers$X.8)))

#barplot of nonsmokers by region
barplot(prop.table(table(nonsmokers$X.8)))

#stacked barplot of the cause of death by each region
region1 = subset(dead, X.8==1)
region2 = subset(dead, X.8==2)
region3 = subset(dead, X.8==3)
region4 = subset(dead, X.8==4)
region5 = subset(dead, X.8==5)
region6 = subset(dead, X.8==6)
all_deaths = c(names(prop.table(table(dead$X.14))))
matrix = matrix(c(prop.table(table(factor(region1$X.14, levels=all_deaths))), 
                  prop.table(table(factor(region2$X.14, levels=all_deaths))), 
                  prop.table(table(factor(region3$X.14, levels=all_deaths))), 
                  prop.table(table(factor(region4$X.14, levels=all_deaths))), 
                  prop.table(table(factor(region5$X.14, levels=all_deaths))), 
                  prop.table(table(factor(region6$X.14, levels=all_deaths)))), ncol=17, byrow=TRUE)
colorlist = c("blue", 
           "orange", 
           "red", 
           "yellow", 
           "purple", 
           "pink")
barplot(matrix, names.arg=names(table(dead$X.14)), cex.names = 0.7, las=2, beside = FALSE, col = colorlist)
legend(x=15,y=1.2, cex=0.5, fill=colorlist, legend=c("region 1", "region 2", "region 3", "region 4", "region 5", "region 6"))
