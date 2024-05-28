bugfixes <- read.table('C:/Users/USER/Documents/FH Technikum/R Tutorials/bugfixes_complete.csv', header = TRUE, 
                       stringsAsFactors = TRUE)

data <- bugfixes[, c("dauer", "programmierer")]

summary_data <- summary(data)
print(summary_data)

# The programmierer varies from 120.0 to 207.0 with a mean of 169.8. We have 'Eckkrammer': 7, 'Mandl': 7
# 'Meyer': 7

# boxplot(data$dauer, horizontal = TRUE)

# The distribution does not seem to be symmetric, since the median is shifted to the right which means that
# the distribution is left-skewed.


plot(data$dauer ~ data$programmierer)
#data <- na.omit(data)
 
library(ggplot2)

datav <- ggplot(data) + aes(x = programmierer, y = dauer, color = programmierer) + geom_jitter()

print(datav)

# ANOVA 
#  We want to apply (use) ANOVA to answer the question "Is there a difference between the programmers"?

library("lattice")

data_dot <- dotplot(dauer ~ programmierer, data = data)
print(data_dot)

library("car")
leven <- leveneTest(dauer ~ programmierer, data = data)
print(leven)

# Since the p-value is larger than the default p-value which is 0.05. Therefore we cannot reject the hypothesis
# that the variance are equal between the programmers

## ANOVA
onew <- oneway.test(dauer ~ programmierer, data=data, var.equal = TRUE)
print(onew)

# After conducting the oneway test, we see a very small p-value of 0.000168 which is smaller than 0.05 
# which means `HO {There is no difference between the programmers}`. Therefore, my conclusion is that at least
# there is one difference between the programmers

