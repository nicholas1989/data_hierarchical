# In the ÖAMTC summer tire test, tires in two different dimensions (175 and 195) were evaluated in 
# 12 different categories (grades from 0.5 to 5.5; lower is better). 
# You can find the results in the data set sommerreifen.txt in Moodle.

# 1. visualize the multivariate data set!

# 2. can the 12 categories be summarized? How many components make sense? Try out 2or 3 different solutions!

# 3. create a biplot to graphically display the results of the PCA and interpret it!

# 4. are there significant differences between the two tire dimensions with regard to (one of) the new variable(s) (= components)?

# the two tire dimensions? Carry out a suitable statistical test!

if (!requireNamespace("psych", quietly=TRUE))install.packages("psych")
if (!requireNamespace("car", quietly=TRUE))install.packages("car")


library("psych")
library("car")

daten <- read.table('C:/Users/USER/Documents/FH Technikum/R Tutorials/sommerreifen.txt', sep=";", header=TRUE)

head(daten)

#....

items <- daten [-c(1:2)]
cm <- colMeans(items)
S <- cov(items)
d <- apply(items, 1, function(x)
    t(x-cm) %*% solve(S) %*% (x-cm))

print(d)

layout(matrix(1:15, nc=3))
sapply(colnames(items), function(x) {
    qqnorm(items[[x]], main=x)
    qqline(items[[x]])
})
plot(qc <- qchisq((1:nrow(items) - 1/2) / nrow(items), df = 12),
    sd <- sort(d),
    xlab = expression(paste(chi[12]^2, " Quantile")), 
    ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
abline(a = 0, b = 1)
par(mfrow = c(1,1))

# ....

apply(items, 2, sd)
round(cov(items), 2)
round(cov(items), 2)

# ...
plot(items, cex = 0.25)
scatterplotMatrix(items)

# ...

res <- prcomp(items) # scale = FALSE
# print(res)
desk <- summary(res)
# print(desk)
plot(res)
daten <- cbind(daten, round(predict(res)[, 1:3], 3))
head(daten)

# ...

biplot(res, xlabs = daten$model, col = c("black", "blue"), cex = 0.6)

# ...
str(daten)
daten$dimension <- factor(daten$dimension)
boxplot(PC2 ~ dimension, data = daten) # "Aquaplaning"
t.test(PC2 ~ dimension, data = daten)