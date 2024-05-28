if (!requireNamespace("tidyverse", quietly=TRUE)) {
    # If not, install the package
    install.packages("tidyverse")
}

# Read the data from the CSV file with delimiter " "
data <- read.table("bugfixes.csv", header=TRUE)

# View the structure of the dataset
str(data)

# Scatter plot of 'codelines' vs 'duration'
ggplot(bugfix, aes(x = codelines, y = duration)) + 
    geom_point() + labs(x = "Number of Code Lines", y = "Time Needed to Fix (duration)", 
    title = "Scatter Plot of 'codelines' vs 'duration'")


# Scatter plot of 'usecases' vs 'dauer'
ggplot(bugfix, aes(x = usecases, y = duration)) +
geom_point() +
labs(x = "Number of Use Cases", y = "Time Needed to Fix (duration)",
title = "Scatter Plot of 'usecases' vs 'duration'")