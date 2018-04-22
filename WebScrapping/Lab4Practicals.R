install.packages('rvest')
library(rvest)

webpage <- read_html('http://www.cso.ie/en/releasesandpublications/ep/p-vsys/vitalstatisticsyearlysummary2015/')
str(webpage)

df_summary <- html_table(html_nodes(webpage, 'table.excel-102206-media-t1')[[1]])
colnames(df_summary) <- c("Event", "Year2015", "Year2014")
row_count <- nrow(df_summary) - 1
df_summary <- df_summary[3:row_count,]

df_summary$Year2014 <- as.numeric(gsub(",", "", df_summary$Year2014))
df_summary$Year2015 <- as.numeric(gsub(",", "", df_summary$Year2015))

rownames(df_summary) <- NULL
str(df_summary)
df_summary


# Read the table in to a data frame. Use fill parameter with value TRUE as the Table has inconsistent number of columns.
df_mortality <- html_table(html_nodes(webpage, 'table.excel-102228-media-t1')[[1]], fill = TRUE)

# Remove first and last row as they are not required for the data frame
row_count <- nrow(df_mortality) - 1
df_mortality <- df_mortality[3:row_count,]

# Concatenate the values of row 1 and 2 and remove the row 2
df_mortality[1,] <- paste0(df_mortality[1,], " ", df_mortality[2,])
df_mortality <- df_mortality[c(1, 3:nrow(df_mortality)),]

# Trim all the values of the data frame
df_mortality <- data.frame(sapply(df_mortality, trimws), stringsAsFactors = FALSE)
# Replace the blank values as NA
df_mortality[df_mortality == ''] <- NA

# The first 3 columns has cause of death, combine them in 1 column and remove other 2
# Replace the first column as NA where the value in first column is equal to second column
df_mortality$X1[df_mortality$X1 == df_mortality$X2] <- NA
# Get the count of NA in first column
sum(is.na(df_mortality$X1))
# This is equal to number of rows which implies that all the values in this column are NA. Hence remove it.
df_mortality$X1 <- NULL

# Apply the above logic between column 2 and 3
df_mortality$X2[df_mortality$X2 == df_mortality$X3] <- NA
sum(is.na(df_mortality$X2))
df_mortality$X2 <- NULL

# Make the values in the first row as column header and remove the first row
colnames(df_mortality) <- df_mortality[1,]
df_mortality <- df_mortality[-1,]

# Remove the rows where all the values are NA
df_mortality <- df_mortality[rowSums(is.na(df_mortality)) != ncol(df_mortality),]

# The 65 COD Code column is irrelevant hence remove it.
df_mortality$'65 COD Code' <- NULL


# Upon verifying data frame, there are few rows where the number of deaths column is NA.
# This is because the long text of Cause of death column is separated in to 2 rows. Let's combine them in 1 row
# Get the positions (row index) of Total Deaths column where the value is NA.
index_na_values <- which(is.na(df_mortality$'Total Deaths'))

# Write a loop which will concatenate the cause of death values in single row
for (idx in index_na_values) {
    df_mortality$'Cause of Death'[idx + 1] <- paste0(df_mortality$'Cause of Death'[idx], " ", df_mortality$'Cause of Death'[idx + 1])
}

# Remove the rows where the death counts are empty
df_mortality <- df_mortality[!is.na(df_mortality$'Total Deaths'),]

# Remove the characters from the first column
df_mortality$'Cause of Death' <- gsub("of which: ", "", df_mortality$'Cause of Death')
df_mortality$'Cause of Death'[9] <- gsub(" of the:-", "", df_mortality$'Cause of Death'[9])
df_mortality$'Cause of Death' <- gsub(":-", "", df_mortality$'Cause of Death')

# Remove paranthesis from a specific value of the dataset
df_mortality[38, 1] <- gsub("\\(", "", df_mortality[38, 1])
df_mortality[38, 1] <- gsub("\\)", "", df_mortality[38, 1])

df_mortality[, 3:14] <- sapply(df_mortality[3:14], function(x) as.numeric(gsub(",", "", x)))

str(df_mortality)
head(df_mortality)

# Save the final data frame to a csv file in the hard drive
write.csv(df_mortality, file = "Data/WebScrap_Mortality2015.csv", na = "", row.names = FALSE)
