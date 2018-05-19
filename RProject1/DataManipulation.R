data(diamonds, package = "ggplot2")
head(diamonds, 4)
dim(head(diamonds, 4))

library(magrittr)

diamonds %>% head(4) %>% dim

x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

round(exp(diff(log(x))), 1)

x %>% log %>% diff %>% exp %>% round(1)

library(dplyr)
diamonds %>% head(5)

select(diamonds, carat, price)
diamonds %>% select(c(carat, price))
diamonds %>% select(c(-carat, -price))

my_attributes <- c('carat', 'price')
diamonds %>% select(one_of(my_attributes))

diamonds %>% select(starts_with("c"))
diamonds %>% select(ends_with("e"))
diamonds %>% select(contains("e"))

diamonds %>% select(matches("r.t"))

diamonds %>% filter(cut=="Ideal")
diamonds %>% filter(cut %in% c("Ideal", "Good"))


slice(diamonds, c(1:5, 8, 15:20))
diamonds %>% slice(c(1:5, 8, 15:20))

?mutate()

set <- diamonds %>% select(c(carat, price))
mutate(set, ratio = price / carat)

set <- set %>% mutate(ratio = price / carat) %>% mutate(Double = ratio *2)
set <- set %>% mutate(ratio = price / carat, Double = ratio * 2 )
set

diamonds %>% summarise(AvgPrice = mean(price), MedianPrice = median(price), AvgCarat = mean(carat))

diamonds %>% group_by(cut) %>% summarise(AvgPrice = mean(price))

diamonds %>% group_by(cut, color) %>% summarise(AvgPrice = mean(price), SumCarat = sum(carat))

diamonds %>% group_by(cut) %>% summarise(AvgPrice = mean(price), SumCarat = sum(carat)) %>% arrange(AvgPrice)

combine_df <- function(years) {

    full_df <- NULL
    for (year in years) {
        tmp <- read.csv(paste0("C:/Users/sgobi/Source/Repos/DataScience2018/HealthResearch/Lotto/", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)
        full_df <- rbind(full_df, tmp)
    }
    return(full_df)
}

lotto_data <- combine_df(1999:2017)
head(lotto_data)

lotto_data %>% summarise(maxA = max(a), maxB = max(b), maxC = max(c), maxD = max(d), maxE = max(e), maxF = max(f), maxBonus = max(Bonus))
lotto_data %>% summarise(meanA = mean(a), meanB = mean(b), meanC = mean(c), meanD = mean(d), meanE = mean(e), meanF = mean(f), meanBonus = mean(Bonus))
lotto_data %>% summarise(minA = min(a), minB = min(b), minC = min(c), minD = min(d), minE = min(e), minF = min(f), minBonus = min(Bonus))
lotto_data %>% summarise(rangeA = max(a) - min(a), rangeB = max(b) - min(b), rangeC = max(c) - min(c), rangeD = max(d) - min(d), rangeE = max(e) - min(e), rangeF = max(f) - min(f), rangeBonus = max(Bonus) - min(Bonus))

range(lotto_data$a)[2] - range(lotto_data$a)[1]






install.packages("hflights")
library(hflights)
head(hflights)
str(hflights)
class(hflights)

hflights_df <- as.tbl(hflights)

f_df <- hflights_df %>% filter(Month == 1, UniqueCarrier == "AA")
head(f_df)
str(f_df)

u_df <- hflights_df %>% filter(UniqueCarrier == "AA" | UniqueCarrier == "UA")
head(u_df)
str(u_df)

unique(u_df$UniqueCarrier)

head(hflights_df %>% arrange(-Month, -DayofMonth, -AirTime))

head(hflights_df %>% select(c(1:4, 9, 10)))

head(hflights_df %>% mutate(Gain = ArrDelay - DepDelay, Gain_Per_Hour = (ArrDelay - DepDelay) / (AirTime / 60)))


Delay <- hflights_df %>% summarise(MeanArrDelay = mean(ArrDelay, na.rm = TRUE))

Delay



drug_df <- data.frame("Dose" = c(20, 30, 40, 45, 60), "Drug A" = c(16, 20, 27, 40, 60), "Drug B" = c(15, 18, 25, 31, 40))
drug_df
str(drug_df)

plot(drug_df$Dose, type = "o", col = "blue")
plot(drug_df$Dose, drug_df$Drug.A, type = "b", col = "blue")
par(new=TRUE)
plot(drug_df$Dose, drug_df$Drug.B, type = "b", col = "orange")

opar <- par(no.readonly = TRUE)
opar

plot(drug_df$Dose, drug_df$Drug.B, type = "b", col = "orange", lty = "solid", pch = 17)

par(lty = "solid", pch = 17)
plot(drug_df$Dose, drug_df$Drug.B, type = "b")

par(opar)
plot(drug_df$Dose, drug_df$Drug.B, type = "b")

par(lty = 3, lwd = 3, pch = 15, cex = 2)
plot(drug_df$Dose, drug_df$Drug.B, type = "b")
title(main = "Drug dosage", col.main = "blue", font.main = 4)

plot(drug_df$Dose, drug_df$Drug.A, type = "l", col="red" )
plot(drug_df$Dose, drug_df$Drug.A, type = "b", col = "blue", lty = 3, lwd = 3, pch = 15, cex = 2, ylim = c(0, 100))

lines(drug_df$Dose, drug_df$Drug.B, type = "o", col = "red", lty = 2, pch = 22)

graph_range <- range(0, drug_df$Drug.A, drug_df$Drug.B)
graph_range


plot(drug_df$Drug.A, type = "o", axes = FALSE, ylim = graph_range, xlab = "millilitres")
lines(drug_df$Drug.B, type = "o", col = "red", lty = 2, pch = 22)
axis(1, at = 1:5, labels = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml"))

axis(2, las = 1, at = 5 * 0:graph_range[2])
box(lty = 'solid')

? box
?axis
? plot
? par()
