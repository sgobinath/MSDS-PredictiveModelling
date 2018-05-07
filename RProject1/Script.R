library(swirl)
install_course_zip("D:/Big Data/DS/swirl_courses-master.zip", multi = TRUE, which_course = "Statistical Inference")

install_course_zip("D:/Big Data/DS/swirl_courses-master.zip", multi = TRUE, which_course = "Open Intro")

swirl()

install.packages("pwr")
library(pwr)

? pwr.t.test
? pwr.p.test

power_changes <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.5), sig.level = 0.05, power = 0.8, alternative = "greater")
plot(pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.5), sig.level = 0.01, power = 0.8, alternative = "greater"))

power_changes <- pwr.p.test(h = ES.h(p1 = 0.65, p2 = 0.5), sig.level = 0.05, power = 0.8, alternative = "two.sided")
power_changes
plot(power_changes)

effect_size <- c(cohen.ES(test = "r", size = "small")$effect.size, cohen.ES(test = "r", size = "medium")$effect.size, cohen.ES(test = "r", size = "large")$effect.size)
effect_size

power_changes <- pwr.r.test(r = effect_size, sig.level = 0.05, n = 20)
power_changes <- pwr.p.test(h = effect_size, sig.level = 0.05, n = 40)
power_changes
plot(power_changes)

cohen.ES(test = "r", size = "small")

?pwr.2p.test
pwr.2p.test(h = ES.h(p1 = 0.1, p2 = 0.05), sig.level = 0.05, power = 0.8)

?cohen.ES
