#ToothGrowth and related data analysis
vc <- ToothGrowth$len[1:30]
oj <- ToothGrowth$len[31:60]
diff <- vc-oj
mn <- mean(diff)
s <- sd(diff) 
n <- 30
t.test(len ~ I(relevel(supp,2)), paired = FALSE, data = ToothGrowth, alternative = "less") %>% print()
t.test(len ~ I(relevel(supp,2)), paired = FALSE, data = ToothGrowth[ToothGrowth$dose==.5,]) %>% print()
t.test(len ~ I(relevel(supp,2)), paired = FALSE, data = ToothGrowth[ToothGrowth$dose==1,]) %>% print()
t.test(len ~ I(relevel(supp,2)), paired = FALSE, data = ToothGrowth[ToothGrowth$dose==2,]) %>% print()

tooth_mns <- ToothGrowth %>% group_by(supp,dose)  %>% summarise(mean = mean(len)) %>% print()

g <- ggplot(ToothGrowth, aes(x = dose, y = len, colour = dose, group = supp))
g <- g + geom_point()
g <- g + geom_line(tooth_mns, mapping = aes(x = dose,y = mean),linetype = 2, size = 1, color = "red") 
#g <- g + stat_summary(aes(group = 3), geom = "line", fun = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ supp)
print(g)

g <- ggplot(ToothGrowth, aes(x = factor(dose), y = len, fill = factor(dose)))
g <- g + geom_violin(col = "black", size = 2)
g <- g + facet_grid(. ~ supp)
print(g)
