library(tidyverse)

boys_wt <- read_csv("./data/boys_weight.csv")
boys_wt$type <- "weight"
boys_L <- read_csv("./data/boys_length.csv")
boys_L$type <- "length"
boys_HC<- read_csv("./data/boys_HC.csv")
boys_HC$type <- "HC"

boys_L_HC<- rbind(boys_L, boys_HC)


calculate_obs <- function(L,M,S, percentile){
  z <- qnorm(percentile)
  X = M*(L*S*(1/(L*S) + z))^(1/L)
  X
}

boys_wt$P03 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.03)#/1000
boys_wt$P10 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.10)#/1000
#boys_wt$P25 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.25)/1000
boys_wt$P50 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.50)#/1000
#boys_wt$P75 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.75)/1000
boys_wt$P90 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.90)#/1000
boys_wt$P97 <- calculate_obs(boys_wt$L,boys_wt$M,boys_wt$S,0.97)#/1000

boys_L_HC$P03 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.03)
boys_L_HC$P10 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.10)
#boys_L_HC$P25 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.25)
boys_L_HC$P50 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.50)
#boys_L_HC$P75 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.75)
boys_L_HC$P90 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.90)
boys_L_HC$P97 <- calculate_obs(boys_L_HC$L,boys_L_HC$M,boys_L_HC$S,0.97)

boys_all<- rbind(boys_L_HC, boys_wt)
boys_all_gather <- boys_all %>% gather(key = annotation, value = value, P03:P97, -`Compl weeks`, -type)
write_csv(boys_all_gather, "./data/boys_all.csv")
write_csv(boys_all, "./data/boys_all_spread.csv")

ggplot(data = boys_all_gather %>% filter(type == "weight"), aes(x = `Compl weeks`, y = as.numeric(value), linetype = annotation, shape = type))  + 
  geom_line() + theme_bw()  + scale_x_continuous(breaks=seq(22, 42, 1), name = "PML") +
  scale_y_continuous(breaks=seq(0, 5000, 200),limits = c(0,5000), name = "gram")

ggplot(data = boys_all_gather %>% filter(type  %in% c("HC","length") ), aes(x = `Compl weeks`, y = as.numeric(value), linetype = annotation, col = type))  + 
  geom_line() + theme_bw() + scale_y_continuous(breaks=seq(18, 60, 5), name = "centimeter") +
  scale_x_continuous(breaks=seq(22, 42, 1), name = "PML") #+ facet_wrap(~ type, ncol = 1, scales = "free") #+ ylim(0,5)


