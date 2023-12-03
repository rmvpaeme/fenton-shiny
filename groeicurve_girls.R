library(tidyverse)


girls_wt <- read_csv("./data/girls_weight.csv")
girls_wt$type <- "weight"
girls_L <- read_csv("./data/girls_length.csv")
girls_L$type <- "length"
girls_HC<- read_csv("./data/girls_HC.csv")
girls_HC$type <- "HC"

girls_L_HC<- rbind(girls_L, girls_HC)


calculate_obs <- function(L,M,S, percentile){
  z <- qnorm(percentile)
  X = M*(L*S*(1/(L*S) + z))^(1/L)
  X
}

girls_wt$P03 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.03)#/1000
girls_wt$P10 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.10)#/1000
#girls_wt$P25 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.25)/1000
girls_wt$P50 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.50)#/1000
#girls_wt$P75 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.75)/1000
girls_wt$P90 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.90)#/1000
girls_wt$P97 <- calculate_obs(girls_wt$L,girls_wt$M,girls_wt$S,0.97)#/1000

girls_L_HC$P03 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.03)
girls_L_HC$P10 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.10)
#girls_L_HC$P25 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.25)
girls_L_HC$P50 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.50)
#girls_L_HC$P75 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.75)
girls_L_HC$P90 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.90)
girls_L_HC$P97 <- calculate_obs(girls_L_HC$L,girls_L_HC$M,girls_L_HC$S,0.97)

girls_all<- rbind(girls_L_HC, girls_wt)
girls_all_gather <- girls_all %>% gather(key = annotation, value = value, P03:P97, -`Compl weeks`, -type)
write_csv(girls_all_gather, "./data/girls_all.csv")
write_csv(girls_all, "./data/girls_all_spread.csv")

ggplot(data = girls_all_gather %>% filter(type == "weight"), aes(x = `Compl weeks`, y = as.numeric(value), linetype = annotation, shape = type))  + 
  geom_line() + theme_bw()  + scale_x_continuous(breaks=seq(22, 42, 1), name = "PML") +
  scale_y_continuous(breaks=seq(0, 5000, 200),limits = c(0,5000), name = "gram")

ggplot(data = girls_all_gather %>% filter(type  %in% c("HC","length") ), aes(x = `Compl weeks`, y = as.numeric(value), linetype = annotation, col = type))  + 
  geom_line() + theme_bw() + scale_y_continuous(breaks=seq(18, 60, 5), name = "centimeter") +
  scale_x_continuous(breaks=seq(22, 42, 1), name = "PML") #+ facet_wrap(~ type, ncol = 1, scales = "free") #+ ylim(0,5)


