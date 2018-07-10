setwd("/Users/thirdlovechangethisname/Documents/Code/Pretend_Company_Growth/charts")

s <- function(x) {
  1 / (1 + exp(-x*1.5))
}

p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = s, size=1) + xlab("Time") + ylab("Growth") + 
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) + 
  geom_vline(xintercept = 2, linetype=3) +  geom_vline(xintercept = -2, linetype=3)

ggsave(p, file="s.png", device = "png", dpi=72, width=9, height=6)
