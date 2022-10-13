

library(ggplot2)

# data
yrs <- 1980:2015
stranding <- c(1,4,1,4,0,0,3,0,0,0,
               5,1,4,7,2,2,2,1,4,0,
               3,3,1,0,0,0,1,2,1,0,
               1,0,2,1,5,3)

df1 <- data.frame(year = yrs, stranding = stranding)
p1 <- ggplot(data = df1, aes(x = year, y = stranding)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("Year") + ylab("Stranding") + 
  theme(axis.text = element_text(size = 14))

p1
