
library(readr)
test.df <- data.frame(value = c(999, 1000, 1001))

write_csv(test.df, path = 'data/test_out.csv')
write.csv(test.df, file = 'data/test_out2.csv')
