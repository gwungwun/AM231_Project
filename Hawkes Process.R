## Hawkes Process

# read data
bitstamp_USD <- read.csv('~/Documents/Spring 2018/AM 231/Final Project/bitcoin_1min/bitstampUSD_1-min_data_2012-01-01_to_2018-01-08.csv')

# install ptproc by downloading the zip file ptproc.tar.gz
# run R CMD INSTALL ptproc.tar.gz in the directory of the file
library(ptproc)
# Find the parameters for the Hawkes process
fit_hawkes_process <- function(data) {
  pinit <- c(mu = 0.5, C = 1, a = 0.1)
  ppm <- ptproc(pts = data, cond.int = hawkes.cond.int, params = pinit)
  condition(ppm) <- penalty(code = NULL, condition = quote(any(params < 0)))
  f <- ptproc.fit(ppm, optim.control = list(trace = 2), 
                  alpha = 1e+5, hessian = TRUE)
  return (f)
}
f <- fit_hawkes_process(bitstamp_USD$Volume_.BTC.[(3161057-100*1440):(3161057-99*1440)])
saveRDS(f, "hawkes_process.rds")
summary(f)
e <- evalCIF(f, xpts = f$ptprocInit$pts)
library(ggplot2)
df = data.frame(x=c(f$ptprocInit$pts, f$ptprocInit$pts), 
                y = c(bitstamp_USD$Volume_.BTC.[(3161057-100*1440):(3161057-99*1440)], e), 
                type = c(rep("Original", 1441), rep("Hawkes", 1441)))
ggplot(df, aes(x=x, y=y, group=type, color=type)) + geom_point()
