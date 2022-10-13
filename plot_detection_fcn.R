#plot_detection_fcn



rm(list=ls())
library(ggplot2)
fig.save <- T

# get the fit for aerial survey:

load('RData/shipLineTransect_2017-05-22.RData')

# get the estimates:
sigma <- exp(fit.yr.pooled$ddf$par['X.Intercept.'])
a1 <- fit.yr.pooled$ddf$par['V2']
a2 <- fit.yr.pooled$ddf$par['V3']
w <- fit.yr.pooled$ddf$meta.data$width # this is truncation distance

dhn <- function(x, sigma, a, w){
  dens <- (exp((-x^2)/(2 * sigma^2))) * ( 1 + a[1] * cos(2 * pi * x/w) + a[2] * cos(3 * pi * x/w))
  dens <- dens/dens[1]
  return(dens)
}

model <- fit.yr.pooled$ddf
df <- data.frame(x = model$data$distance)

bin.width <- 0.13

# observed counts per bin
bins <- seq(from = 0, 
            to = model$meta.data$width, 
            by = bin.width)

obs.counts <- vector(mode = 'numeric', 
                     length = (length(bins)-1))

dens.center <- dhn(bins[2:length(bins)], 
                   sigma, 
                   c(a1, a2), 
                   w)

# expected counts based on the half-normal + cosine
exp.counts <- dim(df)[1] * dens.center/sum(dens.center)  

# observed counts:
for (k in 1:length(obs.counts)){
  obs.counts[k] <- length(df$x[df$x>=bins[k] & df$x<bins[k+1]])
}

df.counts <- data.frame(observed = obs.counts,
                        expected = exp.counts,
                        x = bins[2:length(bins)],
                        observed.p = obs.counts/exp.counts[1],
                        expected.p = exp.counts/exp.counts[1],
                        hn.p = dhn(bins[2:length(bins)], 
                                    sigma, 
                                    c(a1, a2), 
                                    w))

x.df <- data.frame(x = seq(from = 0, 
                           to = 1.50,
                           by = 0.1))

p1 <- ggplot(data = df.counts) +
  geom_bar(aes(x = x-bin.width, y = observed.p),
           width = bin.width,
           stat = "identity",
           color = 'black',
           fill = 'white') +
  stat_function(data = x.df, fun = dhn,
                args = list(sigma = sigma, 
                            a = c(a1, a2), 
                            w = w),
                lwd = 2,
                col = 'black') +
  scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1.0)) +
  scale_x_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.50),
                     labels = c('0', '250', '500', '750',
                                '1000', '1250', '1500')) +
  ylab('Detection probability') +
  xlab('Perpendicular distance (m)') +
  theme(axis.title = element_text(size = 12))

dpi <- 300
if (fig.save){
  ggsave(plot = p1,
         dpi = dpi,
         width = 6.5,
         height = 3.44,
         file = paste0('figures/MM_survey_detection_fcn_',
                       dpi, 'dpi.png'))
  
}

# ESW computation using the easy way:
# mu = Pa * w
sum.model <- summary(fit.yr.pooled)
esw <- sum.model$ds$average.p * sum.model$ddf$meta.data$width
SE.esw <- sum.model$ddf$meta.data$width * sum.model$ds$average.p.se

