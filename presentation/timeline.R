
# import statements
library(tidyverse)
library(magrittr)
library(ggplot2)

# timeline
data <- tibble(x = c(0:5), y = rep(1, 6),
               labels = c(NA, paste0('Category ', 1:4), NA))
dev.off()
ggplot(data, aes(x = x, y = y)) +
  geom_line(aes(x = c(.5, 1:4, 4.5))) +
  coord_cartesian(ylim = c(.75, 1.25), xlim = c(0:5)) +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10", size = 12),
        panel.grid.major = element_blank(), panel.border = element_blank(),
        panel.grid.minor = element_blank(), axis.line    = element_blank(),
        axis.line.y      = element_blank(), axis.text.y  = element_blank(),
        axis.line.x      = element_blank()) +
  ylab('') + xlab('Procurement Rules Schedule (in R$)')