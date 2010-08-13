results <- read.csv('results.csv', header = TRUE, sep = '\t')

png('performance.png')
ggplot(results, aes(x = Iteration,
                        y = LogProbability,
                        color = factor(Pass))) +
  geom_point() +
  opts(title = 'Decryption Success of Each Pass') +
  xlab('Iteration') +
  ylab('Log Probability of Text') +
  scale_colour_discrete(legend = FALSE)
dev.off()
