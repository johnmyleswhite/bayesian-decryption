source('load_probability_table.R')
source('cipher.R')

input.text <- scan('gettysburg.txt', what = 'character')
decrypted.text <- tolower(input.text)
encrypted.text <- apply.cipher.to.text(decrypted.text, caesar.cipher)

number.of.passes <- 1
number.of.iterations <- 100

number.of.passes <- 100
number.of.iterations <- 10000

results <- data.frame()

for (pass in 1:number.of.passes)
{
  cipher <- generate.random.cipher()
  
  for (iteration in 1:number.of.iterations.times)
  { 
    log.probability <- log.probability.of.text(encrypted.text, cipher, probability.table)
    current.decrypted.text <- paste(apply.cipher.to.text(encrypted.text, cipher), collapse = ' ')
    correct.text <- as.numeric(current.decrypted.text == paste(decrypted.text, collapse = ' '))
    results <- rbind(results,
                     data.frame(Sampler = 'Pseudo-Gibbs',
                                InputSize = length(decrypted.text),
                                Pass = pass,
                                Iteration = iteration,
                                LogProbability = log.probability,
                                CurrentDecryptedText = current.decrypted.text,
                                CorrectText = correct.text))
    write.table(results, file = 'results.csv', row.names = FALSE, sep = '\t')
    
    cipher <- sampler.step(encrypted.text, cipher, probability.table)
  }
}
