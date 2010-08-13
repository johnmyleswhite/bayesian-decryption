english.letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
                     'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                     'w', 'x', 'y', 'z')

caesar.cipher <- list()
inverse.caesar.cipher <- list()
for (index in 1:length(english.letters))
{
  caesar.cipher[[english.letters[index]]] <- english.letters[index %% 26 + 1]
  inverse.caesar.cipher[[english.letters[index %% 26 + 1]]] <- english.letters[index]
}

apply.cipher.to.string <- function(string, cipher)
{
  output <- ''
  
  for (i in 1:nchar(string))
  {
    output <- paste(output, cipher[[substr(string, i, i)]], sep = '')
  }
  
  return(output)
}

apply.cipher.to.text <- function(text, cipher)
{
  output <- c()
  
  for (string in text)
  {
    output <- c(output, apply.cipher.to.string(string, cipher))
  }
  
  return(output)
}

generate.random.cipher <- function()
{
  cipher <- list()
  
  inputs <- english.letters
  outputs <- english.letters[sample(1:length(english.letters), length(english.letters))]
  
  for (index in 1:length(english.letters))
  {
    cipher[[inputs[index]]] <- outputs[index]
  }
  
  return(cipher)
}

modify.cipher <- function(cipher, input, output)
{
  new.cipher <- cipher
  new.cipher[[input]] <- output
  old.output <- cipher[[input]]
  collateral.input <- names(which(sapply(names(cipher), function (key) {cipher[[key]]}) == output))
  new.cipher[[collateral.input]] <- old.output
  return(new.cipher)
}

propose.modified.cipher <- function(cipher)
{
  input <- sample(names(cipher), 1)
  output <- sample(english.letters, 1)
  return(modify.cipher(cipher, input, output))
}

one.gram.probability <- function(one.gram, probability.table = list())
{
  lexical.probability <- probability.table[[one.gram]]
  
  if (is.null(lexical.probability) || is.na(lexical.probability))
  {
    return(.Machine$double.eps)
  }
  else
  {
    return(lexical.probability)
  }
}

log.probability.of.text <- function(text, cipher, probability.table = list())
{
  log.probability <- 0.0
  
  for (string in text)
  {
    decrypted.string <- apply.cipher.to.string(string, cipher)
    log.probability <- log.probability + log(one.gram.probability(decrypted.string, probability.table))
  }
  
  return(log.probability)
}

sampler.step <- function(text, cipher, probability.table = list())
{
  proposed.cipher <- propose.modified.cipher(cipher)
  
  lp1 <- log.probability.of.text(text, cipher, probability.table)
  lp2 <- log.probability.of.text(text, proposed.cipher, probability.table)
  
  if (lp2 > lp1)
  {
    return(proposed.cipher)
  }
  else
  {
    a <- exp(lp2 - lp1)
    x <- runif(1)
    if (x < a)
    {
      return(proposed.cipher)
    }
    else
    {
      return(cipher)
    }
  }
}
