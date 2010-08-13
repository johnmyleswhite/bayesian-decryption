source('cipher.R')

test.apply.cipher.to.string <- function()
{
  string <- 'this'
  
  checkEquals('uijt',
              apply.cipher.to.string(string,
                                     caesar.cipher))
  
  checkEquals(string,
              apply.cipher.to.string(apply.cipher.to.string(string,
                                                            caesar.cipher),
                                     inverse.caesar.cipher))
}

test.apply.cipher.to.text <- function()
{
  text <- c('this', 'is', 'a', 'test')
  
  checkEquals(c('uijt', 'jt', 'b', 'uftu'),
              apply.cipher.to.text(text, caesar.cipher))
  
  checkEquals(text,
              apply.cipher.to.text(apply.cipher.to.text(text,
                                                        caesar.cipher),
                                   inverse.caesar.cipher))
}

test.generate.random.cipher <- function()
{
  # Should be a valid cipher, i.e. bijective over english.letters
  cipher <- generate.random.cipher()
  checkEquals(26, length(cipher))
  checkEquals(english.letters, names(cipher))
  checkEquals(english.letters, sort(as.character(sapply(names(cipher), function (key) {cipher[[key]]}))))
}

test.modify.cipher <- function()
{
  modified.caesar.cipher <- modify.cipher(caesar.cipher, 'a', 'c')
  checkEquals('c', modified.caesar.cipher[['a']])
  checkEquals('b', modified.caesar.cipher[['b']])
}

test.propose.modified.cipher <- function()
{
  modified.caesar.cipher <- propose.modified.cipher(caesar.cipher)
  # A modified cipher should differ in at most two places from the original
  differences <- 0
  for (key in names(caesar.cipher))
  {
    if (modified.caesar.cipher[[key]] != caesar.cipher[[key]])
    {
      differences <- differences + 1
    }
  }
  checkEquals(2, differences)
}
