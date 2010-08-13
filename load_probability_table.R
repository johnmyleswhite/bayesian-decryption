library('yaml')
library('RMySQL')

configuration <- yaml.load_file('database.yml')

mysql.driver <- dbDriver("MySQL")

connection <- dbConnect(mysql.driver,
                        user = configuration[['user']],
                        password = configuration[['password']],
                        host = configuration[['host']],
                        dbname = configuration[['dbname']])

sql <- 'SELECT n_gram, probability FROM n_grams WHERE n = 1'

result.set <- dbSendQuery(connection, sql)

parcel.size <- 10000
  
data.parcel <- fetch(result.set, n = parcel.size)

if (nrow(data.parcel) == 0)
{
  return(NULL)
}

probability.table <- list()

for (i in 1:nrow(data.parcel))
{
  n.gram <- data.parcel[i, 'n_gram']
  probability <- data.parcel[i, 'probability']
  probability.table[[n.gram]] <- probability
}

while (!dbHasCompleted(result.set))
{
  data.parcel <- fetch(result.set, n = parcel.size)
  
  if (nrow(data.parcel) > 0)
  {
    for (i in 1:nrow(data.parcel))
    {
      n.gram <- data.parcel[i, 'n_gram']
      probability <- data.parcel[i, 'probability']
      probability.table[[n.gram]] <- probability
    }
  }
}
