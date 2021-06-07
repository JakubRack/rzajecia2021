# Z  A  D  A  N  I  E     1

library("dplyr")

rankAccount <- function(dataFrame, colName, groupName, valueSort, num){
    fileConnection <- file(description = dataFrame, open ="r")
    data <- read.table(fileConnection, header = TRUE, fill = TRUE, sep = ",")
    data <- na.omit (data)
    my_data <- data %>% filter(data[[colName]] == groupName)
    my_data2 <- my_data %>% top_n(num, my_data[[valueSort]])
    print(my_data2)
    }

rankAccount("konta.csv","occupation", "NAUCZYCIEL", "saldo",10)



# Z  A  D  A  N  I  E     2

rankAccount <- function(dataFrame, size, colName, groupName, valueSort, num){
  fileConnection <- file(description = dataFrame, open ="r")
  data <- read.table(fileConnection, nrows = size, header = TRUE, fill = TRUE, sep = ",")
  data <- na.omit (data)
  my_data <- data %>% filter(data[[colName]] == groupName)
  my_data2 <- my_data %>% top_n(num, my_data[[valueSort]])
  print(my_data2)
  
}

rankAccount("konta.csv", 100, "occupation", "NAUCZYCIEL", "saldo",10)




# Z  A  D  A  N  I  E     3

library(RSQLite)

con <- dbConnect(SQLite(),"listakont.sqlite")
dbGetQuery(con, "SELECT * FROM listakont WHERE occupation LIKE 'NAUCZYCIEL' ORDER BY saldo DESC LIMIT 10")
dbDisconnect(con)
