#Zadanie 1

podzielnosc <- function(a1, a2) {
  a1 <- readline(prompt="Wprowadz a1: ")
  a2 <- readline(prompt="Wprowadz a2: ")
  a1 <- as.integer(a1)
  a2 <- as.integer(a2)
  if(a1%%a2 ==0){
    print("Liczby sa podzielne przez siebie")
  } else {
    print("Liczby nie sa podzielne przez siebie")
  }
   
}
podzielnosc(a1,a2)


#Zadanie 2

x <- 120
y <- 90

#Wyprowadzanie wzoru funkcji
#s/t1=x
#s/t2=y
#Vsr=(2s/(t1+t2))

#t2=s/y
#t1=s/x

#Vsr=(2s/(s/x+s/y)) // s sie skraca
#Vsr=(2/(1/x+1/y))


predkosc <- function(x, y) {
  V <- (2/(1/x+1/y))
  V <- round(V, digits = 2)
  paste("Srednia predkosc pociagu to ok.", V, "km/h")
}
predkosc(x,y)



#Zadanie 3
# Zalozenie -> dlugosc(x) == dlugosc(y)

#x <- c(1,2,3,4,6,4,8,5,3,8,4,8,3,7,6)
#y <- c(4,5,1,1,3,4,6,5,7,4,7,9,4,2,3)

#x <- c(18,18,21,19,16,17)
#y <- c(78,77,98,110,80,69)


#wczytanie danych
df<-read.table(file="dane.csv", header = TRUE, sep=";")
#Po piatym zadaniu ustawic -> setwd(D:/Papierkowa robota/PJA/R/rzajecia2021)

#wektory
x <- df[,1]
y <- df[,2]

pearsonr <- function(x, y){

  srx <- mean(x)
  sry <- mean(y)
  
  #iloczyn poszczególnych składników i srednia z tego
  xv <- matrix(x, nrow = 1, ncol = length(x), byrow = TRUE)
  yv <- matrix(y, nrow = 1, ncol = length(y), byrow = TRUE)
  il <- xv*yv
  
  srm <- mean(il)
  
  #Kowariancja
  covv <-srm - srx*sry 
  
  #Odchylenia standardowe x i y
  
  xro <- matrix(x-srx, nrow = 1, ncol = length(x), byrow = TRUE)
  yro <- matrix(y-sry, nrow = 1, ncol = length(y), byrow = TRUE)
  
  odx <- sqrt(sum(xro**2)/length(x))
  ody <- sqrt(sum(yro**2)/length(x))
  mian <- odx*ody
  
  #wspolczynnik korelacji Pearsona
  pear <- covv/mian
  pear <- round(pear, digits = 2)
  paste("Wspolczynnik korelacji Pearsona wynosi", pear)
}
pearsonr(x,y)


#----- W   N   I   O  S   K   I -----

#Wspolczynnik korelacji Pearsona miedzy wysokoscia a waga wynosi 0.98
#Jego watosc miesci sie w zakresie -1:1. Im wyzsza wartosc, tym wieksza zaleznosc
#liniowa danych wektorow.
#Okazuje sie, ze wzrost i waga osob sa praktycznie liniowo zalezne od siebie tzn.:
#jesli wzrost rosnie wzgledem poprzedniej danej to waga rowniez rosnie.



#Zadanie 4


stworzDataFrame <- function(ile=1){
  
  kk <- "podaj nazwy kolumn oddzielone przecinkiem:"
  k <- strsplit(readline(kk), ",")[[1]]
  il <- readline(prompt="podaj liczbe wierszy:")
  ile <- as.integer(il)
  tyt <- matrix(k, nrow = 1, ncol = length(k), byrow = TRUE)
  if (is.na(ile)|| ile == 0 || ile == ""){
    print("Brak liczby wierszy, domyslna wartosc to: 1")
    wi <- "podaj wartosci dla wiersza oddzielone przecinkiem:"
    w <- strsplit(readline(wi), ",")[[1]]
    rr <- matrix(w, nrow = 1, ncol = length(w), byrow = TRUE)
    tyt <- rbind(tyt, rr)
    
  }else
  {
      for (i in 1:ile) {
    tt <- NULL
    wi <- "podaj wartosci dla wierszy oddzielone przecinkiem:"
    w <- strsplit(readline(wi), ",")[[1]]
    rr <- matrix(w, nrow = 1, ncol = length(w), byrow = TRUE)
    tyt <- rbind(tyt, rr)
    #Naming the Data Frame - Step 2  
    #names() <- k  
    #print(rr)
    #Using rbind() function to insert above observation  

    
  }
}
  ddf <- as.data.frame(tyt)
  my.names <- k
  colnames(ddf) <- my.names
  ddf <- ddf[-1, ]
  View(ddf)
}
stworzDataFrame()


#Zadanie 5

  
liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){
  sciezka <-  readline(prompt="podaj sciezke katalogu:")
  setwd(sciezka)
  DlaIluPlikow <-  readline(prompt="podaj liczbe plikow do odczytania w katalogu:")
  
  nazwaKol <-  readline(prompt="podaj nazwe kolumny do odczytania:")
  nazwaKolumny <- paste("X",nazwaKol,sep="")
  
  jakaFunkcja <-  readline(prompt="podaj nazwe funkcji, ktora chcesz wykonac[mean,median,min,max]:")
  
  # Jesli brak ilosci plikow to bierzemy tylko pierwszy
  if (is.na(DlaIluPlikow) || DlaIluPlikow == 0|| DlaIluPlikow ==""){
    print("Brak liczby plikow, domyslna wartosc to: 1")
    nfile <- dir()[1]
    path <- paste(xxy,nfile,sep="/")
    ddt <- read.csv(path)
    vv <- ddt[[nazwaKolumny]]
    vmaster <- na.omit(vv)
  }else
  { vvv <- NULL
  # Petla - tworzymy wektor dla kolumny z podanej liczby plikow
  for (i in 1:DlaIluPlikow){
    nfile <- dir()[i]
    path <- paste(xxy,nfile,sep="/")
    ddt <- read.csv(path)
    vv <- ddt[[nazwaKolumny]]
    vvv <- c(vvv, vv)
    vmaster <- na.omit(vvv)
  }
  
  }
  #Stosujemy funkcje na okreslonej kolumnie okreslonej liczby plikow
  
  #[mean,median,min,max]
  
  if (jakaFunkcja == "mean"){
    m <- mean(vmaster)
    paste("Srednia wynosi", m)
  }else if (jakaFunkcja == "median"){
    m <- median(vmaster)
    paste("Mediana wynosi", m)
  }else if (jakaFunkcja == "min"){
    m <- min(vmaster)
    paste("Wartosc minimalna wynosi", m)
  }else if (jakaFunkcja == "max"){
    m <- max(vmaster)
    paste("Wartosc maksymalna wynosi", m)
  }else{
    print("Brak takiej funkcji. Domyslna funkcja to srednia")
    m <- mean(vmaster)
    paste("Srednia wynosi", m)
  }
  
  
  
  
  
}
liczZplikow()


#na.strings=c("","NA")

#sciezka <- "D:/Papierkowa robota/PJA/R/smogKrakow"
#nazwaKolumny <- "3_temperature"
#jakaFunkcja <- "max"
#DlaIluPlikow <- 5
