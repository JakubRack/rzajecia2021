library(stringr)
library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(gtools)
library(rvest)

#Postawienie selenium
#java -jar selenium-server-standalone-3.0.1.jar -port 4445




remDr<-remoteDr(remoteServerAddr = "http://localhost",
                port=4445,
                browserName = "chrome",
                newSession = TRUE
)
remDr%>% go("https://www.otomoto.pl/osobowe/")

#Zebranie linkow


#a) Sposob wolniejszy
wektorLinkow<-c()
for(i in 1:500){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/?search%5Border%5D=created_at%3Adesc&page=",i)
  remDr%>%go(newUrl)
  elems<-remDr%>%findElements(using = "tag name", "h2")
  for ( j in 1:length(elems)){
    e<-findElementsFromElement(elems[[j]],using = "tag name", "a")
    if(length(e)>0){
      link<-e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}
wektorLinkow



#b) Sposob szybszy
wektorLinkow<-c()
for(i in 1:500){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/?search%5Border%5D=created_at%3Adesc&page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]/article[*]/div[2]/div[1]/div[1]/h2/a')
  wektorLinkow<-c(wektorLinkow,xml_attr(result,"href"))
}
wektorLinkow



# METODA 2

zrobWierszRvest<-function(w,wektorLinkow,remDr){
  nUrl<-wektorLinkow[w]
  page<-read_html(nUrl)
  #Odczytanie ceny
  cen<-html_node(page,".offer-price__number")%>%html_text()
  cena<- gsub("\\s?\n|\\s", "", cen)
  
  #Odczytanie kategorii
  war<-html_nodes(page,".offer-params__label")%>%html_text()
  #Odczytanie wartosci
  values<-html_nodes(page,".offer-params__value")%>%html_text()
  vv<- gsub("\n", "", values)
  v <- str_squish(vv)
  
  
  indexy<-seq(1,length(v),1)
  nazwyKolumn<- war
  wartosci<- v
  df1<- data.frame (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
}

wektorLinkowU<-wektorLinkow%>%unique()

samochody<-NULL
liczbaLinkow<-length(wektorLinkowU)
for(w in 1: liczbaLinkow ){
  print(paste0(w," / ",liczbaLinkow ) )
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU,remDr ),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(samochody)){
    samochody<-df1
  }else{
    samochody<-smartbind(samochody,df1)
  }
}

View(samochody)