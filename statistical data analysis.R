# Statystyczna Analiza Danych, Projekt 

#-----------------------------------------------------------

# Ustawienie glownego katalogu
setwd("filepath")
getwd()

#-----------------------------------------------------------

#Instalacja potrzebnych paczek
install.packages("readxl") #instalacja paczki do wczytania plików xls,xlsx
install.packages("ggplot2") #instalacja paczki do wykresów
install.packages("RColorBrewer") #instalacja paczki do kolorów
install.packages("plotrix") #instalacja paczki do wykresu pie3D
install.packages("moments") #instalacja paczki do miary koncentracji i skośności

#-----------------------------------------------------------

# Mozliwosc wczytania plików xls,xlsx
library("readxl")

# Biblioteka do wykresów
library("ggplot2")

#Biblioteka do kolorów w wykresach
library("RColorBrewer")

#Biblioteka do wykresu trójwymiarowego pie3D
library("plotrix")

#Biblioteka do miary koncentracji i skośności
library("moments")

#-----------------------------------------------------------

#Wczytanie pliku z MS EXCEL
jezyki <- read_excel("Jezyki_obce_nowozytne_liczba_uczniow_rokszkolny20182019_wg_szkol.xlsx")

#Obróbka danych (Usuniecie niepotrzebnych kolumn)
jezyki <- jezyki[, -c(1,3,5,10:26)]

#Obrobka danych (podzielenie ramki na dwie mniejsze według miast i wsi)
(miasto <-jezyki[jezyki$`Klasa Wielkości Miejscowości`=="miasto powyżej 5 tys.mieszkańców",]) 
(wies <-jezyki[jezyki$`Klasa Wielkości Miejscowości`=="wieś",])

#-----------------------------------------------------------

#Parametry opisowe

#Odchylenia standardowe dla miast i wsi według języka obowiazkowego

sdmiasto <- sd(miasto$`Liczba Uczniów Język Obowiązkowy`)
sdmiasto
sdwies <- sd(wies$`Liczba Uczniów Język Obowiązkowy`)
sdwies

#Odchylenia standardowe dla miast i wsi według języka dodatkowego

sdmiastododatkowy <- sd(miasto$`Liczba Uczniów Język Dodatkowy`)
sdmiastododatkowy
sdwiesdodatkowy <- sd(wies$`Liczba Uczniów Język Dodatkowy`)
sdwiesdodatkowy

#Srednia dla miast i wsi według języka obowiązkowego

meanmiasto <- mean(miasto$`Liczba Uczniów Język Obowiązkowy`)
meanmiasto
meanwies <- mean(wies$`Liczba Uczniów Język Obowiązkowy`)
meanwies

#Srednia dla miast i wsi według języka dodatkowego

meanmiastododatkowy <- mean(miasto$`Liczba Uczniów Język Dodatkowy`)
meanmiastododatkowy
meanwiesdodatkowy <- mean(wies$`Liczba Uczniów Język Dodatkowy`)
meanwiesdodatkowy

#Mediana dla miast i wsi według języka obowiązkowego

medianmiasto <- median(miasto$`Liczba Uczniów Język Obowiązkowy`)
medianmiasto
medianwies <- median(wies$`Liczba Uczniów Język Obowiązkowy`)
medianwies

#Mediana dla miast i wsi według języka dodatkowego

medianmiastododatkowy <- median(miasto$`Liczba Uczniów Język Dodatkowy`)
medianmiastododatkowy
medianwiesdodatkowy <- median(wies$`Liczba Uczniów Język Dodatkowy`)
medianwiesdodatkowy

#Kwartyle dla miast i wsi według języka obowiązkowego 
quantilemiasto <- quantile(miasto$`Liczba Uczniów Język Obowiązkowy`)
quantilemiasto
quantilewies <- quantile(wies$`Liczba Uczniów Język Obowiązkowy`)
quantilewies

#Kwartyle dla miast i wsi według języka dodatkowego
quantilemiastododatkowy <- quantile(miasto$`Liczba Uczniów Język Dodatkowy`)
quantilemiastododatkowy
quantilewiesdodatkowy <- quantile(wies$`Liczba Uczniów Język Dodatkowy`)
quantilewiesdodatkowy

#Wariancja dla miast i wsi według języka obowiązkowego 
varmiasto <- var(miasto$`Liczba Uczniów Język Obowiązkowy`)
varmiasto
varwies <- var(wies$`Liczba Uczniów Język Obowiązkowy`)
varwies

#Wariancja dla miast i wsi według języka dodatkowego
varmiastododatkowy <- var(miasto$`Liczba Uczniów Język Dodatkowy`)
varmiastododatkowy
varwiesdodatkowy <- var(wies$`Liczba Uczniów Język Dodatkowy`)
varwiesdodatkowy

#Miara koncentracji dla miast i wsi według języka obowiązkowego 
kurtosismiasto <- kurtosis(miasto$`Liczba Uczniów Język Obowiązkowy`)
kurtosismiasto
kurtosiswies <- kurtosis(wies$`Liczba Uczniów Język Obowiązkowy`)
kurtosiswies

#Miara koncentracji dla miast i wsi według języka dodatkowego
kurtosismiastododatkowy <- kurtosis(miasto$`Liczba Uczniów Język Dodatkowy`)
kurtosismiastododatkowy
kurtosiswiesdodatkowy <- kurtosis(wies$`Liczba Uczniów Język Dodatkowy`)
kurtosiswiesdodatkowy

#Skośność dla miast i wsi według języka obowiązkowego 
skewnessmiasto <- skewness(miasto$`Liczba Uczniów Język Obowiązkowy`)
skewnessmiasto
skewnesswies <- skewness(wies$`Liczba Uczniów Język Obowiązkowy`)
skewnesswies

#Skośność dla miast i wsi według języka dodatkowego
skewnessmiastododatkowy <- skewness(miasto$`Liczba Uczniów Język Dodatkowy`)
skewnessmiastododatkowy
skewnesswiesdodatkowy <- skewness(wies$`Liczba Uczniów Język Dodatkowy`)
skewnesswiesdodatkowy

#Współczynnik zmienności dla miast i wsi według języka obowiązkowego 
wzmiasto <- sd(miasto$`Liczba Uczniów Język Obowiązkowy`) / mean(miasto$`Liczba Uczniów Język Obowiązkowy`)
wzmiasto
wzwies <- sd(wies$`Liczba Uczniów Język Obowiązkowy`) / mean(wies$`Liczba Uczniów Język Obowiązkowy`)
wzwies

#Współczynnik zmienności dla miast i wsi według języka dodatkowego
wzmiastododatkowy <- sd(miasto$`Liczba Uczniów Język Dodatkowy`) / mean(miasto$`Liczba Uczniów Język Dodatkowy`)
wzmiastododatkowy
wzwiesdodatkowy <- sd(wies$`Liczba Uczniów Język Dodatkowy`) / mean(wies$`Liczba Uczniów Język Dodatkowy`)
wzwiesdodatkowy

#Błąd standardowy dla miast i wsi według języka obowiązkowego 
bsmiasto <- sd(miasto$`Liczba Uczniów Język Obowiązkowy`) / sqrt(length(miasto$`Liczba Uczniów Język Obowiązkowy`))
bsmiasto
bswies <- sd(wies$`Liczba Uczniów Język Obowiązkowy`) / sqrt(length(wies$`Liczba Uczniów Język Obowiązkowy`))
bswies

#Błąd standardowy dla miast i wsi według języka dodatkowego
bsmiastododatkowy <- sd(miasto$`Liczba Uczniów Język Dodatkowy`) / sqrt(length(miasto$`Liczba Uczniów Język Dodatkowy`))
bsmiastododatkowy
bswiesdodatkowy <- sd(wies$`Liczba Uczniów Język Dodatkowy`) / sqrt(length(wies$`Liczba Uczniów Język Dodatkowy`))
bswiesdodatkowy
#-----------------------------------------------------------

#Wizualizacja danych

options(scipen=999)

#Suma uczniów w miastach i wsiach dla poszczególnych województw

miastosuma <- aggregate(miasto$`Liczba Uczniów Język Obowiązkowy`, by=list(miasto$Województwo), FUN = sum)
wiessuma <- aggregate(wies$`Liczba Uczniów Język Obowiązkowy`, by=list(wies$Województwo), FUN = sum)

#Wykres liczby uczniów w województwach w mieście

ggplot(data.frame(miastosuma), aes(Group.1, x, group=1)) + geom_line(color="green", size = 2) + theme(axis.text.x = element_text(angle=90, vjust=0.5)) + scale_y_continuous(breaks=seq(50000,900000,by=100000)) + ylab("Liczba uczniów") + xlab("Województwa") + ggtitle("Wykres liczby uczniów w miastach") + theme(plot.title = element_text(hjust = 0.5))

#Wykres liczby uczniów w województwach na wsi

ggplot(data.frame(wiessuma), aes(Group.1, x, group=1)) + geom_line(color="red", size = 2) + theme(axis.text.x = element_text(angle=90, vjust=0.5)) + scale_y_continuous(breaks=seq(50000,900000,by=100000)) + ylab("Liczba uczniów") + xlab("Województwa") + ggtitle("Wykres liczby uczniów na wsi") + theme(plot.title = element_text(hjust = 0.5))


#Wykres liczby uczniów uczących się danego języka podstawowego lub dodatkowego w województwie mazowieckim

wojewodztwouczniowiemazowieckie <- jezyki[jezyki$Województwo=="WOJ. MAZOWIECKIE",]
wums <- (tail(sort(table(wojewodztwouczniowiemazowieckie$`Jezyk Obcy`))))
wojewodztwawykres <- barplot(wums,
                             ylim = c(0,8000),
                             main = "Ilość uczniów uczących się danego języka w woj. Mazowieckim",
                             col=brewer.pal(n = 6, name = "PiYG")
)

#Wykres języków oferowanych w szkołach bez najpopularniejszych (niemieckiego i angielskiego)

jezykisortowanie <- head(tail((sort(table(jezyki$'Jezyk Obcy'),DECREASING=F)), n=7), n=5)
lbls <- paste(names(jezykisortowanie), jezykisortowanie, sep=" ")
paletabarw <- brewer.pal(8, "Set2")
(wykres_jezykow <- pie3D(jezykisortowanie,
                       border="white" ,
                       main = "Wykres języków oferowanych w szkołach bez najpopularniejszych języków \n (angielski i niemiecki)",
                       labels = lbls,
                       theta = 0.95,
                       explode = 0.15,
                       height = 0.2,
                       col = paletabarw))

#Wykres najrzadziej wybieranych języków razem z liczbą uczniów

pop <- c(1,1,1,1,2,3,3,3,3,6,6,14,27) #przypisanie ilości kropek do wektora
najmniejuczacych <- head(sort(table(jezyki$'Jezyk Obcy'),DECREASING=F), n = 13) #wybranie 13 najrzadziej wybieranych języków
najmniejuczacych <- as.data.frame(najmniejuczacych) #zamiana na ramke danych
ggplot(najmniejuczacych, aes(Var1,Freq, size = pop)) +
  geom_point(alpha=0.7) + theme(axis.text.x = element_text(angle=90, vjust=0.5)) + scale_size(range = c(5, 33), name="Population (M)") +
scale_y_continuous(breaks=seq(0,40,by=3)) + ylab("Liczba uczniów") + xlab("Język obcy") + ggtitle("Wykres najrzadziej wybieranych języków razem z liczbą uczniów") + theme(plot.title = element_text(hjust = 0.5))

#Hipotezy

#Hipoteza 1
#Średnia uczniów uczących się języka obowiązkowego w województwach w Polsce jest równa 93

hipoteza1 <- t.test(x=jezyki$`Liczba Uczniów Język Obowiązkowy`, mu=93)
hipoteza1

#Hipoteza 2
#Średnia uczniów uczących się języka dodatkowego w województwach w Polsce jest równa niemal 1/3 średniej uczniów języka obowiązkowego

hipoteza2 <- t.test(x=jezyki$`Liczba Uczniów Język Dodatkowy`, mu=30)
hipoteza2

#Średnia uczniów uczących się języka dodatkowego w województwach w Polsce jest równa 8
hipoteza3 <- t.test(x=jezyki$`Liczba Uczniów Język Dodatkowy`, mu=8)
hipoteza3

































