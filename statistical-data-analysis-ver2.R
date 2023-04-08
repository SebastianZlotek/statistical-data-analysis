# PROJEKT PROGRAMOWANIE W R 
# Złotek Sebastian, Żak Maciej L06

#-----------------------------------------------------------
setwd("C:/Users/Sebastian/Desktop/studia 2 sem/Programowanie w R - lab") # Ustawienie glownego katalogu

#-----------------------------------------------------------

#Instalacja potrzebnych paczek
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")

#-----------------------------------------------------------

# Mozliwosc wczytania plików xls,xlsx
library("readxl")

# Biblioteka potrzebna do funkcji m.in count
library("dplyr") 

# Biblioteka do wykresów
library("ggplot2")

#Biblioteka do kolorów w wykresach
library("RColorBrewer")

#-----------------------------------------------------------

#Wczytanie pliku z ms excel
jezyki <- read_excel("Jezyki_obce_nowozytne_liczba_uczniow_rokszkolny20182019_wg_szkol.xlsx")

#Obrobka danych (Usuniecie niepotrzebnych kolumn)
jezyki$Id_Wojew <- NULL
jezyki$Id_Powiat <- NULL
jezyki$Id_Gmina <- NULL
jezyki$TypJedn_Id <- NULL
jezyki$Jedn_Ulica <- NULL
jezyki$Jedn_DomLokal <- NULL
jezyki$Jedn_KodPoczt <- NULL
jezyki$Jedn_Poczta <- NULL
jezyki$Jedn_Regon <- NULL
jezyki$Jedn_Telefon <- NULL
View(jezyki)
str(jezyki)

#Obrobka danych (podzielenie ramki na dwie mniejsze według miasta i wsi)
(miasto <-jezyki[jezyki$`Klasa Wielkości Miejscowości`=="miasto powyżej 5 tys.mieszkańców",]) 
View(miasto)
(wies <-jezyki[jezyki$`Klasa Wielkości Miejscowości`=="wieś",])
View(wies)

#-----------------------------------------------------------

#Odchylenia standardowe dla miast i wsi według jezyka dodatkowego dla poszczegolnych wojewodztw

sdmiastod <- (aggregate(miasto$`Liczba Uczniów Język Dodatkowy`, by=list(miasto$Województwo), FUN = sd))
View(sdmiastod)

sdwiesd <- (aggregate(wies$`Liczba Uczniów Język Dodatkowy`, by=list(wies$Województwo), FUN = sd))
View(sdwiesd)

#Odchylenia standardowe dla miast i wsi według jezyka obowiazkowego dla poszczegolnych wojewodztw

sdmiasto <- (aggregate(miasto$`Liczba Uczniów Język Obowiązkowy`, by=list(miasto$Województwo), FUN = sd))
View(sdmiasto)

sdwies <- (aggregate(wies$`Liczba Uczniów Język Obowiązkowy`, by=list(wies$Województwo), FUN = sd))
View(sdwies)

#-----------------------------------------------------------

#Srednia dla miast i wsi według jezyka dodatkowego dla poszczegolnych wojewodztw

meanmiastod <- (aggregate(miasto$`Liczba Uczniów Język Dodatkowy`, by=list(miasto$Województwo), FUN = mean))
View(meanmiastod)

meanwiesd <- (aggregate(wies$`Liczba Uczniów Język Dodatkowy`, by=list(wies$Województwo), FUN = mean))
View(meanwiesd)

#Srednia dla miast i wsi według jezyka obowiazkowego dla poszczegolnych wojewodztw

meanmiasto <- (aggregate(miasto$`Liczba Uczniów Język Obowiązkowy`, by=list(miasto$Województwo), FUN = mean))
View(meanmiasto)

meanwies <- (aggregate(wies$`Liczba Uczniów Język Obowiązkowy`, by=list(wies$Województwo), FUN = mean))
View(meanwies)

#-----------------------------------------------------------

#Wyswietlenie wszystkich jezykow i liczby uczacych sie

jezykisortowanie <- (sort(table(jezyki$'Jezyk Obcy')))
View(jezykisortowanie)
tail(jezykisortowanie)

#(testwykres <- pie(tail(jezykisortowanie)))

# Utworzenie pliku pdf i zapisanie do niego wykresu

pdf("posortowane.pdf")

#Dobranie barw
paletabarw <- brewer.pal(6, "Set2")
(wykres_jezykow <- pie(tail(jezykisortowanie),
                       border="white" ,
                       main = "Wykres języków oferowanych w szkołach (TOP 6)",
                       col = paletabarw))
dev.off() 

# Zapisanie jezykow i liczy uczniow do pliku CSV
write.csv(jezykisortowanie,"JezykiPosortowane.csv",row.names = T)

#-----------------------------------------------------------

# Rejestr szkół publicznych i niepublicznych, uczących języków obcych w miescie

sortowaniepublicznychmiasto <- (sort(table(miasto$Publicznosc)))
View(sortowaniepublicznychmiasto)

pdf("szkolymiasto.pdf")

wykresszkolmiasto <- barplot(sortowaniepublicznychmiasto,
        ylim = c(0,35000),
        ylab = "Liczba szkol",
        main = "Wykres szkół w mieście",
        col=brewer.pal(n = 4, name = "PRGn")
        )


# Rejestr szkół publicznych i niepublicznych, uczących języków obcych na wsi

dev.off()

sortowaniepublicznychwies <- (sort(table(wies$Publicznosc)))
View(sortowaniepublicznychwies)

pdf("szkolywies.pdf")

wykresszkolwies <- barplot(sortowaniepublicznychwies,
                             ylim = c(0,35000),
                             ylab = "Liczba szkol",
                             main = "Wykres szkół w mieście",
                             col=brewer.pal(n = 3, name = "RdBu")
                             )

dev.off()

#-----------------------------------------------------------

# Najbardziej popularne języki w woj. Podkarpackim

wojewodztwouczniowiepodkarpackie <- jezyki[jezyki$Województwo=="WOJ. PODKARPACKIE",]
View(wojewodztwouczniowie)
wups <- (sort(table(wojewodztwouczniowiepodkarpackie$`Jezyk Obcy`)))

pdf("podkarpackie.pdf")

wojewodztwawykres <- barplot(wups,
                             ylim = c(0,3000),
                             main = "Ilosc szkol uczacych danego jezyka w woj. Podkarpackim",
                             col=brewer.pal(n = 6, name = "Dark2")
)

dev.off()

# Najbardziej popularne języki w woj. Mazowieckim

wojewodztwouczniowiemazowieckie <- jezyki[jezyki$Województwo=="WOJ. MAZOWIECKIE",]
View(wojewodztwouczniowiemazowieckie)
wums <- (tail(sort(table(wojewodztwouczniowiemazowieckie$`Jezyk Obcy`))))
View(zmiennatestowa)

pdf("mazowieckie.pdf")

wojewodztwawykres <- barplot(wums,
                           ylim = c(0,3000),
                           main = "Ilosc szkol uczacych danego jezyka w woj. Mazowieckim",
                           col=brewer.pal(n = 6, name = "PiYG")
)

dev.off()

#-----------------------------------------------------------

# Liczba szkół w województwach w mieście

liczbaszkolmiasto <- (sort(table(miasto$Województwo)))
View(liczbaszkolmiasto)

# Liczba szkół w województwach na wsi

liczbaszkolwies <- (sort(table(wies$Województwo)))
View(liczbaszkolwies)

#-----------------------------------------------------------

# Języki obce w poszczególnych wojewodztwach

w1 <- jezyki[jezyki$Województwo=="WOJ. DOLNOŚLĄSKIE",]
w2 <- jezyki[jezyki$Województwo=="WOJ. KUJAWSKO-POMORSKIE",]
w3 <- jezyki[jezyki$Województwo=="WOJ. LUBELSKIE",]
w4 <- jezyki[jezyki$Województwo=="WOJ. LUBUSKIE",]
w5 <- jezyki[jezyki$Województwo=="WOJ. ŁÓDZKIE",]
w6 <- jezyki[jezyki$Województwo=="WOJ. MAŁOPOLSKIE",]
w7 <- jezyki[jezyki$Województwo=="WOJ. MAZOWIECKIE",]
w8 <- jezyki[jezyki$Województwo=="WOJ. OPOLSKIE",]
w9 <- jezyki[jezyki$Województwo=="WOJ. PODKARPACKIE",]
w10 <- jezyki[jezyki$Województwo=="WOJ. PODLASKIE",]
w11 <- jezyki[jezyki$Województwo=="WOJ. POMORSKIE",]
w12 <- jezyki[jezyki$Województwo=="WOJ. ŚLĄSKIE",]
w13 <- jezyki[jezyki$Województwo=="WOJ. ŚWIĘTOKRZYSKIE",]
w14 <- jezyki[jezyki$Województwo=="WOJ. WARMIŃSKO-MAZURSKIE",]
w15 <- jezyki[jezyki$Województwo=="WOJ. WIELKOPOLSKIE",]
w16 <- jezyki[jezyki$Województwo=="WOJ. ZACHODNIOPOMORSKIE",]

s1 <- (sort(table(w1$'Jezyk Obcy')))
s2 <- (sort(table(w2$'Jezyk Obcy')))
s3 <- (sort(table(w3$'Jezyk Obcy')))
s4 <- (sort(table(w4$'Jezyk Obcy')))
s5 <- (sort(table(w5$'Jezyk Obcy')))
s6 <- (sort(table(w6$'Jezyk Obcy')))
s7 <- (sort(table(w7$'Jezyk Obcy')))
s8 <- (sort(table(w8$'Jezyk Obcy')))
s9 <- (sort(table(w9$'Jezyk Obcy')))
s10 <- (sort(table(w10$'Jezyk Obcy')))
s11 <- (sort(table(w11$'Jezyk Obcy')))
s12 <- (sort(table(w12$'Jezyk Obcy')))
s13 <- (sort(table(w13$'Jezyk Obcy')))
s14 <- (sort(table(w14$'Jezyk Obcy')))
s15 <- (sort(table(w15$'Jezyk Obcy')))
s16 <- (sort(table(w16$'Jezyk Obcy')))

View(s1)
View(s2)
View(s3)
View(s4)
View(s5)
View(s6)
View(s7)
View(s8)
View(s9)
View(s10)
View(s11)
View(s12)
View(s13)
View(s14)
View(s15)
View(s16)

#-----------------------------------------------------------

#Filtrowanie danych z warunkami

(filtrowaniemiasto <- miasto[(!is.na(miasto$Jedn_www) & !is.na(miasto$Jedn_eMail) & !is.na(miasto$Jedn_Patron)) | !(miasto$`Typ Jednostki`=="Szkoła podstawowa"), ])

View(filtrowaniemiasto)

(filtrowaniewies <- wies[(!is.na(wies$Jedn_www) & !is.na(wies$Jedn_eMail) & !is.na(wies$Jedn_Patron)) | !(wies$`Typ Jednostki`=="Szkoła podstawowa"), ])

View(filtrowaniewies)

#-----------------------------------------------------------

#Maksiumum minimum miasta / wsie

#zmienna <- miasto[max(miasto$`Liczba Uczniów Język Obowiązkowy`),]
#View(zmienna)

#max(miasto$`Liczba Uczniów Język Obowiązkowy`)
#(max(miasto$`Liczba Uczniów Język Dodatkowy`))













































































