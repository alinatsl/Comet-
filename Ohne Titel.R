##3.1
n=10
#Anzahl an Simulationen
simulations=1000
#Function um zu ??berpr??fen, ob jemand sein eigenes Geschenk zur??ckerh??lt
check_own_gift=function(n){
  gifts=sample(1:n)
  return(any(gifts ==1:n))
}
#Durchf??hren von Simulationen
results=replicate(simulations, check_own_gift(n))

#Berechnen der Wahrscheinlichkeit
probability = sum(results)/simulations

print(paste("Die Wahrscheinlichkeit, dass mindestens eine Person ihr eigenes Geschenk zur??ckerh??lt betr??gt etwa", round(probability * 100, 2), "%"))




##3.2 
wichtel_unglueck <- function(n, k, iterationen=1000) {
  zaehler <- 0   
  
  # Ausnahmen 
  #k negativ 
  if(k<0) {
    stop("k darf nicht negativ sein")
  }
  #n negativ 
  if(n<=0) {
    stop("n darf nicht negativ sein")
  }
  # Iterationen negativ 
  if(iterationen<=0) {
    stop("Iterationen d??rfen nicht negativ sein")
  }
  # k>n
  if(k>n) {
    stop("k darf nicht gr????er als n sein")
  }
  # Schleife bauen 
  for (i in 1:iterationen) {
    # Zuf??llige Zuordnung von Geschenken zu Personen
    zuordnung <- sample(1:n, size = n, replace = FALSE)
    
    # ??berpr??fung: Haben mindestns k Personen ihr eigenes Geschenk gezogen 
    if (sum(zuordnung == 1:n) >= k) {
      zaehler <- zaehler + 1
    }
  }
  # Wahrscheinlichkeit sch??tzen
  p <- zaehler / iterationen
  
  # Wahrscheinlichkeit in Prozent umrechnen
  prozent <- (p* 100)
  
  # Ergebnis 
  cat("Die Wahrscheinlichkeit, dass mindestens", k, "Person(en) ihr eigenes Geschenk ziehen, betr??gt:", prozent, "%?n")
  return(p)
} 

# Beispiel: n = 100, k = 5 und iterationen = 1000 #Bemerkung; Ergebnis anders je nach Wahl 
wichtel_unglueck(n = 100, k = 2, iterationen = 1000)
# Kontrollieren von Aufgabe 1 anhand von Aufgabe 2
wichtel_unglueck(n=10, k=1, iterationen=1000)


##3.3 Kommentare siehe R-Skript 

##3.4

install.packages("testthat")
library(testthat)

test_that("wichtel_unglueck gibt einen Fehler zur??ck wenn n negativ ist", {
  expect_error(wichtel_unglueck(-5, 2), "n darf nicht negativ sein")
})
test_that("wichtel_unglueck gibt einen Fehler zur??ck, wenn k negativ ist", {
  expect_error(wichtel_unglueck(5, -2), "k darf nicht negativ sein")
})
test_that("wichtel_unglueck gibt einen Fehler zur??ck wenn Iterationen negativ sind", {
  expect_error(wichtel_unglueck(2, 2, iterationen=0), "Iterationen d??rfen nicht negativ sein")
})
test_that("wichtel_unglueck gibt einen Fehler zur??ck, wenn k gr????er als n ist", {
  expect_error(wichtel_unglueck(5, 10), "k darf nicht gr????er als n sein")
})

##3.5 
#Daten einlesen 
data.frame <- read.csv(
  file = "/Users/alinatuschl//Downloads/Finale_datei.csv",
  header = TRUE, 
  sep = ",",
  dec = ".")
class(data.frame)
# Daten nach Station filtern 
meine.daten<- subset(data.frame, station=="4th & C St SW")
meine.daten
View(meine.daten)
# auf NAs pr??fen 
anyNA(data.frame) ##im Folgenden werden alle Spalten auf NAs gepr??ft 
any(is.na(meine.daten$date))
any(is.na(meine.daten$station))
any(is.na(meine.daten$snow_depth))
any(is.na(meine.daten$count))
any(is.na(meine.daten$wind_speed))
any(is.na(meine.daten$precipitation))
any(is.na(meine.daten$snowfall))
any(is.na(meine.daten$mean_temperature))
any(is.na(meine.daten$max_temperature))
any(is.na(meine.daten$min_temperature))
##Folge: Nas in mean_temperature und count enthalten 
# Umgang mit NAs
which(is.na(meine.daten)) ##herausfinden, welche Datenpunkte von Nas betroffen sind 
meine.daten_ohneNA <- na.omit(meine.daten) ## NAs werden aus Datensatz entfernt 
anyNA(meine.daten_ohneNA) ##??berpr??fen, ob Entfernung erfolgreich war 
# auf Datenanomalien pr??fen 
range(meine.daten_ohneNA$date)
range(meine.daten_ohneNA$station)
range(meine.daten_ohneNA$count)
range(meine.daten_ohneNA$wind_speed) ##Auff??llig: Wert von -1 nicht plausibel
range(meine.daten_ohneNA$precipitation)
range(meine.daten_ohneNA$snowfall)
range(meine.daten_ohneNA$snow_depth)
range(meine.daten_ohneNA$mean_temperature)
range(meine.daten_ohneNA$max_temperature)
range(meine.daten_ohneNA$min_temperature)
# Umgang mit Datenanomalien 
## Datenpunkt mit -1 wird aus dem Datensatz entfernt 
meine.daten_final <- subset(meine.daten_ohneNA, wind_speed>-1)
## ??berpr??fen, ob Entfernung erfolgreich war 
range(meine.daten_final$wind_speed)

##Vorbereitung 4 
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("gridExtra")
library(gridExtra)
install.packages("plotly")
library(plotly)
install.packages("gapminder")
library(gapminder)

##4.1 Zusammenhang f??r jedes Merkmal wie folgt: 
# Fahrr??der und Temperatur 
ggplot(data=meine.daten_final)+
  geom_point(aes(x=count, y=mean_temperature))+
  xlab("Anzahl ausgeliehener Fahrr??der")+
  ylab("Temperatur")+
  ggtitle("Zusammenhang Fahrr??der und Temperatur")
#Fahrrr??der und Niederschlagsmenge 
ggplot(data=meine.daten_final)+
  geom_point(aes(x=count, y=precipitation))+
  xlab("Anzahl ausgeliehener Fahrr??der")+
  ylab("Niederschlagsmenge")+
  ggtitle("Zusammenhang Fahrr??der und Niederschlagsmenge")
#Fahrr??der und Windgeschwingigkeit 
ggplot(data=meine.daten_final)+
  geom_point(aes(x=count, y=wind_speed))+
  xlab("Anzahl ausgeliehener Fahrr??der")+
  ylab("Windgeschwindigkeit")+
  ggtitle("Zusammenhang Fahrr??der und Windgeschwindigkeit")
#Fahrr??der und Zeit 
meine.daten_final$date <- as.Date(meine.daten_final$date)
ggplot(data=meine.daten_final)+
  geom_point(aes(x=count, y=date))+
  xlab("Anzahl ausgeliehener Fahrr??der")+
  ylab("Zeit")+
  ggtitle("Zusammenhang Fahrr??der und Zeit")


##4.2 Zusammenhang f??r Fahrr??der und Temperatur 

#Tage, an denen es nicht geregnet hat 
ggplot(data = filter(meine.daten_final, precipitation== 0)) +
  geom_point(aes(x = count, y = mean_temperature)) +
  xlab("Ausgeliehene Fahrr??der") +
  ylab("Temperatur") +
  ggtitle("Zusammenhang und Temperatur ohne Regen")

#Tage, an denen es geregnet hat 
ggplot(data = filter(meine.daten_final, precipitation>0)) +
  geom_point(aes(x = count, y = mean_temperature)) +
  xlab("Ausgeliehene Fahrr??der") +
  ylab("Temperatur") +
  ggtitle("Zusammenhang ausgeliehene Fahrr??der und Temperatur bei Regen")

##4.3 Verteilungen 
#Anzahl ausgeliehenener Fahrr??der 
ggplot(data=meine.daten_final)+
  geom_density(aes(x=count))+
  xlab("Anzahl ausgeliehene Fahrr??der")+
  ylab("Verteilung")+
  ggtitle("Verteilung der Anzahl ausgeliehener Fahrr??der")
#Temperatur 
ggplot(data=meine.daten_final)+
  geom_density(aes(x=mean_temperature))+
  xlab("Temperatur")+
  ylab("Verteilung")+
  ggtitle("Verteilung der Temperatur")
#Niederschlagsmenge 
ggplot(data=meine.daten_final)+
  geom_density(aes(x=precipitation))+
  xlab("Niederschlagsmenge")+
  ylab("Verteilung")+
  ggtitle("Verteilung der Niederschlagsmenge")
#Windgeschwindigkeit 
ggplot(data=meine.daten_final)+
  geom_density(aes(x=wind_speed))+
  xlab("Windgeschwindigkeit")+
  ylab("Verteilung")+
  ggtitle("Verteilung der Windgeschwindigkeit")


##4.4 Kerndichtesch??tzer 
##Speichern von Jahreszeiten 
Winter<- subset(meine.daten_final, date< "2022-03-20")
Fr??hling <- subset(meine.daten_final, date>= "2022-03-20", date <"2022-06-21")
Sommer <- subset(meine.daten_final, date>="2022-06-21", date< "2022-09-23" )
Herbst <- subset(meine.daten_final, date>="2022-09-23", date< "2022-12-21")
# Winter 2022 nicht erw??hnt, da Daten nicht vorhanden 
Jahreszeiten <- ggplot() +
  geom_density(data=Winter, aes(x=count, col="Winter"))+
  geom_density(data=Fr??hling, aes(x=count, col="Fr??hling"))+
  geom_density(data=Sommer, aes(x=count, col="Sommer"))+
  geom_density(data=Herbst, aes(x=count, col="Herbst"))+
  labs(color="Legende")+
  xlab("Anzahl ausgeliehener Fahrr??der")+
  ylab("Verteilung")+
  ggtitle("Verteilung der Anzahl ausgeliehener Fahrr??der in den unterschiedlichen Jahreszeiten")+
  theme_classic()+
  theme(legend.text = element_text(size=20))+
  scale_colour_manual(values=c("lightgreen", "brown", "red", "darkblue"))+
  guides(color=guide_legend(override.aes = list(fill=c("lightgreen", "brown", "red", "darkblue"))))

print(Jahreszeiten)


##4.5 3D-Scatterplot 


Scatterplot <- plot_ly(data = meine.daten_final, x = ?mean_temperature, y = ?wind_speed, z = ?count, type = "scatter3d",
        mode = "markers", marker = list(size = 5, opacity = 0.5), color = ?count,
        text = ?paste("durchschnittliche Temperatur:", mean_temperature, 
                "<br>Windgeschwindigkeit:", wind_speed, "<br>Ausgeliehene Fahrr??der:", count))
Scatterplot %>% layout(scene=list(xaxis=list(title="mittlere Temperatur"), 
                  yaxis=list(title="Windgeschwindigkeit"), 
                  zaxis=list(title="Fahrr??der")))










