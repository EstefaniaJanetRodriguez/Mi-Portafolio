#TWITTER
#En el presente archivo, podrá ver un analisis realizado sobre datos de la red social Tweeter.
#Se analizó seguidores, seguidos del tenista Rafel Nadal.
# Paginas marcadas como favoritas por Rafa Nadal.
#El hashtagh mas visto, cantidad de veces que se twiteeó sobre un tema particular.
#Se hizo una comparacion de seguidores en comun entre Rafa Nadal y roger Federer.
#Cada set de datos obtenido, tiene su grafica correspondiente.
#Uso de librerias.

library("rtweet")
library("tidyverse")

#Obtengo tweets
tw_search <- search_tweets(q = "COVID19", 
                    n = 100)
#Veo los tweets
tw_search %>% View()

#Veo los tweets
tw_search %>% glimpse()

#obtengo los twitter en español
tw_search <- tw_search %>% filter(lang == 'es')

tw_search %>% count(screen_name)  %>% arrange(desc(n))


#Obtengo tweets de un usuario particular
usuario = "RafaelNadal"
time_line_RN = get_timeline(user = usuario, n = 100)

time_line_RN %>% View()

#Obtengo seguidores de un usuario particular
usuario = "rnadalacademy"
fl_RN = get_followers(user = usuario, n = 67500)

fl_RN %>% View()

#Obtengo el perfil del usuario AL QUE RAFA le puso MG
usuario = "RafaelNadal"
perf_RN = users_data(fav_RN)

perf_RN %>% View()


#Obtengo cuentas a las que sigue de un usuario particular
usuario = "RafaelNadal"
fr_RN = get_friends(user = usuario, n = 5000)


fr_RN %>% View()


#Obtengo los favoritos de un usuario particular
usuario = "RafaelNadal"
fav_RN = get_favorites(user = usuario, n = 1000)

fav_RN %>% View()

#Lo que mas le gusta a RAfael Nadal - Tyding
dat <- fav_RN %>% count(screen_name) %>% arrange(desc(n)) %>% head(5)

#Graficamos - Mapping
grafica <- ggplot(data = dat, mapping = aes(x = reorder(screen_name, n), y = n))

#Geom
grafica <- grafica + geom_point() 

#labels

grafica <- grafica + labs(title = "Cuentas que mas les gusta a Rafael Nadal",
                          y = "# de MG", x = "Nombre de cuentas")

grafica


#Otro grafico - Barras que presentan el numero de MG
grafica <- ggplot(data = dat, mapping = aes(x = reorder(screen_name, n), y = n))
grafica <- grafica + geom_bar(stat = "identity", fill = "red") 
grafica <- grafica + labs(title = "Cuentas que mas les gusta a Rafael Nadal",
                          y = "# de MG", x = "Nombre de cuentas")
grafica

#estilos
grafica + theme_dark()


#Instalo una libreria llamada lubridate para fechas.
install.packages("lubridate")
library("lubridate")

#Numero de tweets por año

dat <- fav_RN %>% 
  mutate(fecha = ymd_hms(created_at)) %>%
  mutate(fecha = format(fecha, "%Y")) %>%
  select(created_at,fecha ) %>%
  count(fecha)

#grafica
favoritos_nadal <- ggplot(data = dat, mapping = aes(x = fecha, y = n))

favoritos_nadal <- favoritos_nadal + geom_bar(stat = "identity", fill = "blue")

favoritos_nadal <- favoritos_nadal + labs(title = "Grafica",
                          y = "Cantidad de MG", x = "Año")
favoritos_nadal


#Obtengo los hashtags mas comunes
install.packages("tidytext")
library("tidytext")

install.packages("tidyar")
library("tidyar")

tw_futbol <- search_tweets("fútbol", n=5000, lang = 'es', include_rts = FALSE)


tw_futbol <- tw_futbol %>% 
  unnest(hashtags) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  filter(!is.na(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n)) %>% 
  head(20) %>% View()

#dibujo

tw_grafica = tw_futbol

tw_grafica <- ggplot(data = tw_grafica, 
                  mapping = aes(x = reorder(hashtags, n), y = n))

tw_grafica <- tw_grafica + geom_bar(stat = "identity") 

tw_grafica <- tw_grafica + coord_flip()

tw_grafica

#WORDCLOUD
install.packages("wordcloud")
library("wordcloud")
install.packages("RColorBrewer")
library("RColorBrewer")

tw_wordcloud_hsthtags <- tw_futbol %>% 
  unnest(hashtags) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  filter(!is.na(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n))

wordcloud(words = tw_wordcloud_hsthtags$hashtags,
             freq = tw_wordcloud_hsthtags$n,
          colors = brewer.pal(9, "RdBu"))


#datos de creacion de las cuentas a las que rafa nadal le dio MG

users <- fav_RN %>% 
  distinct(user_id, account_created_at) %>% 
  select(account_created_at) %>% 
  mutate(anio = account_created_at %>% format("%Y"))  %>% 
  count(anio)

p <- ggplot(data = users, mapping = aes(x = anio, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Cuentas creadas por año", x= "Año", y = "Cantidad")


#Amigos de rafa nadal y roger federer en comun
usuario ="RogerFederer"
fr_RF = get_friends(user = usuario, n = 5000)

install.packages("VennDiagram")
library("VennDiagram")

venn.diagram(x = list(roger = fr_RF$user_id, nadal= fr_RN$user_id),
             filename="phto.png",
             fill = c("Red3", "Blue"),
             alpha = 0.5,
             cex=2.1)

