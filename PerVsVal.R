
# librerias ---------------------------------------------------------------



library(tidyverse)
library(rvest)
library(janitor)
library(ggrepel)
library(ggtext)


# Tema --------------------------------------------------------------------


theme_ivo <- function() {
  theme_minimal(base_size = 9, base_family = "Inconsolata") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#fff5e3", color = "#fff5e3"),
      plot.caption = element_markdown(size = 7.5, family = "Chivo", color = "#626060")
    )
}


# Número de jornada -------------------------------------------------------



jor <- read_html("https://www.acb.com/") %>%
  html_element("h2") %>%
  html_text("class") %>%
  str_extract(., "[0-9]+")


# csv con logos nombres y colores acb -------------------------------------



teams <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/acbTeams2022/main/acb.csv") %>%
  select(abbRm, logo, color)


# Función que extrae los datos de GM --------------------------------------



pag <- c(1,2, 3)

gm_tbl <- function(pag){
  url <- paste0("https://basketball.realgm.com/international/league/4/Spanish-ACB/stats/2022/Advanced_Stats/All/All/per/All/desc/",pag,"?pace_adjustment=") %>%
  read_html()

df <- url %>% html_element("table") %>%
  html_table() %>%
  clean_names()
return(df)
}

gmtbldf <- map_df(pag, gm_tbl)




# Datos el Rincon del super manager ---------------------------------------



url <- "https://www.rincondelmanager.com/smgr/valoracion.php"

pgsession <- session(url)
pgform <- html_form(pgsession)[[5]]
filled_form <- html_form_set(pgform,
                             "de_jor"  =  "1",
                             "a_jor"   =  "4",
                             "cancha"  =  "2"
)

df <- submit_form(session = pgsession, form = filled_form, POST = url)

df_rs1 <- df %>% html_element("table#tTodos") %>%
  html_table() %>%
  clean_names() %>%
  mutate_at(vars("media_sm":"j4"), funs(str_replace(., ",", ".")))


url1 <- "https://www.rincondelmanager.com/smgr/valoracion.php"

pgsession <- session(url1)
pgform <- html_form(pgsession)[[5]]
filled_form <- html_form_set(pgform,
                             "de_jor"  =  "5",
                             "a_jor"   =  "8",
                             "cancha"  =  "2"
)

df1 <- submit_form(session = pgsession, form = filled_form, POST = url1)

df_rs2 <- df1 %>% html_element("table#tTodos") %>%
  html_table() %>%
  clean_names() %>%
  mutate_at(vars("media_sm":"j8"), funs(str_replace(., ",", ".")))

df_rs <- df_rs1 %>% left_join(df_rs2, by =c("jugador", "jug")) %>%
  select(jugador, jug, eq=eq.x, j1:j4,j5:j8) %>%
  mutate(across(c(j1:j8), as.numeric) %>%
 replace(is.na(.), 0)) %>% group_by(jug) %>%
  mutate(total = rowSums(across(j1:j8))) %>%
  arrange(desc(total))

pj <- "https://www.rincondelmanager.com/smgr/avanzadas.php" %>%
  read_html()

pjdf <- pj %>% html_element("table#tTodos") %>%
  html_table() %>%
  clean_names() %>%
  select(jug, jugador, pj, uso) %>%
  left_join(df_rs) %>%
  mutate(uso= as.numeric(str_remove(uso, "%")),
  media = total/pj)



# Union de las tablas SuperManager y GM -----------------------------------



gmtbl_df <- gmtbldf  %>% fuzzyjoin::stringdist_left_join(pjdf, by = c("player" = "jugador")) %>%
  filter(jugador != "NA" & pj >= 7) %>%
  mutate(pertotal = per*pj) %>%
  select(jug, abb = eq, pj,  uso, total, media, pertotal, per) %>%
  left_join(teams, by = c("abb" = "abbRm")) %>%
  arrange(desc(total))


# tablas para separar los puntos  -----------------------------------------


df1 <- gmtbl_df %>% arrange(desc(media)) %>% slice(1:30)
df2 <- gmtbl_df %>% arrange(desc(media)) %>% slice(31:187)
df3 <- gmtbl_df %>% arrange(desc(per)) %>% slice(1:30)


# Gráfico -----------------------------------------------------------------



df <- gmtbl_df %>%
  ggplot(aes(x = media, y = per)) +
  geom_point(data = df2, size = df2$uso/5, alpha = .233, color = '#626060', fill = '#85714D', shape = 21)+
  geom_point(data = df1, size = df1$uso/5, alpha = .333, color = '#626060', fill = df1$color, shape = 21) +
  geom_point(data = df1 %>% filter(jug == "Á. Delgado"), alpha = .433, size = 5, color = '#626060', fill = "#000000", shape = 21)+
  geom_point(data = df3 %>% filter(jug == "V. Arteaga"), alpha = .433, size = 4.2, color = '#626060', fill = "#0165BA", shape = 21)+
  annotate(geom = 'text', x = 6.4, y = 25.6 , size = 1.618, label = "V. Arteaga", family = "Consolas", color = "#000000") +
  geom_curve(aes(x =7.06, y = 25.48, xend = 7.5, yend =24.85), size = .1, curvature = -0.2, alpha = .1, color = "darkgrey", arrow = arrow(length = unit(0.1,"cm")))+
  geom_text_repel(data = df1, aes(label = jug), max.overlaps = 12, segment.size = .15, family = "Consolas", size = 1.5) +
  scale_fill_identity()+
  scale_x_continuous(limits = c(0, 2 + max(gmtbl_df$media)), breaks = seq(0, 2 +max(gmtbl_df$media), 5)) +
  scale_y_continuous(limits = c(0, 2+ max(gmtbl_df$per)), breaks = seq(0, 2 + max(gmtbl_df$per), 5)) +
  theme_ivo() +
  theme(plot.title = element_markdown(face = "bold", size = 11 ),
        plot.subtitle = element_text(size = 9 ),
        plot.caption = element_markdown(size = 5, hjust = 0),
        axis.title.y = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 8, face = "bold"),
        plot.margin = margin(.25, .25, .10, .25, "cm")) +
  labs(x = "\nValoración",
       y = "PER\n",
       title = glue::glue("TOP 30 Valoración media Vs PER<sup style='font-weight:bold;font-size:11px'>1</sup>  (Hasta J{jor})"),
        subtitle = str_to_title(paste0("Con más de 7 partidos jugados | Pace Adjusted 2021-22 A ", format(Sys.Date(), "%B, %Y"))),
        caption = "<sup style='font-weight:bold;font-size:5px'>1</sup>(**PER**) *Player efficiency rating por* **<i>John Hollinger<i>**<br>
        El tamaño del circulo corresponde al **usage** del jugador
       <br>**Datos**: *@RealGM @elrincondelsm* **Gráfico**: <i>Ivo Villanueva @elcheff<i>")
ggsave("gmtbl_df.png", df, width =6 * 1.17, height = 6, dpi ="retina")



# Iván Villanueva noviembre 2021 ------------------------------------------


