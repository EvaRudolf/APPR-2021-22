# 4. faza: Napredna analiza podatkov


source("lib/libraries.r")


################################################################################
# RAZVRŠČANJE V SKUPINE - CLUSTERING

# Razvrščanje držav v skupine glede na to, kakšne rezultate dosegajo,
# upoštevajoč letnik rojstva, disciplino, smuči in dosežene točke.


# Tabela s podatki o smučarjih:
# | ime | YB | država | smuči | točke DH | točke SG | točke GS | točke SL | 
smuci <- levels(REZULTATI.VREME$Ski)[-c(9)]
DH.analiza <- REZULTATI.VREME %>% filter(Disciplina == "DH") %>% group_by(Name) %>%
  summarise(tocke.DH = sum(tocke_30))
SG.analiza <- REZULTATI.VREME %>% filter(Disciplina == "SG") %>% group_by(Name) %>%
  summarise(tocke.SG = sum(tocke_30))
GS.analiza <- REZULTATI.VREME %>% filter(Disciplina == "GS") %>% group_by(Name) %>%
  summarise(tocke.GS = sum(tocke_30))
SL.analiza <- REZULTATI.VREME %>% filter(Disciplina == "SL") %>% group_by(Name) %>%
  summarise(tocke.SL = sum(tocke_30))
smucarji <- left_join(REZULTATI.VREME, DH.analiza, by = "Name")
smucarji <- left_join(smucarji, SG.analiza, by = "Name")
smucarji <- left_join(smucarji, GS.analiza, by = "Name")
smucarji <- left_join(smucarji, SL.analiza, by = "Name")
smucarji <- smucarji %>% 
  dplyr::select(Name, YB, NSA, Ski, tocke.DH, tocke.SG, tocke.GS, tocke.SL) %>%
  filter(Ski %in% smuci)
smucarji$tocke.DH[is.na(smucarji$tocke.DH)] <- 0
smucarji$tocke.SG[is.na(smucarji$tocke.SG)] <- 0
smucarji$tocke.GS[is.na(smucarji$tocke.GS)] <- 0
smucarji$tocke.SL[is.na(smucarji$tocke.SL)] <- 0
smucarji <- unique(smucarji)


# spemenimo imenske spremenljivke v numerične
one_hot = function(pod, spr) {
  domena = pod %>%
    dplyr::select({{spr}}) %>%
    unlist() %>%
    unique()
  for (v in domena) {
    pod = pod %>%
      mutate(
        "{{spr}}.{v}" := ifelse({{spr}} == v, 1, 0)
      )
  }
  pod %>% dplyr::select(-{{spr}})
}

podatki.za.skupine <- smucarji
skupine.po.drzavah <- one_hot(podatki.za.skupine, Ski)
skupine.po.drzavah <- skupine.po.drzavah %>% dplyr::select(-Name) %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "SLO", replacement = "SVN"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "GER", replacement = "DEU"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "CRO", replacement = "HRV"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "SUI", replacement = "CHE"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "NED", replacement = "NLD"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "BUL", replacement = "BGR"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "GRE", replacement = "GRC"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "MON", replacement = "MCO"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "SUI", replacement = "CZE"))
skupine.po.drzavah <- skupine.po.drzavah %>% 
  mutate(NSA = str_replace_all(skupine.po.drzavah$NSA, pattern = "RFS", replacement = "RUS"))
skupine.po.drzavah


set.seed(123)

# Clustering
skupine <- skupine.po.drzavah[,-2] %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()
print(skupine)


dodane.skupine <- skupine.po.drzavah %>% mutate(skupina = as.factor(skupine))
dodane.skupine <- dodane.skupine %>% group_by(NSA) %>% summarise(skup = sum(as.numeric(skupina)))

tabela.skupine1 <- dodane.skupine %>% filter(skup <= 5) %>% mutate(skup = 1)
tabela.skupine2 <- dodane.skupine %>% filter(skup >=5 & skup < 20) %>% mutate(skup = 2)
tabela.skupine3 <- dodane.skupine %>% filter(skup >=20 & skup < 40) %>% mutate(skup = 3) 
tabela.skupine4 <- dodane.skupine %>% filter(skup >=40) %>% mutate(skup = 4)

tabela.skupine <- rbind(tabela.skupine1, tabela.skupine2, tabela.skupine3, tabela.skupine4)


zemljevid.za.skupine <-
  uvozi.zemljevid(
    "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
    "ne_50m_admin_0_countries",
    mapa = "zemljevidi",
    pot.zemljevida = "",
    encoding = "UTF-8"
  ) %>%
  fortify() %>% filter(long < 170 & long > -170 & lat > -55 & lat < 85)


zemljevid.po.skupinah <- ggplot() +
  aes(x = long, y = lat, group = group, fill = skup) +
  geom_polygon(data = tabela.skupine %>% right_join(zemljevid.za.skupine, by = c("NSA" = "ADM0_A3"))) +
  xlab("") +
  ylab("") +
  ggtitle("Države s podobno strukturo smučarjev 2020/21")
zemljevid.po.skupinah

################################################################################
# NAPOVEDNI MODEL
# Napovemo uvrstitev smučarja na naslednji tekmi (spremenljivke so država, smuči
# in disciplina)
# če se da, naredi še napovedovanje za to, katere države bi bile v prihodnje lahko najboljše.

