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
  fortify() %>% filter(long < 170 & long > -170 & lat > 0 & lat < 85)


zemljevid.po.skupinah <- ggplot() +
  aes(x = long, y = lat, group = group, fill = skup) +
  geom_polygon(data = tabela.skupine %>% right_join(zemljevid.za.skupine, by = c("NSA" = "ADM0_A3"))) +
  xlab("") +
  ylab("") +
  ggtitle("Države s podobno strukturo smučarjev 2020/21") +
  coord_fixed(ratio = 2) +
  guides(fill=guide_legend(title="Skupina")) + 
  theme(legend.title = element_text(color = "black", size = 11),
        legend.background = element_rect(colour ="#006699", fill = "white"), 
        plot.title = element_text(color = "#006699", hjust = 0.5, size = 15))
zemljevid.po.skupinah



# Izbira optimalnega števila skupin:
dendrogram <- skupine.po.drzavah[,-2] %>%
  dist() %>%
  hclust()

plot(
  dendrogram,
  labels = skupine.po.drzavah$oznaka,
  ylab = "višina",
  main = NULL
)

tibble(
  k = 14:1,
  visina = dendrogram$height
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = k, y = visina),
    color = "red"
  ) +
  geom_point(
    mapping = aes(x = k, y = visina),
    color = "red"
  ) +
  scale_x_continuous(
    breaks = 14:1
  ) +
  labs(
    x = "število skupin (k)",
    y = "višina združevanja"
  ) +
  theme_classic()

hc.kolena <- function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k <- function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# izračunamo tabelo s koleni za dendrogram
r <- hc.kolena(dendrogram)

# narišemo diagram višin združevanja
diagram.kolena <- function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.kolena(r) # -> izbrala sem razvrstitev v 4 skupine

################################################################################
# NAPOVEDNI MODEL
# Napovemo uvrstitev smučarja na naslednji tekmi (spremenljivke so država, smuči
# in disciplina)
# če se da, naredi še napovedovanje za to, katere države bi bile v prihodnje lahko najboljše.
smuci <- levels(REZULTATI.VREME$Ski)[-c(9)]

podatki <- REZULTATI.VREME %>% 
  dplyr::select(Rank, YB, Ski, Disciplina) %>% 
  filter(Ski %in% smuci) %>%
  one_hot(Ski) %>% one_hot(Disciplina)

# normaliziramo YB
podatki$YB <- (podatki$Rank-mean(podatki$Rank))/sd(podatki$Rank)

# namesto uvrstitve 1-30 jih dam po skupinah:
# 1: 1.-5. mesto
# 2: 6.-10. mesto
# 3: 11.-15. mesto
# 4: 16.-20. mesto
# 5: 20.-30. mesto

# for (i in 1:1052){
#   if (i < 6){ucni.podatki$Rank[i] = 1}
#   else if (i > 5 & i < 11){ucni.podatki$Rank[i] = 2}
#   else if (i > 10 & i < 16){ucni.podatki$Rank[i] = 3}
#   else if (i > 15 & i < 21){ucni.podatki$Rank[i] = 4}
#   else {ucni.podatki$Rank[i] = 5}
# }


# linearna regresija, da napovemo, katere smuci je treba imeti, za uvrstitev med top 5:
set.seed(123)

lin.reg.smuci = lm(
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
    Ski.Salomon + Ski.Stoeckli + Ski.Blizzard + Ski.Dynastar + Ski.Voelkl,
  data = podatki)
print(lin.reg.smuci)

# disciplina:

lin.reg.disc = lm(
  Rank ~ Disciplina.DH + Disciplina.SG + Disciplina.GS + Disciplina.SL,
  data = podatki)
print(lin.reg.disc)

# leto rojstva
lin.reg.leto = lm(
  Rank ~ YB,
  data = podatki
)
print(lin.reg.leto)

# prečno preverjanje
  napaka.cv <- function(podatki, formula, k){
    # ponovljivost
    set.seed(123)
    # za k-kratno prečno preverjanje najprej podatke razdelimo na k enako velikih delov
    n <- nrow(podatki)
    # najprej naključno premešamo primere
    r <- sample(1:n)
    # razrežemo na k intervalov
    razrez <- cut(seq_along(r), k, labels = FALSE)
    # Razbijemo vektor na k seznamov na osnovi razreza intervalov
    razbitje = split(r, razrez)
    # zdaj imamo dane indekse za vsakega od k-tih delov
    pp.napovedi = rep(0, nrow(podatki))
    # prečno preverjanje
    for (i in 1:length(razbitje)){
      train.data = podatki[ -razbitje[[i]], ]  # učni podatki
      test.data = podatki[ razbitje[[i]], ]# testni podatki
      # naučimo model
      model = lm(data = train.data, formula = formula)
      # napovemo za testne podatke
      napovedi = predict(model, newdata = test.data)
      pp.napovedi[ razbitje[[i]] ] = napovedi
    }
    # izračunamo MSE
    napaka = mean((pp.napovedi - podatki$Rank) ^ 2)
    return(napaka)
  }

formula.smuci <- Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol +
  Ski.Salomon + Ski.Stoeckli + Ski.Blizzard + Ski.Dynastar + Ski.Voelkl
formula.disciplina <- Rank ~ Disciplina.DH + Disciplina.SG + Disciplina.GS + Disciplina.SL
formula.YB <- Rank ~ YB

n.smuci <- napaka.cv(podatki, formula.smuci, 10)
n.disciplina <- napaka.cv(podatki, formula.disciplina, 10)
n.YB <- napaka.cv(podatki, formula.YB, 10)
print(c(n.smuci, n.disciplina, n.YB))


new = podatki[1,3:13]
new[1,] = c(rep(0,6), 1, rep(0,4))# določimo, na katerm mestu je 1 -> tiste smuči izberemo
# napovemo mesto
napoved <- predict(lin.reg.smuci, newdata = new)
