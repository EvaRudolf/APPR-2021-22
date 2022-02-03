# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

################################################################################
# RAZVRŠČANJE V SKUPINE - 
# Razvrščanje držav v skupine glede na to, kakšne rezultate dosegajo

### Najprej k-means:

# Tabela s podatki o državah:
# | država | točke DH | točke SG | točke GS | točke SL | povprečen YB |

# Najprej spremenimo kratice držav, da lahko združimo z zemljevidom
prave.kratice <- REZULTATI.VREME %>% 
  mutate(NSA = str_replace_all(REZULTATI.VREME$NSA, pattern = "SLO", replacement = "SVN"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "GER", replacement = "DEU"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "CRO", replacement = "HRV"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "SUI", replacement = "CHE"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "NED", replacement = "NLD"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "BUL", replacement = "BGR"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "GRE", replacement = "GRC"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "MON", replacement = "MCO"))
prave.kratice <- prave.kratice %>% 
  mutate(NSA = str_replace_all(prave.kratice$NSA, pattern = "RSF", replacement = "RUS"))
prave.kratice

DH.analiza <- prave.kratice %>% filter(Disciplina == "DH") %>% group_by(NSA) %>%
  summarise(tocke.DH = sum(tocke_30))
SG.analiza <- prave.kratice %>% filter(Disciplina == "SG") %>% group_by(NSA) %>%
  summarise(tocke.SG = sum(tocke_30))
GS.analiza <- prave.kratice %>% filter(Disciplina == "GS") %>% group_by(NSA) %>%
  summarise(tocke.GS = sum(tocke_30))
SL.analiza <- prave.kratice %>% filter(Disciplina == "SL") %>% group_by(NSA) %>%
  summarise(tocke.SL = sum(tocke_30))
YB.analiza <- prave.kratice %>% group_by(NSA) %>% summarise(YB.povprecen = mean(YB))

drzave <- left_join(prave.kratice, DH.analiza, by = "NSA")
drzave <- left_join(drzave, SG.analiza, by = "NSA")
drzave <- left_join(drzave, GS.analiza, by = "NSA")
drzave <- left_join(drzave, SL.analiza, by = "NSA")
drzave <- left_join(drzave, YB.analiza, by = "NSA")

drzave <- drzave %>% 
  dplyr::select(YB.povprecen, NSA, tocke.DH, tocke.SG, tocke.GS, tocke.SL)

drzave$tocke.DH[is.na(drzave$tocke.DH)] <- 0
drzave$tocke.SG[is.na(drzave$tocke.SG)] <- 0
drzave$tocke.GS[is.na(drzave$tocke.GS)] <- 0
drzave$tocke.SL[is.na(drzave$tocke.SL)] <- 0
drzave <- unique(drzave)


# Clustering s k-means
set.seed(123)

skupine <- drzave[,-2] %>%
  kmeans(centers = 5) %>%
  getElement("cluster") %>%
  as.ordered()
print(skupine)

tabela.skupine <- drzave %>% 
  transform(NSA = as.factor(NSA)) %>% 
  mutate(Skupine = as.numeric(skupine))

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
  aes(x = long, y = lat, group = group, fill = Skupine) +
  geom_polygon(data=tabela.skupine %>% right_join(zemljevid.za.skupine, 
                                                  by = c("NSA" = "ADM0_A3"))) +
  xlab("") +
  ylab("") +
  ggtitle("Države s podobno strukturo smučarjev 2020/21") +
  coord_fixed(ratio = 2) +
  guides(fill=guide_legend(title="Skupina")) + 
  theme(legend.title = element_text(color = "black", size = 11),
        legend.background = element_rect(colour ="#006699", fill = "white"), 
        plot.title = element_text(color = "#006699", hjust = 0.5, size = 15))
zemljevid.po.skupinah


### Še hierarhično razvrščanje:
dendrogram <- drzave[,-2] %>%
  dist() %>%
  hclust()

plot(dendrogram,
     labels = drzave$NSA,
     ylab = "višina",
     main = NULL)

hc.kolena <- function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k, pri katerih opazujemo koleno
hc.kolena.k <- function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# tabela s koleni za dendrogram
r <- hc.kolena(dendrogram)

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
      color = "#006699", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.kolena(r)


################################################################################
# NAPOVEDNI MODEL
# Napovemo uvrstitev, ki jo pričakujemo od smučarja glede na to, katere smuči uporablja, 
# v kateri disciplini tekmuje in glede na snežno podlago

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


smuci <- levels(REZULTATI.VREME$Ski)[-c(9)]

# Podatki, na katerih bom delala napovedi:
podatki <- REZULTATI.VREME %>% 
  dplyr::select(Rank, Ski, Disciplina, sneg) %>% 
  filter(Ski %in% smuci) %>%
  one_hot(Ski) %>% one_hot(Disciplina) %>% one_hot(sneg)


set.seed(123)
# linearna regresija, da napovemo uvrstitve tekmovalca glede na:
# smuči (1. MODEL)
lin.reg.smuci = lm(
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
    Ski.Salomon + Ski.Stoeckli + Ski.Voelkl + Ski.Dynastar + Ski.Blizzard,
  data = podatki)
print(lin.reg.smuci)

# smuči in disciplino (2.MODEL)
lin.reg.smuci.disc = lm(
  Rank ~  Disciplina.DH + Disciplina.SG + Disciplina.GS + Disciplina.SL +
    Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol +
    Ski.Salomon + Ski.Stoeckli  + Ski.Dynastar + Ski.Voelkl+ Ski.Blizzard,
  data = podatki)
print(lin.reg.smuci.disc)

# smuči in snežno podlago (3. MODEL)
lin.reg.smuci.sneg = lm(
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
    Ski.Salomon + Ski.Stoeckli + Ski.Blizzard + Ski.Dynastar + Ski.Voelkl + sneg.HARD + 
    `sneg.SPRING CONDITIONS` + sneg.COMPACT + sneg.SOFT + sneg.PACKED,
  data = podatki)
print(lin.reg.smuci.sneg)

# Še model, ki vključuje tako smuči kot tudi disciplino in snežno podlago (4. MODEL):
lin.reg.skupaj = lm(
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
    Ski.Salomon + Ski.Stoeckli + Ski.Blizzard + Ski.Dynastar + Ski.Voelkl +
    Disciplina.DH + Disciplina.SG + Disciplina.GS + Disciplina.SL + sneg.HARD + 
    `sneg.SPRING CONDITIONS` + sneg.COMPACT + sneg.SOFT + sneg.PACKED,
  data = podatki)
print(lin.reg.skupaj)

# prečno preverjanje
napaka.cv <- function(podatki, formula, k) {
  set.seed(123)
  n <- nrow(podatki)
  # naključno premešamo primere
  r <- sample(1:n)
  # razrez na k intervalov
  razrez <- cut(seq_along(r), k, labels = FALSE)
  # razbijemo vektor na k seznamov na osnovi razreza intervalov
  razbitje = split(r, razrez)
  # zdaj imamo dane indekse za vsakega od k-tih delov
  pp.napovedi = rep(0, nrow(podatki))
  # prečno preverjanje
  for (i in 1:length(razbitje)) {
    train.data = podatki[-razbitje[[i]],]  # učni podatki
    test.data = podatki[razbitje[[i]],]    # testni podatki
    # naučimo model
    model = lm(data = train.data, formula = formula)
    # napovemo za testne podatke
    napovedi = predict(model, newdata = test.data)
    pp.napovedi[razbitje[[i]]] = napovedi
  }
  # izračunamo MSE
  napaka = mean((pp.napovedi - podatki$Rank) ^ 2)
  return(napaka)
}

# formule za napovedi
formula.smuci <-
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
  Ski.Salomon + Ski.Stoeckli + Ski.Voelkl + Ski.Dynastar + Ski.Blizzard
formula.smuci.disc <- 
  Rank ~  Disciplina.DH + Disciplina.SG + Disciplina.GS + Disciplina.SL +
  Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol +
  Ski.Salomon + Ski.Stoeckli  + Ski.Dynastar + Ski.Voelkl+ Ski.Blizzard
formula.smuci.sneg <- 
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
  Ski.Salomon + Ski.Stoeckli + Ski.Blizzard + Ski.Dynastar + Ski.Voelkl + sneg.HARD + 
  `sneg.SPRING CONDITIONS` + sneg.COMPACT + sneg.SOFT + sneg.PACKED
formula.skupaj <- 
  Rank ~ Ski.Atomic + Ski.Fischer + Ski.Head + Ski.Kaestle + Ski.Nordica + Ski.Rossignol + 
  Ski.Salomon + Ski.Stoeckli + Ski.Blizzard + Ski.Dynastar + Ski.Voelkl +
  Disciplina.DH + Disciplina.SG + Disciplina.GS + Disciplina.SL + sneg.HARD + 
  `sneg.SPRING CONDITIONS` + sneg.COMPACT + sneg.SOFT + sneg.PACKED

napaka.smuci <- napaka.cv(podatki, formula.smuci, 10)
napaka.smuci.disc <- napaka.cv(podatki, formula.smuci.disc, 10)
napaka.smuci.sneg <- napaka.cv(podatki, formula.smuci.sneg, 10)
napaka.skupaj <- napaka.cv(podatki, formula.skupaj, 10)
print(c(napaka.smuci, napaka.smuci.disc, napaka.smuci.sneg, napaka.skupaj))
# --> najboljši model je tisti s smučmi in disciplino (2. model)

# Prikaz za 1. model:
uvrstitev <- c(1:length(smuci)) * 0
napovedi.za.smuci <- tibble(smuci, uvrstitev)

for (i in 1:length(smuci)){
  vzorec = podatki[1,2:12]
  vzorec[1,] = c(rep(0,i-1), 1, rep(0, length(smuci)-i))
  napovedi.za.smuci[i,2] <- predict(lin.reg.smuci, newdata = vzorec)
}
napovedi.za.smuci[2]

barve <- c("#FF0000", "#FF6633","#00CCCC", "#FFFF00", "#FFF7A9", "#009966", 
           "#330000", "#FF3300", "#000066", "#990000", "#CCCC00")

graf.napovedi <-
  ggplot(napovedi.za.smuci) +
  aes(x = smuci, y = uvrstitev) +
  geom_bar(stat = "identity", aes(fill = smuci)) + 
  scale_fill_manual(values = barve) +
  xlab("Smuči") +
  ylab("Uvrstitev") +
  ggtitle("Napovedane uvrstitve glede na smuči v sezoni 2020/21") +
  theme(legend.position = "none",
        plot.title = element_text(color = "#006699", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 25))
graf.napovedi

# Primeri za v poročilo, delam jih na podlagi 2. modela, ker ima najmanjšo napako:
vzorec.2 = podatki[1, 2:16]
# Opazujmo smuči znamke Atomic, za katere smo ugotovili, da so najbolj vsestranske, 
# torej primerne z tekmovanje v vseh disciplinah:
vzorec.2[1,] = c(rep(0,3), 1, rep(0, 7), 1, rep(0, 3))
pred.atomic.dh <- predict(lin.reg.smuci.disc, newdata = vzorec.2)
vzorec.2[1,] = c(rep(0,3), 1, rep(0, 8), 1, rep(0, 2))
pred.atomic.sg <- predict(lin.reg.smuci.disc, newdata = vzorec.2)
vzorec.2[1,] = c(rep(0,3), 1, rep(0, 9), 1, 0)
pred.atomic.gs <- predict(lin.reg.smuci.disc, newdata = vzorec.2)
vzorec.2[1,] = c(rep(0,3), 1, rep(0, 10), 1)
pred.atomic.sl <- predict(lin.reg.smuci.disc, newdata = vzorec.2)

Uvrstitev <- c(pred.atomic.dh, pred.atomic.sg, pred.atomic.gs, pred.atomic.sl)
Disciplina <- c("Smuk", "Superveleslalom", "Veleslalom", "Slalom")
# Tabela za v poročilo:
atomic <- t(tibble(Disciplina, Uvrstitev))

