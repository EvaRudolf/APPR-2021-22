# 2. faza: Uvoz podatkov

source("lib/libraries.r")
sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")

podatki <- "podatki/"
rezultati <- list.files(podatki, pattern = ".csv")

################################################################################
# VREME
################################################################################
# za hitre discipline (prebere eno vrstico vremena - en tek)
vreme_v_tabelo <- function(path, regex) {
  tabela <- readLines(path)
  tf <- str_detect(tabela, regex)
  kje_je_true <- match(TRUE, tf)
  preskoci <- kje_je_true - 1
  return(read.csv2(
    path,
    skip = preskoci,
    nrows = 1,
    header = TRUE,
    encoding = "UTF-8"
  ))
}

preberi_vreme <- function(pod) {
  tabela <- list()
  for (i in 1:length(rezultati)) {
    tabela[[i]] <-
      (vreme_v_tabelo(paste(podatki, rezultati[i], sep = ""), "Conditions"))
  }
  return(tabela)
}

# za tehnične discipline (prebere dve vrstici vremena - dva teka)
vreme_v_tabelo2 <- function(path, regex) {
  tabela <- readLines(path)
  tf <- str_detect(tabela, regex)
  kje_je_true <- match(TRUE, tf)
  preskoci <- kje_je_true - 1
  return(read.csv2(
    path,
    skip = preskoci,
    nrows = 2,
    header = TRUE,
    encoding = "UTF-8"
  ))
}

preberi_vreme2 <- function(pod) {
  tabela <- list()
  for (i in 1:length(rezultati)) {
    tabela[[i]] <-
      (vreme_v_tabelo2(paste(podatki, rezultati[i], sep = ""), "Conditions"))
  }
  return(tabela)
}


# povprečna temperatura, ki jo dobimo iz startne in ciljne
povprecna_temp <- function(start, finish) {
  povprecna <- (as.numeric(start) + as.numeric(finish)) / 2
}

# funkcija, ki naredi tabele za vreme hitrih disciplin
vreme_hitre <- function(vr) {
  vreme <- data.frame(
    temperatura = povprecna_temp(
      str_replace_all(vr$X.2, "[^0-9-]", ""),
      str_replace_all(vr$X.4, "[^0-9-]", "")
    ),
    vreme = as.factor(vr$Conditions.on.course.),
    sneg = as.factor(str_remove(vr$X, "Snow: "))
  )
}

# funkcija, ki naredi tabele za vreme tehničnih disciplin
vreme_tehnicne <- function(vr) {
  T_1 = povprecna_temp(
    str_replace_all(vr$X.3[1], "[^0-9-]", ""),
    str_replace_all(vr$X.5[1], "[^0-9-]", "")
  )
  T_2 = povprecna_temp(
    str_replace_all(vr$X.3[2], "[^0-9-]", ""),
    str_replace_all(vr$X.5[2], "[^0-9-]", "")
  )
  vreme <- data.frame(
    temperatura = povprecna_temp(T_1, T_2),
    vreme1 = as.factor(vr$X[1]),
    vreme2 = as.factor(vr$X[1]),
    sneg1 = as.factor(str_remove(vr$X.1[1], "Snow:")),
    sneg2 = as.factor(str_remove(vr$X.1[2], "Snow:"))
  )
  return(vreme)
}

# seznam tabel za vreme
seznam_vremena1 <- preberi_vreme(pod = rezultati)
seznam_vremena2 <- preberi_vreme2(pod = rezultati)

# Vreme na smukaških tekmah
DH.bormio.vreme <- vreme_hitre(seznam_vremena1[[1]])
DH.cortina.vreme <- vreme_hitre(seznam_vremena1[[2]])
DH.garmisch.vreme <- vreme_hitre(seznam_vremena1[[3]])
DH.saalbach.vreme <- vreme_hitre(seznam_vremena1[[4]])
DH.valdisere.vreme <- vreme_hitre(seznam_vremena1[[5]])
DH.valgardenagroeden.vreme <- vreme_hitre(seznam_vremena1[[6]])
DH1.kitzbuehl.vreme <- vreme_hitre(seznam_vremena1[[7]])
DH2.kitzbuehl.vreme <- vreme_hitre(seznam_vremena1[[8]])

# Vreme na superveleslalomskih tekmah
SG.bormio.vreme <- vreme_hitre(seznam_vremena1[[20]])
SG.cortina.vreme <- vreme_hitre(seznam_vremena1[[21]])
SG.garmisch.vreme <- vreme_hitre(seznam_vremena1[[22]])
SG.kitzbuehl.vreme <- vreme_hitre(seznam_vremena1[[23]])
SG.saalbach.vreme <- vreme_hitre(seznam_vremena1[[24]])
SG.valdisere.vreme <- vreme_hitre(seznam_vremena1[[25]])
SG.valgardenagroeden.vreme <- vreme_hitre(seznam_vremena1[[26]])

# Vreme na veleslalomskih tekmah
GS.altabadia.vreme <- vreme_tehnicne(seznam_vremena2[[9]])
GS.cortina.vreme <- vreme_tehnicne(seznam_vremena2[[10]])
GS.kranjskagora.vreme <- vreme_tehnicne(seznam_vremena2[[11]])
GS.lenzerheide.vreme <- vreme_tehnicne(seznam_vremena2[[12]])
GS.soelden.vreme <- vreme_tehnicne(seznam_vremena2[[13]])
GS1.adelboden.vreme <- vreme_tehnicne(seznam_vremena2[[14]])
GS1.bansko.vreme <- vreme_tehnicne(seznam_vremena2[[15]])
GS1.santacaterina.vreme <- vreme_tehnicne(seznam_vremena2[[16]])
GS2.adelboden.vreme <- vreme_tehnicne(seznam_vremena2[[17]])
GS2.bansko.vreme <- vreme_tehnicne(seznam_vremena2[[18]])
GS2.santacaterina.vreme <- vreme_tehnicne(seznam_vremena2[[19]])

# Vreme na slalomskih tekmah
SL.adelboden.vreme <- vreme_tehnicne(seznam_vremena2[[27]])
SL.altabadia.vreme <- vreme_tehnicne(seznam_vremena2[[28]])
SL.cortina.vreme <- vreme_tehnicne(seznam_vremena2[[29]])
SL.kranjskagora.vreme <- vreme_tehnicne(seznam_vremena2[[30]])
SL.lenzerheide.vreme <- vreme_tehnicne(seznam_vremena2[[31]])
SL.madonnadicampiglio.vreme <- vreme_tehnicne(seznam_vremena2[[32]])
SL.schladming.vreme <- vreme_tehnicne(seznam_vremena2[[33]])
SL.zagreb.vreme <- vreme_tehnicne(seznam_vremena2[[34]])
SL1.chamonix.vreme <- vreme_tehnicne(seznam_vremena2[[35]])
SL1.flachau.vreme <- vreme_tehnicne(seznam_vremena2[[36]])
SL2.chamonix.vreme <- vreme_tehnicne(seznam_vremena2[[37]])
SL2.flachau.vreme <- vreme_tehnicne(seznam_vremena2[[38]])



################################################################################
# REZULTATI TEKEM
################################################################################

rezultati.DH <- list.files(podatki, pattern = "DH.*.csv")
rezultati.SG <- list.files(podatki, pattern = "SG.*.csv")
rezultati.GS <- list.files(podatki, pattern = "GS.*.csv")
rezultati.SL <- list.files(podatki, pattern = "SL.*.csv")

rezultati_v_tabelo <- function(path, regex) {
  tabela <- readLines(path)
  tf <- str_detect(tabela, regex)
  kje_je_true <- match(TRUE, tf)
  preskoci <- kje_je_true - 1
  return(read.csv2(
    path,
    skip = preskoci,
    nrows = 30,
    encoding = "UTF-8"
  ))
}


preberi_vse_tabele <- function(pod) {
  tabela <- list()
  for (i in 1:length(rezultati)) {
    tabela[[i]] <-
      (rezultati_v_tabelo(paste(podatki, rezultati[i], sep = ""), "Rank"))
  }
  return(tabela)
}

seznam_rezultatov <- preberi_vse_tabele(pod = rezultati)



# Rezultati smukaških tekem z dodanim vremenom
DH.bormio <- left_join(seznam_rezultatov[[1]] %>% mutate(venue = "Bormio"),
                       DH.bormio.vreme, by = character())
DH.cortina <- left_join(seznam_rezultatov[[2]] %>% mutate(venue = "Cortina") %>% 
                          dplyr::select(-c(10)), DH.cortina.vreme, by = character())
DH.garmisch <- left_join(seznam_rezultatov[[3]] %>% mutate(venue = "Garmisch") %>% 
                           dplyr::select(-c(10)), DH.garmisch.vreme, by = character())
DH.saalbach <- left_join(seznam_rezultatov[[4]] %>% mutate(venue = "Saalbach") %>% 
                           dplyr::select(-c(10)), DH.saalbach.vreme, by = character())
DH.valdisere <- left_join(seznam_rezultatov[[5]] %>% mutate(venue = "Val d'Isere"),
                          DH.valdisere.vreme, by = character())
DH.valgardenagroeden <- left_join(seznam_rezultatov[[6]] %>% mutate(venue = "Val Gardena"),
                                  DH.valgardenagroeden.vreme, by = character())
DH1.kitzbuehl <- left_join(seznam_rezultatov[[7]][-c(29,30),] %>% mutate(venue = "Kitzbuehl1"),
                           DH1.kitzbuehl.vreme, by = character())
DH2.kitzbuehl <- left_join(seznam_rezultatov[[8]] %>% mutate(venue = "Kitzbuehl2"),
                           DH2.kitzbuehl.vreme, by = character())

smuk <-
  rbind(
    DH.bormio,
    DH.cortina,
    DH.garmisch,
    DH.saalbach,
    DH.valdisere,
    DH.valgardenagroeden,
    DH1.kitzbuehl,
    DH2.kitzbuehl
  ) %>%
  mutate(disc = "DH") %>% dplyr::select(-c(9)) %>% 
  transform(Name = as.factor(Name)) %>%
  transform(Rank = as.numeric(Rank)) %>%
  transform(Bib = as.numeric(Bib)) %>% 
  transform(FIS.Code = as.numeric(FIS.Code)) %>%
  transform(YB = as.numeric(YB)) %>%
  transform(NSA.Code = as.factor(NSA.Code)) %>%
  transform(Ski = as.factor(Ski)) %>% 
  transform(disc = as.factor(disc)) %>%
  rename(NSA = NSA.Code)



# Rezultati superveleslalomskih tekem
SG.bormio <- left_join(seznam_rezultatov[[20]] %>% mutate(venue = "Bormio"),
                       SG.bormio.vreme, by = character())
SG.cortina <- left_join(seznam_rezultatov[[21]] %>% mutate(venue = "Cortina") %>% 
                          dplyr::select(-c(10)), SG.cortina.vreme, by = character())
SG.garmisch <- left_join(seznam_rezultatov[[22]] %>% mutate(venue = "Garmisch") %>% 
                           dplyr::select(-c(10)),SG.garmisch.vreme, by = character())
SG.kitzbuehl <- left_join(seznam_rezultatov[[23]] %>% mutate(venue = "Kitzbuehl"),
                          SG.kitzbuehl.vreme, by = character())
SG.saalbach <- left_join(seznam_rezultatov[[24]] %>% mutate(venue = "Saalbach") %>% 
                           dplyr::select(-c(10)), SG.saalbach.vreme, by = character())
SG.valdisere <- left_join(seznam_rezultatov[[25]] %>% mutate(venue = "Val d'Isere"),
                          SG.valdisere.vreme, by = character())
SG.valgardenagroeden <- left_join(seznam_rezultatov[[26]] %>% mutate(venue = "Val Gardena"),
                                  SG.valgardenagroeden.vreme, by = character())

superG <- rbind(SG.bormio, 
                SG.cortina, 
                SG.garmisch, 
                SG.kitzbuehl, 
                SG.saalbach,
                SG.valdisere, 
                SG.valgardenagroeden) %>% 
  mutate(disc = "SG") %>% dplyr::select(-c(9)) %>%
  transform(Name = as.factor(Name)) %>%
  transform(Rank = as.numeric(Rank)) %>%
  transform(Bib = as.numeric(Bib)) %>% 
  transform(FIS.Code = as.numeric(FIS.Code)) %>%
  transform(NSA.Code = as.factor(NSA.Code)) %>%
  transform(YB = as.numeric(YB)) %>%
  transform(Ski = as.factor(Ski)) %>% 
  transform(disc = as.factor(disc)) %>%
  rename(NSA = NSA.Code)

# Rezultati veleslalomskih tekem
GS.altabadia <-
  left_join(seznam_rezultatov[[9]][-c(30), ] %>% mutate(venue = "Alta Badia"),
            GS.altabadia.vreme,
            by = character())
GS.cortina <-
  left_join(seznam_rezultatov[[10]] %>% mutate(venue = "Cortina"),
            GS.cortina.vreme,
            by = character())
GS.kranjskagora <-
  left_join(seznam_rezultatov[[11]] %>% mutate(venue = "Kranjska Gora"),
            GS.kranjskagora.vreme,
            by = character())
GS.lenzerheide <-
  left_join(seznam_rezultatov[[12]][-c(20:30), ] %>% mutate(venue = "Lenzerheide"),
            GS.lenzerheide.vreme,
            by = character())
GS.soelden <-
  left_join(seznam_rezultatov[[13]][-c(29, 30), ] %>% mutate(venue = "Soelden"),
            GS.soelden.vreme,
            by = character())
GS1.adelboden <-
  left_join(seznam_rezultatov[[14]][-c(28:30), ] %>% mutate(venue = "Adelboden1"),
            GS1.adelboden.vreme,
            by = character())
GS1.bansko <-
  left_join(seznam_rezultatov[[15]] %>% mutate(venue = "Bansko1"),
            GS1.bansko.vreme,
            by = character())
GS1.santacaterina <-
  left_join(
    seznam_rezultatov[[16]][-c(29:30), ] %>% mutate(venue = "Santa Caterina1"),
    GS1.santacaterina.vreme,
    by = character()
  )
GS2.adelboden <-
  left_join(seznam_rezultatov[[17]][-c(30), ] %>% mutate(venue = "Adelboden2"),
            GS2.adelboden.vreme,
            by = character())
GS2.bansko <-
  left_join(seznam_rezultatov[[18]][-c(30), ] %>% mutate(venue = "Bansko2"),
            GS2.bansko.vreme,
            by = character())
GS2.santacaterina <-
  left_join(
    seznam_rezultatov[[19]][-c(30), ] %>% mutate(venue = "Santa Caterina2"),
    GS2.santacaterina.vreme,
    by = character()
  )

veleslalom <- rbind(GS.altabadia, GS.cortina, GS.kranjskagora, GS.lenzerheide, 
                    GS.soelden, GS1.adelboden, GS1.bansko, GS1.santacaterina,
                    GS2.adelboden, GS2.bansko, GS2.santacaterina) %>% mutate(disc = "GS")
veleslalom$Run.1 <- paste(veleslalom$Run.1, veleslalom$X, sep = ":")
veleslalom$Run.2 <- paste(veleslalom$Run.2, veleslalom$X.1, sep = ":")
veleslalom <- veleslalom %>% dplyr::select(-c(8,11,15)) %>%
  transform(Rank = as.numeric(Rank)) %>%
  transform(Name = as.factor(Name)) %>%
  transform(Bib = as.numeric(Bib)) %>% 
  rename(NSA = NSA.Code) %>%
  transform(FIS.Code = as.numeric(FIS.Code)) %>%
  transform(NSA = as.factor(NSA)) %>%
  transform(YB = as.numeric(YB)) %>% 
  transform(Ski = as.factor(Ski)) %>%
  transform(disc = as.factor(disc)) %>%
  mutate(vreme = vreme1) %>% mutate(sneg = sneg2) %>%
  dplyr::select(-vreme1, -vreme2, -sneg1, -sneg2)



# Rezultati slalomskih tekem
SL.adelboden <-
  left_join(seznam_rezultatov[[27]][-c(27:30), ] %>% mutate(venue = "Adelboden"),
            SL.adelboden.vreme,
            by = character())
SL.altabadia <-
  left_join(seznam_rezultatov[[28]][-c(30), ] %>% mutate(venue = "Alta Badia"),
            SL.altabadia.vreme,
            by = character())
SL.cortina <-
  left_join(seznam_rezultatov[[29]] %>% mutate(venue = "Cortina"),
            SL.cortina.vreme,
            by = character())
SL.kranjskagora <-
  left_join(seznam_rezultatov[[30]][-c(27:39), ] %>% mutate(venue = "Kranjska Gora"),
            SL.kranjskagora.vreme,
            by = character())
SL.lenzerheide <-
  left_join(seznam_rezultatov[[31]][-c(17:30), ] %>% mutate(venue = "Lenzerheide"),
            SL.lenzerheide.vreme,
            by = character())
SL.madonnadicampiglio  <-
  left_join(
    seznam_rezultatov[[32]][-c(27:30), ] %>% mutate(venue = "Madonna di Campiglio"),
    SL.madonnadicampiglio.vreme,
    by = character()
  )
SL.schladming <-
  left_join(seznam_rezultatov[[33]][-c(26:30), ] %>% mutate(venue = "Schladming"),
            SL.schladming.vreme,
            by = character())
SL.zagreb <-
  left_join(seznam_rezultatov[[34]][-c(29:30), ] %>% mutate(venue = "Zagreb"),
            SL.zagreb.vreme,
            by = character())
SL1.chamonix <-
  left_join(seznam_rezultatov[[35]][-c(30), ] %>% mutate(venue = "Chamonix1"),
            SL.zagreb.vreme,
            by = character())
SL1.flachau <-
  left_join(seznam_rezultatov[[36]][-c(28:30), ] %>% mutate(venue = "Flachau1"),
            SL1.flachau.vreme,
            by = character())
SL2.chamonix <-
  left_join(seznam_rezultatov[[37]][-c(29:30), ] %>% mutate(venue = "Chamonix2"),
            SL2.chamonix.vreme,
            by = character())
SL2.flachau <-
  left_join(seznam_rezultatov[[38]][-c(27:30), ] %>% mutate(venue = "Flachau2"),
            SL2.flachau.vreme,
            by = character())

slalom <- rbind(SL.adelboden, 
                SL.altabadia, 
                SL.cortina, 
                SL.kranjskagora,
                SL.lenzerheide, 
                SL.madonnadicampiglio, 
                SL.schladming, 
                SL.zagreb,
                SL1.chamonix, 
                SL1.flachau, 
                SL2.chamonix, 
                SL2.flachau) %>% 
  mutate(disc = "SL")


# popravim narobe izpisani čas (zaradi ločevanja z ;)
slalom$Total <- paste(slalom$Total, slalom$X, sep = ":")

slalom <- slalom %>% dplyr::select(-c(12,14)) %>% 
  transform(Rank = as.numeric(Rank)) %>%
  transform(Name = as.factor(Name)) %>%
  transform(Bib = as.numeric(Bib)) %>% 
  transform(FIS.Code = as.numeric(FIS.Code)) %>%
  transform(NSA.Code = as.factor(NSA.Code)) %>%
  transform(YB = as.numeric(YB)) %>% 
  transform(Ski = as.factor(Ski)) %>% 
  transform(disc = as.factor(disc)) %>%
  mutate(vreme = vreme1) %>% mutate(sneg = sneg1) %>%
  dplyr::select(-vreme1, -vreme2, -sneg1, -sneg2) %>%
  rename(NSA = NSA.Code)



################################################################################
# Združevanje tabel
################################################################################

# Hitre discipline
hitre <- rbind(smuk, superG) %>% rename("Disciplina" = disc) %>%
  dplyr::select(
    Rank,
    FIS.Code,
    Name,
    YB,
    NSA,
    Time,
    Difference,
    Ski,
    venue,
    Disciplina,
    temperatura,
    vreme,
    sneg
  )

# Tehnične discipline
tehnicne <- rbind(veleslalom, slalom) %>%
  rename(Time = Total, Difference = Diff.) %>% rename("Disciplina" = disc) %>%
  dplyr::select(
    Rank,
    FIS.Code,
    Name,
    YB,
    NSA,
    Time,
    Difference,
    Ski,
    venue,
    Disciplina,
    vreme,
    sneg,
    temperatura
  )

# Urejanje glavne tabele, ki vsebuje vse podatke + dodane točke, ki jih prejme
# prvih 30 uvrščenih tekmovalcev
REZULTATI.VREME <- rbind(hitre, tehnicne) %>% 
  dplyr::select(Rank, Name, YB, NSA, Ski, venue, Disciplina, temperatura, vreme, sneg) %>%
  transform(NSA = as.factor(NSA)) %>%
  mutate(NSA = str_replace_all(NSA, pattern = "[0-9]", replacement = "")) %>%
  mutate(NSA = str_replace_all(NSA, pattern = "[\\t]", replacement = "")) %>%
  mutate(vreme = str_replace_all(vreme, pattern = "[\\t]", replacement = "")) %>%
  mutate(NSA = str_replace_all(NSA, pattern = "[ ]", replacement = "")) %>%
  transform(NSA = as.factor(NSA))
  
tocke_30 <-
  c(
    100,
    80,
    60,
    50,
    45,
    40,
    36,
    32,
    29,
    26,
    24,
    22,
    20,
    18,
    16,
    15,
    14,
    13,
    12,
    11,
    10,
    9,
    8,
    7,
    6,
    5,
    4,
    3,
    2,
    1
  )

uvrstitev <- 1:30

tocke <- tibble(tocke_30, uvrstitev)


# Glavna tabela:
REZULTATI.VREME <- left_join(REZULTATI.VREME, tocke, by = c("Rank" = "uvrstitev"))


# preverim, kakšna je tabela
summary(REZULTATI.VREME)

################################################################################
# Tabela z dobitniki velikih kristalnih globusov/skupnih zmagovalcev
################################################################################


# moški
linkM <- "https://ski-db.com/db/stats/overall_m_gc.php"

zM <- html_table(read_html(linkM))[[10]][-c(1),]
zmagovalci <- zM %>% 
  dplyr::select(-c(10)) %>% 
  rename(NSA = NAT) %>% 
  dplyr::select(Season, Winner, NSA, WINS, TOP3) %>%
  transform(NSA = as.factor(NSA)) %>%
  transform(Winner = as.factor(Winner)) %>%
  mutate(Season = rev(c(1967:2021)))

# ženske
linkW <- "https://ski-db.com/db/stats/overall_f_gc.php"

zW <- html_table(read_html(linkW))[[10]][-c(1),]
zmagovalke <- zW %>% 
  dplyr::select(-c(10)) %>% 
  rename(NSA = NAT) %>% 
  dplyr::select(Season, Winner, NSA, WINS, TOP3) %>%
  transform(NSA = as.factor(NSA)) %>%
  transform(Winner = as.factor(Winner)) %>%
  mutate(Season = rev(c(1967:2021)))


