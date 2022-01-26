# 3. faza: Vizualizacija podatkov

source("lib/libraries.r")
source("lib/uvozi.zemljevid.r")


################################################################################
# GRAFI
################################################################################
# 1. graf: TOČKE GLEDE NA DISCIPLINO IN SMUČI

smuci <- levels(REZULTATI.VREME$Ski)[-c(9)]

graf1 <- ggplot(REZULTATI.VREME %>% dplyr::select(Ski, Disciplina, tocke_30) %>%
                  filter(Ski %in% smuci)) + 
  aes(x = Ski) + 
  geom_bar(aes(fill = Disciplina)) + 
  scale_fill_manual(labels = c("Smuk", "Superveleslalom", "Veleslalom", "Slalom"), values = c("darkgreen", "lightgreen", "darkblue", "skyblue")) +
  xlab("Smuči") + 
  ylab("Točke") + 
  ggtitle("Smuči glede na točke v posamezni disciplini v sezoni 2020/21") + 
  guides(fill=guide_legend(title="Disciplina:")) + 
  theme(legend.title = element_text(color = "black", size = 11),
        legend.background = element_rect(colour ="#006699", fill = "white"), 
        plot.title = element_text(color = "#006699", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 25))
graf1

################################################################################
# 2. graf: TOČKE GLEDE NA SNEŽNO PODLAGO IN SMUČI

graf2 <- ggplot(REZULTATI.VREME %>% dplyr::select(Ski, sneg, tocke_30) %>% 
                  filter(Ski %in% smuci)) + 
  aes(x = Ski) + 
  geom_bar(aes(fill = sneg)) +
  scale_fill_manual(labels = c("Trda in ledena podlaga", "Kompakten sneg", 
                               "Južen spomladanski sneg", "Mehak razmočen sneg", "Zbit sneg"),
                      values = c("#CCFFFF", "#339999", 
                                 "#99CC00", "#FFFF66", "#336666")) +
  xlab("Smuči") + 
  ylab("Točke") + 
  ggtitle("Smuči glede na snežno podlago v sezoni 2020/21") + 
  guides(fill=guide_legend(title="Snežna podlaga:")) + 
  theme(legend.title = element_text(color = "black", size = 11),
        legend.background = element_rect(colour ="#006699", fill = "white"), 
        plot.title = element_text(color = "#006699", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 25))
graf2


################################################################################
# 3. graf: tortni diagram za vsako disciplino glede na stopničke, barve pa se razlikujejo
# glede na to katere smuči uporabljamo

# shiny = ne samo za stopničke ampak za vsakih recimo top 3, 5, 10, 15, 20, 30

barve_smuci <- c("#FF0000", "#FFFF00", "#FFFFCC", "#009966", 
                 "#330000", "#FF3300", "#000066", "#990000", 
                 "#FF6633", "#00CCCC", "#CCCC00")


stopnicke.tocke <- function(disciplina){
  stopnicke.tocke <- REZULTATI.VREME %>% 
    filter(Disciplina == disciplina) %>% 
    filter(Rank < 4) %>% 
    dplyr::select(Ski, tocke_30, Disciplina) %>%
    filter(Ski %in% smuci)
  return(stopnicke.tocke)
}

stopnicke.DH <- stopnicke.tocke("DH")
stopnicke.SG <- stopnicke.tocke("SG")
stopnicke.GS <- stopnicke.tocke("GS")
stopnicke.SL <- stopnicke.tocke("SL")

graf.DH <- ggplot(stopnicke.DH) + 
  aes(x = "", y = tocke_30, fill = Ski) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) + theme_void() +
  scale_fill_hue() + 
  scale_fill_manual(values = barve_smuci[-c(4)]) +
  ggtitle("Smuk")
graf.DH

graf.SG <- ggplot(stopnicke.SG) + 
  aes(x = "", y = tocke_30, fill = Ski) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) + theme_void() +
  scale_fill_hue() + 
  scale_fill_manual(values = barve_smuci[-c(2,4,5)]) +
  ggtitle("Superveleslalom")
graf.SG

graf.GS <- ggplot(stopnicke.GS) + 
  aes(x = "", y = tocke_30, fill = Ski) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) + theme_void() +
  scale_fill_hue() + 
  scale_fill_manual(values = barve_smuci[-c(4,5,9)]) +
  ggtitle("Veleslalom")
graf.GS

graf.SL <- ggplot(stopnicke.SL) + 
  aes(x = "", y = tocke_30, fill = Ski) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) + theme_void() +
  scale_fill_hue() + 
  scale_fill_manual(values = barve_smuci[-c(2,4,8,9)]) +
  ggtitle("Slalom")
graf.SL

graf3 <- ggarrange(graf.DH, graf.SG, graf.GS, graf.SL, ncol = 2, nrow = 2)
graf3 <- annotate_figure(
  graf3, 
  top = text_grob("Točke uvrščenih na stopničke glede na smuči v sezoni 2020/21", 
                  color = "#006699", size = 15))
graf3

################################################################################
# 4. graf: ŠKATLE Z BRKI PO DISCIPLINAH GLEDE NA STAROST
starosti <- REZULTATI.VREME %>% dplyr::select(Disciplina, YB, Name, tocke_30) %>%
  group_by(Name) %>% summarise(tocke_30 = sum(tocke_30), YB, Disciplina)

graf4 <- ggplot(starosti) + 
  geom_boxploth(notch=FALSE) + 
  aes(x = YB, y = "") + 
  geom_jitter(position = position_jitter()) +
  facet_wrap(.~Disciplina) +
  ggtitle("Starost tekmovalcev glede na točke in disciplino v sezoni 2020/21")
graf4

################################################################################
#5. graf: UVRSTITVE NA STOPNIČKE

stopnicke <- REZULTATI.VREME %>% filter(Rank < 4) %>% dplyr::select(Rank, Ski) %>%
  transform(Rank = as.factor(Rank))

graf5 <- ggplot(stopnicke) + 
  aes(x = Ski, fill = Rank, labels = TRUE) +
  scale_fill_manual(values = c("#D4AF37", "#C0C0C0", "#b08d57")) +
  xlab("Smuči") + 
  ylab("Stpničke") + 
  ggtitle("Uvrstitve na stopničke v sezoni 2020/21") + 
  geom_bar()
graf5

################################################################################
# 6. graf : NAJBOLJŠIH DESET SKUPNO ZA VSAKO DISCIPLINO

# v shiny dodaj koliko najbojših želiš, da se upošteva (3, 5, 10, 15, 20, 30)

skupno <- function(disc) {
  REZULTATI.VREME %>%
  filter(Disciplina == disc) %>%
  group_by(Name) %>% 
  summarise(tocke = sum(tocke_30)) %>%
  arrange(desc(tocke))
}


skupnoDH <- skupno("DH")[c(1:100),] %>% rename(tockeDH = tocke)
skupnoSG <- skupno("SG")[c(1:100),] %>% rename(tockeSG = tocke)
skupnoGS <- skupno("GS")[c(1:100),] %>% rename(tockeGS = tocke)
skupnoSL <- skupno("SL")[c(1:100),] %>% rename(tockeSL = tocke)


top <- REZULTATI.VREME %>%
  group_by(Name) %>% 
  summarise(tocke = sum(tocke_30)) %>%
  arrange(desc(tocke))

skupaj1 <- full_join(skupnoDH, top, by = c("Name" = "Name")) %>% 
  arrange(desc(tocke))
skupaj2 <- full_join(skupaj1, skupnoSG, by = c("Name" = "Name")) %>% 
  arrange(desc(tocke))
skupaj3 <- full_join(skupaj2, skupnoGS, by = c("Name" = "Name")) %>% 
  arrange(desc(tocke))
skupaj4 <- full_join(skupaj3, skupnoSL, by = c("Name" = "Name"))[c(1:10),] %>% 
  arrange(desc(tocke))

# tabela s točkami najboljših desetih:
top.deset <- skupaj4[c(1:10),] %>% 
  replace_na(list(tockeDH = 0, tockeSG = 0, tockeGS = 0, tockeSL = 0)) %>% 
  arrange(desc(tocke))


# naredi posebaj graf (kakšen??) za vsako disciplino


najboljsih.10 <- as.character(top.deset$Name[1:10])

# kaj pa če bi REZULTATI.VREME filtrirala po teh najboljših desetih???
# kakšno anlizo bi potem lahko naredila??:
naj <- REZULTATI.VREME %>% 
  filter(Name %in% najboljsih.10) %>% 
  dplyr::select(Rank, Name, NSA, Ski, tocke_30, Disciplina, sneg) %>%
  transform(Name = as.factor(Name))

graf6 <- ggplot(naj) +
  aes(x = Disciplina, fill = Ski) +
  geom_bar()+
  scale_fill_manual(values = c("#FF0000", "#FFFFCC", "#FF3300", "#990000")) +
  facet_wrap(.~ Name) +
  xlab("") +
  ylab("") +
  ggtitle("")
  
graf6


################################################################################
# 7. graf: Obnašanje smuči v odvisnosti od temperature

graf7 <-ggplot(tocke_T) + 
  aes(x = temperatura, y = tocke_30, color = Disciplina) + 
  geom_jitter() + 
  scale_color_manual(values = c("darkgreen", "lightgreen", "darkblue", "skyblue")) +
  stat_smooth(method = 'lm') + 
  facet_wrap(.~ Ski)
graf7




################################################################################
# ZEMLJEVIDI
################################################################################
# 1. zemljevid: zemljevid po državah glede kristalnih globusov


# Uvoz zemljevida sveta (Evropa + Severna Amerika)
zemljevid <-
  uvozi.zemljevid(
    "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
    "ne_50m_admin_0_countries",
    mapa = "zemljevidi",
    pot.zemljevida = "",
    encoding = "UTF-8"
  ) %>%
  fortify() %>% filter(CONTINENT %in% c("Europe", "North America"),
                       long < 50 & long > -170 & lat > 20 & lat < 85)

# za moške:
enice <- c(1:55) * 0 + 1
zmage.po.drzavah.M <- zmagovalci %>%
  mutate(st.zmag = enice) %>%
  group_by(NSA) %>%
  summarise(Zmage = sum(st.zmag))


zemljevid1M <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Zmage) +
  geom_polygon(data = zmage.po.drzavah.M %>% right_join(zemljevid, by = c("NSA" = "ADM0_A3"))) +
  xlab("") +
  ylab("")
zemljevid1M


# za ženske:
enice <- c(1:55) * 0 + 1
zmage.po.drzavah.W <- zmagovalke %>%
  mutate(st.zmag = enice) %>%
  group_by(NSA) %>%
  summarise(Zmage = sum(st.zmag))
  
zmage.po.drzavah.W <- zmage.po.drzavah.W %>%
  mutate(NSA = str_replace_all(zmage.po.drzavah.W$NSA, pattern = "CRO", replacement = "HRV"))


zemljevid1W <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Zmage) +
  geom_polygon(data = zmage.po.drzavah.W %>% right_join(zemljevid, by = c("NSA" = "ADM0_A3"))) +
  xlab("") +
  ylab("") + 
  scale_color_discrete()
zemljevid1W

zemljevid1 <- ggarrange(zemljevid1M, zemljevid1W, labels = c("Moški", "Ženske"), 
                        common.legend = TRUE, legend = "bottom")
zemljevid1 <- annotate_figure(
  zemljevid1, 
  top = text_grob("Število osvojenih velikih kristalnih globusov 1967-2021", 
                  color = "#006699", size = 15))
zemljevid1

################################################################################
# 2. zemljevid: zemljevid z zmagami za sezono 2020/21

zmage2021 <- REZULTATI.VREME %>% 
  filter(Rank == 1) %>% 
  group_by(NSA) %>%
  summarise(Zmage = sum(Rank))
zmage2021 <- zmage2021 %>% mutate(NSA = str_replace_all(zmage2021$NSA, pattern = "SLO", replacement = "SVN"))

zemljevid2 <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Zmage) +
  geom_polygon(data = zmage2021 %>% right_join(zemljevid, by = c("NSA" = "ADM0_A3"))) +
  xlab("") +
  ylab("") +
  ggtitle("Število zmag v sezoni 2020/21")
zemljevid2
