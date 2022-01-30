library(shiny)

shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi.zemljevid(input$stevilo)
  })
})

# izbiramo lahko med tem, da nam kaže, 
# koliko zmag ima katera država in med tem, da prikazuje države po točkah za
# prve 3, 5, 10, 15, 20, 30 tekmovalcev

narisi.zemljevid <- function(stevilo){
  uvrstitve2021 <- REZULTATI.VREME %>% 
    filter(Rank <= as.numeric(stevilo)) %>%
    dplyr::select(Rank, NSA, tocke_30) %>%
    group_by(NSA) %>% summarise(tocke_30 = sum(tocke_30))
  
  uvrstitve2021 <- uvrstitve2021 %>% 
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "SLO", replacement = "SVN"))
  uvrstitve2021 <- uvrstitve2021 %>% 
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "CRO", replacement = "HRV"))
  uvrstitve2021 <- uvrstitve2021 %>% 
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "GER", replacement = "DEU"))
  uvrstitve2021 <- uvrstitve2021 %>%
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "SUI", replacement = "CHE"))
  uvrstitve2021 <- uvrstitve2021 %>%
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "NED", replacement = "NLD"))
  uvrstitve2021 <- uvrstitve2021 %>%
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "BUL", replacement = "BGR"))
  uvrstitve2021 <- uvrstitve2021 %>%
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "GRE", replacement = "GRC"))
  uvrstitve2021 <- uvrstitve2021 %>%
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "MON", replacement = "MCO"))
  uvrstitve2021 <- uvrstitve2021 %>%
    mutate(NSA = str_replace_all(uvrstitve2021$NSA, pattern = "RFS", replacement = "RUS"))
  uvrstitve2021
  
  zemljevid.shiny <- ggplot() +
    aes(x = long, y = lat, group = group, fill = tocke_30) +
    geom_polygon(data = uvrstitve2021 %>% right_join(zemljevid, by = c("NSA" = "ADM0_A3"))) +
    xlab("") +
    ylab("") +
    ggtitle("Osvojene točke po državah v sezoni 2020/21") +
    coord_fixed(ratio = 2) +
    guides(fill=guide_legend(title="Točke")) + 
    theme(legend.title = element_text(color = "black", size = 11),
          legend.background = element_rect(colour ="#006699", fill = "white"), 
          plot.title = element_text(color = "#006699", hjust = 0.5, size = 15))
  print(zemljevid.shiny)
}
