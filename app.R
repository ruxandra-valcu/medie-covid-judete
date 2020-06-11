#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# data wrangling

library(stringr)
library(lubridate)
library(tidyr)
library(reshape2)

library(zoo)
library(jsonlite)
library(dplyr)
library(shiny)

# geographical
library(sf)

# plot
library(ggplot2)
library(ggrepel)


options(warn = -1)

# data downloaders and utilitary functions
mean_without_4 <- function(x) {
  x <- sort(x, decreasing = TRUE)
  x <- x[5:length(x)]
  mean(x, na.rm = TRUE)
}

mean_all <- function(x) mean(x, na.rm = TRUE)

get_data <- function() {
  judete <- list(
    judet = c(
      "Alba", "Arad", "Argeș", "Bacău", "Bihor", "Bistrița-Năsăud", "Botoșani" ,
      "Brăila", "Brașov", "București", "Buzău", "Călărași", "Caraș-Severin", "Cluj" ,
      "Constanța", "Covasna", "Dâmbovița", "Dolj", "Galați", "Giurgiu", "Gorj" ,
      "Harghita", "Hunedoara", "Iași", "Ialomița",    "Ilfov", "Maramureș", "Mehedinți" ,
      "Mureș", "Neamț", "Olt", "Prahova", "Sălaj", "Satu Mare", "Sibiu" ,
      "Suceava", "Teleorman", "Timiș", "Tulcea", "Vâlcea", "Vaslui", "Vrancea" 
    ),
    populatie = c(
      342376, 430629, 612431, 616168, 575398, 286225, 412626, 
      321212, 549217, 1883425, 451069, 306691, 295579, 691106, 
      684082, 210177, 518745, 660544, 536167, 281422, 341594, 
      310867, 418565, 772348, 274148, 388738, 478659, 265390, 
      550846, 470766, 436400, 762886, 224384, 344360, 397322, 
      634810, 380123, 683540, 213083, 371714, 395499, 340310
    )
  ) %>% as.data.frame(stringsAsFactors = FALSE)
  
  new_cases <- fromJSON("https://covid19.geo-spatial.org/external/charts_vasile/assets/json/cazuri_zile_long.json") %>%
    suppressMessages() %>%
    rename(
      judet = Judet,
      data = Data,
      total = `Cazuri total`,
      noi = `Cazuri noi`
    ) %>%
    left_join(judete, by = "judet") %>%
    group_by(judet) %>%
    arrange(judet, data) %>%
    mutate(
      data = ymd(data),
      noi = as.numeric(noi),
      total_p = total * 1000000 / populatie,
      noi_p = noi * 1000000 / populatie,
      noi_na = ifelse(noi < 0, NA, noi_p),
      noi = ifelse(noi < 0, NA, noi)
    ) %>%
    mutate(
      media10 = rollapply(
        data = noi_na,
        width = 14,
        FUN = mean_without_4,
        align = "right",
        fill = NA
      ),
      media14 = rollapply(
        data = noi_na,
        width = 14,
        FUN = mean_all,
        align = "right",
        fill = NA
      ),
      media10_regular = rollapply(
        data = noi,
        width = 14,
        FUN = mean_without_4,
        align = "right",
        fill = NA
      ),
      media14_regular = rollapply(
        data = noi,
        width = 14,
        FUN = mean_all,
        align = "right",
        fill = NA
      ),
      label10 = paste0(judet, ": ", as.character(round(media10, 2))),
      label14 = paste0(judet, ": ", as.character(round(media14, 2)))
    ) %>%
    ungroup()
  
  nuts3 <- st_read("nuts3_ro.shp") %>%
    suppressMessages() %>%
    #dir(filter(CNTR_CODE == "RO") %>%
    rename(judet = NUTS_NAME) %>%
    arrange(judet) %>%
    mutate(judet = judete$judet) #weird diacritics issue
  
  new_cases_geometry <- inner_join(new_cases, nuts3, by = "judet")
  return(new_cases_geometry)
}





ui <- fluidPage(
  titlePanel("Media numărului de cazuri noi pe județe"),
  fluidRow(
    radioButtons(
      inputId = "mean_type",
      label = NULL,
      choiceNames = c(
        "Media numărului de cazuri noi/milion de locuitori din ultimele 14 zile",
        "Media numărului de cazuri noi/milion de locuitori din cele mai bune 10 din ultimele 14 zile (ascunde focare unice, ținute sub control)"),
      choiceValues = c(14, 10),
      width = "100%"
    ),
    plotOutput(
      outputId = "plot",
      width = "100%"
    ),
    sliderInput(
      inputId = "date", 
      label = "Data", 
      min = ymd("2020-04-17"), 
      max = ymd(Sys.Date() - 1), 
      value = ymd(Sys.Date() - 1),
      width = '100%'
    )
  ),
  fluidRow(
    column( 2,
      selectInput(
        "judet", "Județ",
        c(
          "Alba", "Arad", "Argeș", "Bacău", "Bihor", "Bistrița-Năsăud", "Botoșani" ,
          "Brăila", "Brașov", "București", "Buzău", "Călărași", "Caraș-Severin", "Cluj" ,
          "Constanța", "Covasna", "Dâmbovița", "Dolj", "Galați", "Giurgiu", "Gorj" ,
          "Harghita", "Hunedoara", "Iași", "Ialomița",    "Ilfov", "Maramureș", "Mehedinți" ,
          "Mureș", "Neamț", "Olt", "Prahova", "Sălaj", "Satu Mare", "Sibiu" ,
          "Suceava", "Teleorman", "Timiș", "Tulcea", "Vâlcea", "Vaslui", "Vrancea" 
        ),
        selected = "Suceava"
      )
    ),
    column( 10,
      plotOutput(
        outputId = "judet_plot",
        width = "100%",
        height = "300px"
      )
    )
  ),
  span(textOutput("blame"), style = "color:grey; font-size: 75%")
)



server <- function(input, output, session) {
  dataset <- reactive({get_data()})
  cases <- get_data()
  
  output$plot <- renderPlot({
    cases <- dataset()
    test <- subset(cases, data == input$date) %>%
      mutate(
        fm = input$mean_type == 14,
        media = ifelse(fm, media14, media10),
        label = ifelse(fm, label14, label10)
      ) %>%
      as.data.frame(stringsAsFactors = FALSE)
    p <- ggplot(data = test) +
      geom_sf(aes(fill = log(media + 1), geometry = geometry)) +
      ggrepel::geom_label_repel(
        aes(label = label, geometry = geometry), stat = "sf_coordinates"
      ) +
      scale_fill_gradientn(
        limits = c(log(1), log(126)),
        colors = c("#006837", "#a6d96a","#fee08b", "#f46d43", "#d73027", "#c51b7d", "#762a83", "#40004b"),
        breaks = c(log(1), log(2), log(3), log(6), log(11), log(26), log(51), log(101)),
        labels = c(0, 1, 2, 5, 10, 25, 50, 100)
      ) +
      theme_void() +
      theme(legend.title = element_blank())

    p
  })
  
  output$judet_plot <- renderPlot({
    cases <- dataset()
    judet <- cases %>% 
      subset(judet == input$judet) %>%
      select(
        Data = data, 
        `Cazuri noi` = noi, 
        `Media celor mai bune 10 din ultimele 14 zile` = media10_regular, 
        `Media ultimelor 14 zile` = media14_regular
        ) %>%
      pivot_longer(2:4, names_to = "Indicator", values_to = "Valoare") %>%
      mutate(
        Indicator = factor(
          Indicator, 
          levels = c("Cazuri noi", "Media ultimelor 14 zile", "Media celor mai bune 10 din ultimele 14 zile")
          )
        )
    max_value <- max(judet$Valoare, na.rm = TRUE)
    y_scale_unit = 10 ^ (floor(log(max_value, 10)))
    p <- ggplot(data = judet, aes(x = Data, y = Valoare, color = Indicator)) +
      geom_line() +
      scale_y_continuous(breaks = seq(0, max_value, by = y_scale_unit)) +
      scale_color_manual(values = c("#AAAAAA", "#E69F00", "#56B4E9"))
    p
  })
  
  output$blame <- renderText({
    'Creat de Ruxandra Vâlcu cu ajutorul setului de date geo-spatial.org. 
    Găzduit de SAGE Group - Universitatea de Vest din Timișoara.'
    })
 

}

# Run the application 
shinyApp(ui, server)
