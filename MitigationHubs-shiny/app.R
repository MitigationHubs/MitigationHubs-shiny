#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
source('config.R')

# Define plots
plt_fallzahlen <- function(dat_dots, dat_smooth = NULL, dat_gr = NULL, log_scale = T, shared_axes = T) {
    plt <- ggplot(mapping = aes(x = Meldedatum)) + 
        geom_line(aes(y = csum_LK_100kEinwohner), dat_dots, color = 'black', size = 1.2) +
        geom_point(aes(y = csum_LK_100kEinwohner), dat_dots, color = 'black', size = 7, shape = '\u2716')
    if (!is.null(dat_gr)) {
        plt <- plt + 
            geom_line(aes(y = gr_ma), dat_gr, color = 'orange', alpha = 0.7, size = 1.2) + 
            geom_point(aes(y = gr_ma), dat_gr, color = 'orange', size = 7, shape = '\u2716')
    }
    if (!is.null(dat_smooth)) {
        plt <- plt + 
            geom_smooth(aes(y = csum_ma_LK_100kEinwohner), dat_smooth %>% filter(use == T), size = 1.2)
    }
    plt <- plt +
        facet_wrap(~ Landkreis, scales = {if(shared_axes) {'fixed'} else {'free'}})
    if (log_scale & !is.null(dat_gr)) {
        plt <- plt + 
            scale_y_log10(sec.axis = dup_axis(name = 'Wachstumsrate'))
    } else if (log_scale) {
        plt <- plt + 
            scale_y_log10()
    } else if (!is.null(dat_gr)) {
        plt <- plt + 
            scale_y_continuous(sec.axis = dup_axis(name = 'Wachstumsrate'))
    } else {
        plt <- plt + 
            scale_y_continuous()
    }
    plt <- plt + 
        labs(y = 'Fälle pro 100.000 Einwohner') + 
        theme_bw() + 
        theme(text = element_text(size = 24), title = element_text(size = 30, face = 'bold'),
              strip.background = element_blank(), strip.text = element_text(size = 30, face = 'bold', hjust = 0))
    return(plt)
}

# load data
load(files$LK_dat)

# Now define the actual app
# Define UI contents
## Header
header <- dashboardHeader(
    title = "MitigationHubs"
)

## Siderbar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Fallzahlen", tabName = "fallzahlen", icon = icon("chart-line"),
                 badgeLabel = "Neu", badgeColor = "green"),
        menuItem("Prediktoren", tabName = "prediktoren", icon = icon("adjust"),
                 badgeLabel = "Im Aufbau", badgeColor = "orange")
    )
)

## Body
body <- dashboardBody(
    tabItems(
        # Content "Fallzahlen"
        tabItem(tabName = "fallzahlen",
                fluidRow(
                    box(
                        title = 'Fallzahlenentwicklung', solidHeader = T, status = "primary", width = 12,
                        plotOutput("fallzahlen_plot1", height = 750),
                        textOutput('fallzahlen_publikationsdatum')
                    )
                ),
                fluidRow(
                    box(
                        title = "Landkreisauswahl", status = "primary", width = 4,
                        selectizeInput(
                            'fallzahlen_landkreis_selector', paste('Landkreissuche (Namen eintippen) | Maximale Anzahl:', nlk_max), 
                            choices = LK_set$Landkreis, selected = clks_selected,
                            multiple = TRUE, options = list(maxItems = nlk_max)
                        ),
                        actionButton("fallzahlen_plot_action1", "Zeig mir die Fallzahlen", icon = icon("calculator"))
                    ),
                    box(
                        title = "Gegenmaßnahmen", status = "primary", width = 4,
                        checkboxGroupInput('fallzahlen_checkbox_gm', 'Bisher gesammelte Maßnahmen (noch nicht für alle Landkreise verfügbar)',
                                           list('Test 1' = T, 'Test 2' = F)),
                        actionButton("fallzahlen_plot_action2", "Zeig mir die Gegenmaßnahmen", icon = icon("calculator")),
                        tags$div(class="header", checked=NA,
                                 tags$h4("Gegenmaßnahme melden?",
                                         tags$a(href="https://forms.gle/3Jd2hRYbJGRBZ42d6?hl=de", "Hier Mitmachen!"))
                        )
                    ),
                    box(
                        title = "Darstellungsoptionen", status = "primary", width = 4,
                        checkboxInput("fallzahlen_checkbox_gr", "Wachstumsraten", T),
                        checkboxInput("fallzahlen_checkbox_ls", "Logarithmische Skala", T),
                        checkboxInput("fallzahlen_checkbox_ta", "Gemeinsame Achsen", T),
                        actionButton("fallzahlen_plot_action3", "Änderungen anwenden", icon = icon("calculator"))
                    )
                )
        ),
        
        # Content "Prediktoren"
        tabItem(tabName = "prediktoren",
                h2("Prediktoren für die Entwicklung der Fallzahlen"),
                fluidRow(
                    valueBox(10 * 2, "Alter 18-24", icon = icon("plus"), color = 'red'),
                    valueBox(10 * 2, "Alter > 65", icon = icon("minus"), color = 'blue'),
                    valueBox(10 * 2, "Einkommen", icon = icon("plus"), color = 'red'),
                    valueBox(10 * 2, "Einwohner", icon = icon("circle-notch"), color = 'yellow'),
                    valueBox(10 * 2, "Krankenhausbetten", icon = icon("circle-notch"), color = 'yellow'),
                    valueBox(10 * 2, "Personenkilometer", icon = icon("minus"), color = 'blue')
                ),
                h2("Prediktoren für die Entwicklung der Wachstumsraten")
        )
    )
)

# Define UI
ui <- dashboardPage(
    header,
    sidebar,
    body,
    skin = "yellow"
)

# Define Server
server <- function(input, output) {
    
    output$fallzahlen_plot1 <- renderPlot({
        #data <- histdata[seq_len(input$fallzahlen_slider)]
        #hist(data)
        input$fallzahlen_plot_action1
        input$fallzahlen_plot_action2
        input$fallzahlen_plot_action3
        
        llog <- isolate(input$fallzahlen_checkbox_ls)
        lgr <- isolate(input$fallzahlen_checkbox_gr)
        lta <- isolate(input$fallzahlen_checkbox_ta)
        clks <- isolate(input$fallzahlen_landkreis_selector)
        
        IdsLandkreis <- LK_set %>% 
            filter(Landkreis %in% clks) %>% 
            .$IdLandkreis
        
        if (lgr) {
            plt_fallzahlen(LK_dat_csum_proc %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_ma %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_gr %>% filter(IdLandkreis %in% IdsLandkreis),
                           llog, lta)
        } else {
            plt_fallzahlen(LK_dat_csum_proc %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_ma %>% filter(IdLandkreis %in% IdsLandkreis),
                           NULL,
                           llog, lta)
        }
        
    })
    
    output$fallzahlen_publikationsdatum <- renderText(
        paste('Letzte Aktualisierung der Datenlage (RKI):', LK_time)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
