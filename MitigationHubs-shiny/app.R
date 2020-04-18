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
library(googledrive)
library(ggplot2)
library(ggnewscale)
library(readr)
library(dplyr)
library(readxl)
source('config.R')

# deauth googledrive
drive_deauth()

# check if data is up to date
update <- list(
    cases = TRUE,
    measuresf = TRUE
)
drive_download(
    file = drive_get(as_id(gfiletokens$update)), 
    path = gfiles$update,
    type = gtypes$update, 
    overwrite = TRUE
)
g_update <- read_csv(
    gfiles$update
)
g_update <- as.list(g_update$last_update) %>% 
    setNames(g_update$name)

if (file.exists(afiles$log_update)) {
    load(afiles$log_update)
    if (log_update$cases == g_update$cases & file.exists(afiles$cases)) update$cases <- FALSE
    if (log_update$measuresf == g_update$measures & file.exists(afiles$measuresf)) update$measuresf <- FALSE
}

# load data
if (update$cases) {
    drive_download(
        file = drive_get(as_id(gfiletokens$cases)), 
        path = gfiles$cases,
        type = gtypes$cases, 
        overwrite = TRUE
    )
    tibs <- excel_sheets(gfiles$cases)
    invisible(
        lapply(tibs, 
           function(s) {
               temp <- read_xlsx(gfiles$cases, sheet = s, col_types = actypes$cases[[s]])
               if ('Meldedatum' %in% colnames(temp)) {
                   temp <- mutate_at(temp, vars(Meldedatum), as.Date)
               }
               assign(x = s, value = temp, envir = globalenv())
               }
           )
    )
    save(list = tibs, file = afiles$cases)
    print('Loaded case data from GDrive and saved to RData')
} else {
    load(afiles$cases)
    print('Loaded case data from RData')
}
if (update$measuresf) {
    drive_download(
        file = drive_get(as_id(gfiletokens$measuresf)), 
        path = gfiles$measuresf,
        type = gtypes$measuresf, 
        overwrite = TRUE
    )
    tibs <- excel_sheets(gfiles$measuresf)
    invisible(
        lapply(tibs, 
               function(s) assign(s, read_xlsx(gfiles$measuresf, sheet = s, col_types = actypes$measuresf[[s]]), envir = globalenv()))
    )
    save(list = tibs, file = afiles$measuresf)
    print('Loaded measure data from GDrive and saved to RData')
} else {
    load(afiles$measures)
    print('Loaded measure data from RData')
}
# type conversions for downstream
LK_meas <- LK_meas %>% mutate_at(vars(date, wann_aufgehoben), as.Date)

# save update log
log_update <- g_update
save(log_update, file = afiles$log_update)

# Define plots
plt_fallzahlen <- function(dat_dots, dat_smooth = NULL, dat_gr = NULL, dat_meas = NULL, log_scale = T, shared_axes = T) {
    # data processing
    ## should be minimal at this place
    ## -> move as much processing as possible to preprocessing scripts
    ## or at least out of app and server (and functions executed within them)
    #dat_gr <- dat_gr %>% mutate_at(vars(Meldedatum), as.Date)
    if (log_scale) {
        dat_dots <- dat_dots %>% 
            filter(csum_LK_100kEinwohner >= 1)
        if(!is.null(dat_smooth)) {
            dat_smooth <- dat_smooth %>% 
                filter(csum_ma_LK_100kEinwohner >= 1)   
        }
        if (!is.null(dat_gr)) {
            dat_gr <- dat_gr %>% 
                inner_join(dat_dots %>% 
                               group_by(IdLandkreis) %>% 
                               summarize(mindat = min(Meldedatum)), by = 'IdLandkreis') %>% 
                filter(Meldedatum >= mindat) %>% 
                select(-mindat)
        }
    }
    
    # actual plotting
    plt <- ggplot(mapping = aes(x = Meldedatum)) + 
        geom_col(aes(y = csum_LK_100kEinwohner, fill = 'Fallzahlen'), dat_dots, size = 1.2, alpha = 0.2) +
        geom_point(aes(y = csum_LK_100kEinwohner, color = 'Fallzahlen'), dat_dots, size = 7, shape = '\u2716') + 
        scale_fill_manual(values = csc$lblue, guide = guide_legend('Fallzahlen', override.aes = list(size = 5))) + 
        scale_color_manual(values = csc$lblue, guide = guide_legend('Fallzahlen')) + 
        new_scale_colour() + 
        new_scale_fill()
    if (!is.null(dat_gr)) {
        if (log_scale) {
            dat_gr <- dat_gr %>% 
                mutate(gr_ma = 10^gr_ma)
        } else {
            dat_gr <- dat_gr %>% 
                mutate(gr_ma = 100*gr_ma)
        }
        plt <- plt + 
            geom_line(aes(y = gr_ma, color = 'Wachstumsraten'), dat_gr, alpha = 0.7, size = 1.2) + 
            geom_point(aes(y = gr_ma, color = 'Wachstumsraten'), dat_gr, size = 7, shape = '\u2716') + 
            scale_color_manual(values = csc$red, guide = guide_legend(override.aes = list(size = 5))) + 
            new_scale_color()
    }
    if (!is.null(dat_smooth)) {
        plt <- plt + 
            geom_smooth(aes(y = csum_ma_LK_100kEinwohner, color = 'Interpolation\n(mit Fehlerbereich)'), 
                        dat_smooth %>% filter(use == T), size = 1.2) + 
            scale_color_manual(values = csc$lorange, guide = guide_legend(override.aes = list(size = 5))) + 
            new_scale_color()
    }
    if (!is.null(dat_meas)) {
        if (log_scale & !shared_axes) {
            dat_meas <- dat_meas %>% 
                inner_join(dat_dots %>% 
                               group_by(IdLandkreis) %>% 
                               summarise(y = 2.5 * max(csum_LK_100kEinwohner)),
                           by = 'IdLandkreis')
        } else if (log_scale & shared_axes) {
            dat_meas <- dat_meas %>% 
                mutate(y = 2.5 * max(dat_dots$csum_LK_100kEinwohner))
        } else if (!shared_axes) {
            dat_meas <- dat_meas %>% 
                inner_join(dat_dots %>% 
                               group_by(IdLandkreis) %>% 
                               summarise(y = 1.15 * max(csum_LK_100kEinwohner)),
                           by = 'IdLandkreis')
        } else {
            dat_meas <- dat_meas %>% 
                mutate(y = 1.15 * max(dat_dots$csum_LK_100kEinwohner))
        }
        plt <- plt + 
            geom_vline(aes(xintercept = date, color = measure_short), dat_meas, size = 1.2) + 
            geom_label(aes(x = date, y = y, color = measure_short, label = measure_short), dat_meas, hjust = 0.5, vjust = 1) + 
            scale_color_viridis_d(guide = FALSE)#guide_legend('Gegenmaßnahme', override.aes = list(size = 5)))
    }
    plt <- plt +
        facet_wrap(~ Landkreis, scales = {if(shared_axes) {'fixed'} else {'free'}})
    if (log_scale & !is.null(dat_gr)) {
        plt <- plt + 
            scale_y_log10(sec.axis = dup_axis(name = 'Wachstumsrate pro Tag / %',
                                              breaks = 10^gr_ax_breaks, labels = gr_ax_breaks * 100))
    } else if (log_scale) {
        plt <- plt + 
            scale_y_log10()
    } else if (!is.null(dat_gr)) {
        plt <- plt + 
            scale_y_continuous(sec.axis = dup_axis(name = 'Wachstumsrate pro Tag / %',
                                                   breaks = gr_ax_breaks * 100, labels = gr_ax_breaks * 100))
    } else {
        plt <- plt + 
            scale_y_continuous()
    }
    plt <- plt + 
        labs(y = 'Fälle pro 100.000 Einwohner') + 
        theme_bw() + 
        theme(text = element_text(size = 24), title = element_text(size = 30, face = 'bold'), 
              axis.title.y.left = element_text(color = csc$lblue), axis.text.y.left = element_text(color = csc$lblue), 
              axis.title.y.right = element_text(color = csc$red), axis.text.y.right = element_text(color = csc$red), 
              strip.background = element_blank(), strip.text = element_text(size = 30, face = 'bold', hjust = 0),
              legend.position = 'bottom', legend.box.background = element_rect(color = 'black'), legend.box.just = 'top',
              legend.title = element_blank(), legend.key.size = unit(1, 'cm'))
    return(plt)
}

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
                        title = 'Fallzahlenentwicklung der Infektion mit Covid-19', solidHeader = T, status = "primary", width = 12,
                        plotOutput("fallzahlen_plot1", height = 600),
                        textOutput('fallzahlen_publikationsdatum')
                    )
                ),
                fluidRow(
                    column(
                        width = 4,
                        box(
                            status = "warning", width = 12, title = "Was sehe ich hier?",
                            #tags$div(class="header", checked=NA,
                            #         tags$h2("Was sehe ich hier?")
                            #),
                            tags$div(class="text", checked=NA,
                                     tags$h4("Bevor du loslegst, schau dir unser",
                                             tags$a(href="", "Erklärvideo"),
                                             "an oder wirf einen Blick in unsere",
                                             tags$a(href="", "Kurzanleitung"), 
                                             ".")
                            )
                        ),
                        box(
                            status = "warning", width = 12, 
                            tags$div(class="header", checked=NA,
                                     tags$h3("MitigationHubs - ",
                                             tags$a(href="https://mitigationhubs.github.io/", "mitigationhubs.github.io"))
                            ),
                            tags$div(class="header", checked=NA,
                                     tags$h4("MitigationHubs bringt das Projekt #flattenthecurve in Eure Wohnzimmer! Welche Region dämmt außergewöhnlich gut die Corona-Pandemie ein? Und wieso? Der Kampf gegen das Virus ist eine wissenschaftlichen Aufgabe, welche die Beteilung der Bürger:innen benötigt. Lasst uns",
                                             tags$a(href="https://forms.gle/3Jd2hRYbJGRBZ42d6?hl=de", "kooperieren,"),
                                             "lasst uns positive Stories über Corona erzählen und daraus lernen!")
                            )
                        )
                    ),
                    column(
                        width = 4,
                        box(
                            title = "Landkreisauswahl", status = "primary", width = 12,
                            selectizeInput(
                                'fallzahlen_landkreis_selector', paste('Landkreissuche (Namen eintippen) | Maximale Anzahl:', nlk_max), 
                                choices = LK_set$Landkreis, selected = clks_selected,
                                multiple = TRUE, options = list(maxItems = nlk_max)
                            ),
                            actionButton("fallzahlen_plot_action1", "Zeig mir die Fallzahlen", icon = icon("calculator"))
                        ),
                        box(
                            title = "Darstellungsoptionen", status = "primary", width = 12,
                            checkboxInput("fallzahlen_checkbox_gr", "Wachstumsraten", T),
                            selectizeInput("fallzahlen_grd_selector", "Zeitraum zur Berechnung der Wachstumsraten", choices = c(3,7), selected = 7),
                            checkboxInput("fallzahlen_checkbox_ls", "Logarithmische Skala (Fallzahlen)", T),
                            checkboxInput("fallzahlen_checkbox_ta", "Gemeinsame Achsen", T),
                            actionButton("fallzahlen_plot_action3", "Änderungen anwenden", icon = icon("calculator"))
                        )
                    ),
                    column(
                        width = 4,
                        box(
                            title = "Gegenmaßnahmen", status = "primary", width = 12,
                            checkboxGroupInput('fallzahlen_checkbox_gm', 'Bisher gesammelte Maßnahmen (noch nicht für alle Landkreise verfügbar)',
                                               choiceNames = LK_meas_set$measure, choiceValues = LK_meas_set$measureId),
                            actionButton("fallzahlen_plot_action2", "Zeig mir die Gegenmaßnahmen", icon = icon("calculator")),
                            tags$div(class="header", checked=NA,
                                     tags$h4("Gegenmaßnahme melden?",
                                             tags$a(href="https://forms.gle/3Jd2hRYbJGRBZ42d6?hl=de", "Hier Mitmachen!"))
                            )
                        ),
                        box(
                            status = "primary", width = 12, title="Erklärung zu den Maßnahmenpaketen",
                            tags$div(class="text", checked=NA,
                                     tags$h5(tags$a(href="https://www.bundesregierung.de/breg-de/themen/coronavirus/beschluss-zu-corona-1730292",
                                     "1. Maßnahmenpaket"), "(12.03.2020): u.a. Stärkung der Intensiv- & Beatmungskapazitäten, 
                                             Absage von Veranstaltungen mit mehr als 1.000 Teilnehmer:innen, 
                                             Ankündigung von Liquiditätshilfen"),
                                     tags$h5(tags$a(href="https://www.bundesregierung.de/breg-de/themen/coronavirus/besprechung-der-bundeskanzlerin-mit-den-regierungschefinnen-und-regierungschefs-der-laender-1733248",
                                                    "2. Maßnahmenpaket"), "(22.03.2020): u.a. Abstandsregeln im öffentlichen Leben & Kontaktsperren, Schließung von Gastronomiebetrieben & weiteren Geschäften")
                            )
                        )
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
        lgrd <- isolate(input$fallzahlen_grd_selector)
        lta <- isolate(input$fallzahlen_checkbox_ta)
        clks <- isolate(input$fallzahlen_landkreis_selector)
        imes <- isolate(input$fallzahlen_checkbox_gm)
        
        IdsLandkreis <- LK_set %>% 
            filter(Landkreis %in% clks) %>% 
            .$IdLandkreis
        
        LK_meas_loc <- NULL
        if (!is.null(imes)) {
            imes <- as.numeric(imes)
            LK_meas_loc <- LK_meas %>% 
                filter(measureId %in% imes & IdLandkreis %in% IdsLandkreis)
            if (nrow(LK_meas_loc) == 0) LK_meas_loc <- NULL
        }
        
        if (lgr & lgrd == 3) {
            plt_fallzahlen(LK_dat_csum_proc %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_ma %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_gr3d %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_meas_loc,
                           log_scale = llog, shared_axes = lta)
        } else if (lgr & lgrd == 7) {
            plt_fallzahlen(LK_dat_csum_proc %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_ma %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_gr7d %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_meas_loc,
                           log_scale = llog, shared_axes = lta)
        } else {
            plt_fallzahlen(LK_dat_csum_proc %>% filter(IdLandkreis %in% IdsLandkreis),
                           LK_dat_csum_ma %>% filter(IdLandkreis %in% IdsLandkreis),
                           NULL,
                           LK_meas_loc,
                           log_scale = llog, shared_axes = lta)
        }
        
    })
    
    output$fallzahlen_publikationsdatum <- renderText(
        paste('Letzte Aktualisierung der Datenlage (RKI):', LK_time)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

