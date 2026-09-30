#!/usr/bin/env Rscript
# ==============================================================================
# GOALSTATSLAB | STAT LEADERS INTERACTIVE APP (POSIT CLOUD)
# ==============================================================================
pkgs <- c("shiny", "plotly", "DT", "htmltools", "dplyr", "tibble")
missing <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org")

library(shiny); library(plotly); library(DT); library(htmltools); library(dplyr); library(tibble)

db <- tibble::tribble(
  ~Player,           ~Team,         ~Pos,             ~Matches, ~Goals, ~xG,  ~Assists, ~Shots_p90, ~Pass_Acc, ~Photo,                                                                                ~Badge,
  "Erling Haaland",  "Man City",    "Delantero",      31,       27,     24.2, 5,        4.12,       76.5,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p223094.png", "https://resources.premierleague.com/premierleague/badges/50/t43.png",
  "Mohamed Salah",   "Liverpool",   "Extremo",        32,       22,     19.8, 12,       3.45,       78.2,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p118748.png", "https://resources.premierleague.com/premierleague/badges/50/t14.png",
  "Cole Palmer",     "Chelsea",     "Mediapunta",     34,       22,     17.4, 11,       3.10,       82.4,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p244851.png", "https://resources.premierleague.com/premierleague/badges/50/t8.png",
  "Alexander Isak",  "Newcastle",   "Delantero",      30,       21,     18.1, 2,        3.25,       75.1,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p219168.png", "https://resources.premierleague.com/premierleague/badges/50/t4.png",
  "Ollie Watkins",   "Aston Villa", "Delantero",      37,       19,     16.5, 13,       2.95,       73.0,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p178301.png", "https://resources.premierleague.com/premierleague/badges/50/t7.png",
  "Phil Foden",      "Man City",    "Extremo",        35,       19,     12.3, 8,        3.15,       88.3,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p209244.png", "https://resources.premierleague.com/premierleague/badges/50/t43.png",
  "Son Heung-min",   "Spurs",       "Delantero",      35,       17,     13.1, 10,       2.55,       83.1,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p85971.png",  "https://resources.premierleague.com/premierleague/badges/50/t6.png",
  "Bukayo Saka",     "Arsenal",     "Extremo",        35,       16,     14.2, 9,        3.05,       81.9,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p223340.png", "https://resources.premierleague.com/premierleague/badges/50/t3.png",
  "Bruno Fernandes", "Man Utd",     "Centrocampista", 35,       10,     8.2,  8,        2.75,       79.5,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p141746.png", "https://resources.premierleague.com/premierleague/badges/50/t1.png",
  "Martin Ødegaard", "Arsenal",     "Centrocampista", 35,       8,      7.9,  10,       2.25,       86.4,      "https://resources.premierleague.com/premierleague/photos/players/250x250/p184029.png", "https://resources.premierleague.com/premierleague/badges/50/t3.png"
) %>%
  mutate(
    xG_Diff    = round(Goals - xG, 2),
    Goals_p90  = round(Goals / Matches, 2),
    Efficiency = round(Goals / xG, 2)
  )

# ------------------------------------------------------------------------
# PERCENTILES (para el radar): 0-100 dentro del grupo de 10 jugadores
# ------------------------------------------------------------------------
pct <- function(x) round(rank(x, ties.method = "average") / length(x) * 100)

radar_df <- db %>%
  transmute(
    Player,
    `Goles / PJ`     = pct(Goals_p90),
    `Tiros p90`      = pct(Shots_p90),
    Asistencias      = pct(Assists),
    `% Pase`         = pct(Pass_Acc),
    `Efectividad xG` = pct(Efficiency)
  )

radar_cats_base <- c("Goles / PJ", "Tiros p90", "Asistencias", "% Pase", "Efectividad xG")

# ------------------------------------------------------------------------
# GRAFICO: Goles vs xG (resalta al jugador seleccionado)
# ------------------------------------------------------------------------
build_scatter <- function(selected_player) {
  max_val <- max(c(db$Goals, db$xG)) + 2
  fig <- plot_ly() %>%
    add_trace(x = c(0, max_val), y = c(0, max_val), type = "scatter", mode = "lines",
              line = list(color = "#94a3b8", dash = "dash", width = 1.5),
              hoverinfo = "skip", showlegend = FALSE)

  for (i in seq_len(nrow(db))) {
    r <- db[i, ]
    is_sel <- r$Player == selected_player
    fig <- fig %>% add_trace(
      x = r$xG, y = r$Goals, type = "scatter", mode = "markers+text",
      text = r$Player, textposition = "top center",
      textfont = list(color = ifelse(is_sel, "#0f172a", "#94a3b8"), size = ifelse(is_sel, 11, 9)),
      marker = list(
        size = ifelse(is_sel, 18, 12),
        color = ifelse(r$Goals >= r$xG, "#00a86b", "#ef4444"),
        line = list(color = ifelse(is_sel, "#0f172a", "#ffffff"), width = ifelse(is_sel, 2.5, 1.5))
      ),
      hoverinfo = "text",
      hovertext = paste0("<b>", r$Player, "</b> (", r$Team, ")<br>Goles: ", r$Goals,
                          " | xG: ", r$xG, "<br>Dif: ", ifelse(r$xG_Diff > 0, "+", ""), r$xG_Diff),
      showlegend = FALSE
    )
  }

  fig %>% layout(
    title = list(text = "<b>Eficiencia Ofensiva: Goles vs xG</b>", font = list(size = 13, color = "#0f172a")),
    xaxis = list(title = "Expected Goals (xG)", gridcolor = "#f1f5f9"),
    yaxis = list(title = "Goles Convertidos", gridcolor = "#f1f5f9"),
    paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
    margin = list(l = 30, r = 20, t = 40, b = 35), height = 340
  ) %>% config(displayModeBar = FALSE)
}

# ------------------------------------------------------------------------
# TABLA (todos los jugadores)
# ------------------------------------------------------------------------
dt_table <- db %>%
  arrange(desc(Goals)) %>%
  mutate(
    Foto = sprintf('<img src="%s" style="width:32px; height:32px; border-radius:50%%; object-fit:cover; background:#f8fafc;">', Photo),
    Club = sprintf('<span style="display:inline-flex; align-items:center; gap:6px;"><img src="%s" style="width:18px; height:18px;"> <b>%s</b></span>', Badge, Team),
    Jugador = paste0("<b>", Player, "</b> <span style='color:#94a3b8; font-size:11px;'>(" , Pos, ")</span>"),
    `Dif xG` = ifelse(xG_Diff >= 0, sprintf('<span style="color:#00a86b; font-weight:bold;">+%0.2f</span>', xG_Diff), sprintf('<span style="color:#ef4444; font-weight:bold;">%0.2f</span>', xG_Diff))
  ) %>%
  select(Foto, Jugador, Club, PJ = Matches, Goles = Goals, xG, `Dif xG`, Asist = Assists, `Tiros p90` = Shots_p90, `% Pase` = Pass_Acc) %>%
  datatable(escape = FALSE, rownames = FALSE, options = list(pageLength = 10, dom = "tp", ordering = TRUE))

# ------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$title("GoalStatsLab | Stat Leaders Hub"),
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f8fafc; padding: 16px; margin: 0; }
      .lab-header { display: flex; align-items: center; justify-content: space-between; border-bottom: 2px solid #e2e8f0; padding-bottom: 10px; margin-bottom: 16px; flex-wrap: wrap; gap: 10px; }
      .scout-card { background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%); color: #fff; border-radius: 12px; padding: 16px 20px; margin-bottom: 16px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 14px; }
      .kpi-tile { background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.12); padding: 6px 12px; border-radius: 8px; text-align: center; min-width: 65px; }
      .grid-2col { display: grid; grid-template-columns: 1fr 1fr; gap: 14px; margin-bottom: 16px; }
      .card-box { background: #fff; border: 1px solid #e2e8f0; border-radius: 10px; padding: 12px; }
      .player-select .selectize-input { border-radius: 8px; }
      @media(max-width: 768px) { .grid-2col { grid-template-columns: 1fr; } }
    "))
  ),

  div(class = "lab-header",
      div(style = "display: flex; align-items: center; gap: 10px;",
          div(style = "background: #00a86b; color: #fff; width: 34px; height: 34px; border-radius: 8px; display: grid; place-items: center; font-size: 18px; font-weight: bold;", "⚽"),
          div(h2("GoalStatsLab", span("| Stat Leaders Hub", style = "font-weight: 400; color: #64748b; font-size: 18px;"), style = "margin: 0; font-size: 20px;"),
              div("Tablero analítico generado en Posit Cloud", style = "font-size: 12px; color: #94a3b8;"))),
      div(style = "display:flex; align-items:center; gap:10px;",
          span("Temporada 2025/2026", style = "background: #ecfdf5; color: #065f46; border: 1px solid #a7f3d0; padding: 4px 10px; border-radius: 20px; font-size: 11px; font-weight: bold;"),
          div(class = "player-select", style = "min-width: 220px;",
              selectInput("player_sel", label = NULL, choices = db$Player, selected = "Erling Haaland", width = "100%")))
  ),

  uiOutput("scout_card"),

  div(class = "grid-2col",
      div(class = "card-box", plotlyOutput("radar_plot", height = "340px")),
      div(class = "card-box", plotlyOutput("scatter_plot", height = "340px"))),

  div(class = "card-box",
      h4("Clasificación de Líderes y Estadísticas Completas", style = "margin-top: 0; margin-bottom: 10px; font-size: 14px; font-weight: 700;"),
      dt_table)
)

# ------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------
server <- function(input, output, session) {

  selected_row <- reactive({
    req(input$player_sel)
    db %>% filter(Player == input$player_sel)
  })

  output$scout_card <- renderUI({
    r <- selected_row()
    eff_txt <- if (r$xG_Diff >= 0) {
      paste0("Eficiencia: +", r$xG_Diff, " xG (Definidor Clinico)")
    } else {
      paste0("Eficiencia: ", r$xG_Diff, " xG (Por debajo del xG)")
    }
    eff_color <- if (r$xG_Diff >= 0) "#34d399" else "#f87171"

    div(class = "scout-card",
        div(style = "display: flex; align-items: center; gap: 14px;",
            tags$img(src = r$Photo, style = "width: 70px; height: 70px; border-radius: 50%; object-fit: cover; border: 3px solid #00a86b; background: #0f172a;"),
            div(div(style = "display: flex; align-items: center; gap: 6px;",
                    h3(r$Player, style = "margin: 0; font-size: 20px;"),
                    tags$img(src = r$Badge, style = "width: 22px; height: 22px;")),
                div(paste0(r$Team, " - ", r$Pos, " - ", r$Matches, " PJ"), style = "color: #94a3b8; font-size: 13px; margin-top: 2px;"),
                div(eff_txt, style = paste0("color: ", eff_color, "; font-weight: bold; font-size: 12px; margin-top: 4px;")))),
        div(style = "display: flex; gap: 8px;",
            div(class = "kpi-tile", div("GOLES", style = "font-size: 10px; color: #94a3b8; font-weight: bold;"), div(r$Goals, style = "font-size: 18px; font-weight: bold; color: #38bdf8;")),
            div(class = "kpi-tile", div("xG", style = "font-size: 10px; color: #94a3b8; font-weight: bold;"), div(r$xG, style = "font-size: 18px; font-weight: bold; color: #fbbf24;")),
            div(class = "kpi-tile", div("ASIST", style = "font-size: 10px; color: #94a3b8; font-weight: bold;"), div(r$Assists, style = "font-size: 18px; font-weight: bold; color: #34d399;")),
            div(class = "kpi-tile", div("TIROS p90", style = "font-size: 10px; color: #94a3b8; font-weight: bold;"), div(r$Shots_p90, style = "font-size: 18px; font-weight: bold; color: #fff;"))))
  })

  output$radar_plot <- renderPlotly({
    row <- radar_df %>% filter(Player == input$player_sel)
    vals <- as.numeric(row[radar_cats_base])
    vals <- c(vals, vals[1])
    cats <- c(radar_cats_base, radar_cats_base[1])

    plot_ly(
      type = "scatterpolar", r = vals, theta = cats,
      fill = "toself", fillcolor = "rgba(0, 168, 107, 0.25)",
      line = list(color = "#00a86b", width = 2.5)
    ) %>% layout(
      title = list(text = paste0("<b>Radar Percentil: ", input$player_sel, "</b>"), font = list(size = 13, color = "#0f172a")),
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 100), color = "#94a3b8"),
                   angularaxis = list(color = "#475569")),
      paper_bgcolor = "#ffffff", margin = list(l = 30, r = 30, t = 40, b = 25)
    ) %>% config(displayModeBar = FALSE)
  })

  output$scatter_plot <- renderPlotly({
    build_scatter(input$player_sel)
  })
}

shinyApp(ui = ui, server = server)
