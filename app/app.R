# NFL pre-snap tendencies: a scouting app with honest sample sizes.
# Data: nflverse play-by-play (CC BY 4.0) and participation/FTN charting (CC BY-SA 4.0).
library(shiny)
library(bslib)
library(DT)
library(plotly)

A <- readRDS("data/app_data.rds")
SEASONS <- rev(A$seasons)
INK <- "#1d2733"
MUTED <- "#6b7480"
TEAM_COL <- "#c8102e"
LEAGUE_COL <- "#4b5563"

pct <- function(x, d = 0) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f%%"), 100 * x))
situation_order <- c("1st", "2nd 1-3", "2nd 4-6", "2nd 7-10", "2nd 11+", "3rd 1-3", "3rd 4-6",
                     "3rd 7-10", "3rd 11+", "4th 1-3", "4th 4-6", "4th 7-10", "4th 11+")

theme <- bs_theme(version = 5, bg = "#ffffff", fg = INK, primary = TEAM_COL,
                  base_font = font_collection("Inter", "Helvetica Neue", "Arial", "sans-serif")) |>
  bs_add_rules(".bslib-value-box .value-box-value { font-size: 1.6rem; }
                .bslib-value-box .value-box-title { font-size: 0.85rem; }
                .bslib-value-box { min-height: 0; }")

about_md <- "
**What this is.** Pre-snap run/pass tendencies for every NFL offense, 2019–2025, built so a coach
or analyst does not over-read small samples. Every rate is shown with its play count. Team rates are
*shrunk* toward the league in proportion to how little data supports them, and come with 95%
intervals. Cells with fewer than 30 plays are flagged.

**League expected** is what a league-wide model predicts for the same plays. It uses down, distance,
field position, clock, score, timeouts, shotgun/no-huddle and personnel, and it was trained only on
earlier seasons. So a team's deviation is its tendency *beyond* what the game situation implies.

**Cells.** A situation × personnel × alignment estimate combines the team's situation, personnel and
alignment tendencies. It is not the cell's own rate, so a 4-play cell mostly reflects the team's
broader habits. The self-scout list only uses cells with at least 15 plays. Intervals treat the league
model as known, so they are slightly too narrow.

**Pre-snap only.** Inputs are things a defense can see before the snap. Formation labels are not used
because the data source changed in 2023 and the labels are not comparable across sources.

**Descriptive vs predictive.** Scouting tables are fit on each full season (descriptive). The
predictability results come from held-out tests: league models are tested on seasons they never saw,
and team models predict each week using only earlier weeks.

**Data.** nflverse play-by-play (CC BY 4.0); participation from NFL Next Gen Stats (through 2022) and
FTN Data (2023 on), via nflverse (CC BY-SA 4.0). This app's data is released under CC BY-SA 4.0.
Not affiliated with or endorsed by the NFL, nflverse or FTN.

Code, methods and full results: [github.com/Andresperez397/nfl-presnap-predictability](https://github.com/Andresperez397/nfl-presnap-predictability)
"

ui <- page_navbar(
  title = "NFL Pre-Snap Tendencies",
  theme = theme,
  fillable = FALSE,
  sidebar = sidebar(
    width = 260,
    selectInput("team", "Offense", A$teams, selected = "KC"),
    selectInput("season", "Season", SEASONS, selected = SEASONS[1]),
    helpText(sprintf("Team rates are shrunk toward the league; cells with fewer than %d plays are flagged.",
                     A$small_n))
  ),
  nav_panel(
    "Scout",
    layout_columns(
      fill = FALSE,
      value_box("Plays", textOutput("vb_plays"), theme = "light"),
      value_box("Pass rate", textOutput("vb_pass"), theme = "light"),
      value_box("League expected, same plays", textOutput("vb_league"), theme = "light"),
      value_box("Tendency index", textOutput("vb_index"), theme = "light")
    ),
    radioButtons("view", NULL, inline = TRUE,
                 c("Down & distance" = "situation", "Personnel" = "personnel",
                   "Alignment" = "alignment")),
    card(full_screen = TRUE, card_header(textOutput("plot_title")), plotlyOutput("scout_plot", height = "430px")),
    card(card_header("Table (team = shrunk estimate with 95% interval)"), DTOutput("scout_table"))
  ),
  nav_panel(
    "Self-scout",
    card(
      card_header("What an opponent would learn: the biggest reliable departures from league norms"),
      p(class = "text-muted", "Situation × personnel × alignment cells with at least 15 plays, where the
        95% interval for the team's shrunk pass rate excludes the league expectation. Sorted by size of
        the departure."),
      uiOutput("self_scout")
    ),
    card(card_header("All cells"), DTOutput("cell_table"))
  ),
  nav_panel(
    "Predictability",
    layout_columns(
      fill = FALSE,
      value_box("Accuracy, unseen seasons", textOutput("vb_acc"), theme = "light"),
      value_box("Down & distance alone", textOutput("vb_acc_b1"), theme = "light"),
      value_box("Team tendencies add", textOutput("vb_team_gain"), theme = "light")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE, card_header(textOutput("index_title")), plotlyOutput("index_plot", height = "640px")),
      card(full_screen = TRUE, card_header("League model calibration (held-out season)"),
           plotlyOutput("calib_plot", height = "330px"), uiOutput("reliability_text"))
    )
  ),
  nav_panel(
    "Third down",
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Third-down tendencies by distance"), DTOutput("third_table")),
      card(card_header("Third-down targets (pass attempts with a listed receiver)"), DTOutput("target_table"))
    )
  ),
  nav_panel("About", card(markdown(about_md))),
  nav_spacer(),
  nav_item(tags$a("Code", href = "https://github.com/Andresperez397/nfl-presnap-predictability",
                  target = "_blank"))
)

server <- function(input, output, session) {
  tt <- reactive({
    A$tables[[paste(input$team, input$season)]]
  })
  idx_row <- reactive({
    A$index[A$index$team == input$team & A$index$season == as.integer(input$season), ]
  })

  output$vb_plays <- renderText(format(tt()$overall$n, big.mark = ","))
  output$vb_pass <- renderText(pct(tt()$overall$raw, 1))
  output$vb_league <- renderText(pct(tt()$overall$league, 1))
  output$vb_index <- renderText({
    r <- idx_row()
    if (!nrow(r)) return("n/a")
    sprintf("%+.1f ± %.1f", r$T, 1.96 * r$se)
  })

  view_data <- reactive({
    x <- tt()[[input$view]]
    if (input$view == "situation") x <- x[order(match(x$group, situation_order)), ]
    if (input$view == "personnel") x <- x[order(-x$n), ]
    x
  })
  output$plot_title <- renderText(sprintf("%s %s: pass rate by %s", input$team, input$season,
                                          c(situation = "down and distance", personnel = "personnel group",
                                            alignment = "QB alignment")[input$view]))
  output$scout_plot <- renderPlotly({
    x <- view_data()
    x$label <- sprintf("%s (n=%d)%s", x$group, x$n, ifelse(x$small, " *", ""))
    x$label <- factor(x$label, levels = rev(x$label))
    plot_ly(x, y = ~label) |>
      add_segments(x = ~lo, xend = ~hi, yend = ~label, line = list(color = TEAM_COL, width = 3),
                   name = "Team 95% interval", hoverinfo = "none") |>
      add_markers(x = ~team, marker = list(color = TEAM_COL, size = 11), name = "Team (shrunk)",
                  text = ~sprintf("Team %s [%s, %s]<br>Raw %s on %d plays", pct(team), pct(lo), pct(hi),
                                  pct(raw), n), hoverinfo = "text") |>
      add_markers(x = ~league, marker = list(color = LEAGUE_COL, size = 10, symbol = "line-ns-open",
                                             line = list(width = 3)),
                  name = "League expected", text = ~sprintf("League expected %s", pct(league)),
                  hoverinfo = "text") |>
      add_markers(x = ~raw, marker = list(color = "rgba(0,0,0,0)", size = 8,
                                          line = list(color = MUTED, width = 1.5)),
                  name = "Raw rate", text = ~sprintf("Raw %s (n=%d)", pct(raw), n), hoverinfo = "text") |>
      layout(xaxis = list(title = "Pass rate", tickformat = ".0%", range = c(0, 1.02)),
             yaxis = list(title = ""), legend = list(orientation = "h", y = -0.15),
             margin = list(l = 10)) |>
      config(displayModeBar = FALSE)
  })

  fmt_table <- function(x, first = "Group") {
    out <- data.frame(x$group, x$n, pct(x$raw), pct(x$league), pct(x$team),
                      sprintf("%s–%s", pct(x$lo), pct(x$hi)), sprintf("%+.0f pts", 100 * x$dev),
                      ifelse(x$small, "small sample", ""))
    names(out) <- c(first, "Plays", "Raw", "League expected", "Team (shrunk)", "95% interval",
                    "Team − league", "Flag")
    out
  }
  output$scout_table <- renderDT(datatable(fmt_table(view_data()), rownames = FALSE,
                                           options = list(dom = "t", pageLength = 50, ordering = FALSE)))

  # Cells with enough plays first, each group sorted by size of the departure.
  cells <- reactive({
    x <- tt()$cell
    x[order(x$n < 15, -abs(x$dev)), ]
  })
  output$self_scout <- renderUI({
    x <- cells()
    x <- x[x$n >= 15 & (x$lo > x$league | x$hi < x$league), ]
    if (!nrow(x)) return(p("No cell has a departure from league norms that the data can distinguish from noise."))
    x <- head(x, 8)
    tags$ol(lapply(seq_len(nrow(x)), function(i) {
      parts <- strsplit(x$group[i], " | ", fixed = TRUE)[[1]]
      tags$li(HTML(sprintf(
        "<b>%s, %s personnel, %s:</b> passed %s of %d plays. Shrunk estimate %s (%s–%s) vs league %s, so %s.",
        parts[1], parts[2], parts[3], pct(x$raw[i]), x$n[i], pct(x$team[i]), pct(x$lo[i]), pct(x$hi[i]),
        pct(x$league[i]), ifelse(x$dev[i] > 0, "expect <b>pass</b> more than the situation suggests",
                                  "expect <b>run</b> more than the situation suggests"))))
    }))
  })
  output$cell_table <- renderDT(datatable(fmt_table(cells(), "Situation | personnel | alignment"),
                                          rownames = FALSE, filter = "top",
                                          options = list(pageLength = 15, order = list())))

  q1 <- A$q1_pooled
  q2 <- A$q2_comparisons
  best4 <- q1[q1$model == A$league_model, ]
  output$vb_acc <- renderText(sprintf("%s (AUC %.2f)", pct(best4$accuracy, 1), best4$auc))
  output$vb_acc_b1 <- renderText(pct(q1$accuracy[q1$model == "B1 situation"], 1))
  output$vb_team_gain <- renderText({
    r <- q2[grepl("^team tendencies \\(shrunk\\)", q2$comparison), ]
    sprintf("%.1f millinats/play", r$logloss_reduction_millinats)
  })
  output$index_title <- renderText(sprintf("Tendency index by offense, %s (held-out weeks; ±95%%)", input$season))
  output$index_plot <- renderPlotly({
    x <- A$index[A$index$season == as.integer(input$season), ]
    x <- x[order(x$T), ]
    x$team <- factor(x$team, levels = x$team)
    x$col <- ifelse(as.character(x$team) == input$team, TEAM_COL, "#9aa3ad")
    plot_ly(x, y = ~team) |>
      add_segments(x = ~T - 1.96 * se, xend = ~T + 1.96 * se, yend = ~team,
                   line = list(color = "#c9ced4", width = 2), hoverinfo = "none", showlegend = FALSE) |>
      add_markers(x = ~T, marker = list(color = ~col, size = 9), showlegend = FALSE,
                  text = ~sprintf("%s: %+.1f ± %.1f millinats/play (%d plays)", team, T, 1.96 * se, plays),
                  hoverinfo = "text") |>
      layout(xaxis = list(title = "Log-loss gain from team tendencies (millinats per play)", zeroline = TRUE),
             yaxis = list(title = "", tickfont = list(size = 10))) |>
      config(displayModeBar = FALSE)
  })
  output$calib_plot <- renderPlotly({
    x <- A$calibration[A$calibration$season == as.integer(input$season), ]
    plot_ly(x, x = ~predicted, y = ~observed) |>
      add_lines(x = c(0, 1), y = c(0, 1), line = list(color = "#c9ced4", dash = "dash"),
                inherit = FALSE, showlegend = FALSE, hoverinfo = "none") |>
      add_markers(marker = list(color = TEAM_COL, size = 9), showlegend = FALSE,
                  text = ~sprintf("Predicted %s, observed %s (%s plays)", pct(predicted), pct(observed),
                                  format(n, big.mark = ",")), hoverinfo = "text") |>
      layout(xaxis = list(title = "Predicted pass probability", tickformat = ".0%", range = c(0, 1)),
             yaxis = list(title = "Observed pass rate", tickformat = ".0%", range = c(0, 1))) |>
      config(displayModeBar = FALSE)
  })
  output$reliability_text <- renderUI({
    r <- A$q2_reliability
    p(class = "text-muted small", sprintf(
      "Is the index a stable trait? Split-half reliability (odd vs even weeks) r = %.2f; same offense in consecutive seasons r = %.2f (n = %d). Index = average improvement in log loss when an offense's own earlier-week tendencies are added to the league model.",
      r$split_half_r, r$year_to_year_r, r$year_to_year_n))
  })

  output$third_table <- renderDT({
    x <- A$third[A$third$team == input$team & A$third$season == as.integer(input$season), ]
    x <- x[order(match(x$distance, c("1-3", "4-6", "7-10", "11+"))), ]
    out <- data.frame(Distance = x$distance, Plays = x$n, `Pass rate` = pct(x$pass_rate),
                      `League expected` = pct(x$league_expected), `EPA/play` = sprintf("%+.2f", x$epa),
                      `Success rate` = pct(x$success), check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
  output$target_table <- renderDT({
    x <- A$targets[A$targets$team == input$team & A$targets$season == as.integer(input$season), ]
    out <- data.frame(Receiver = x$receiver, Targets = x$targets, Share = pct(x$share),
                      `EPA/target` = sprintf("%+.2f", x$epa_per_target), check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
  })
}

shinyApp(ui, server)
