# Two more Explore pooled data pages that work across the whole file, not one kind of table:
#   Compare countries  continuum of care, source triangulation, level and change, trends (pooled-compare.R draws them)
#   All datasets       a table and summary for any dataset in the file, including those with no page of their own
# Both take the same arguments as pooled_kind_server(): the open file, its name, and the filters shared across pages.

# ---- shared pieces ---------------------------------------------------------------------------------------------------------

pooled_empty_state <- function(ns, what) {
  div(
    class = "pooled-empty",
    tags$i(class = "fa fa-folder-open pooled-empty__icon"),
    tags$div(class = "pooled-empty__title", "No pooled file is open"),
    tags$div(class = "pooled-muted", "Open a pooled file to see ", what, "."),
    pooled_btn(ns("goto_open"), "Open a file", icon = "folder-open", primary = TRUE)
  )
}

pooled_file_bar <- function(ns, name, pooled, just_built, extra = NULL) {
  div(
    class = "pooled-filebar",
    pooled_file_chip(name, nrow(pooled$countries), length(pooled$datasets), just_built),
    pooled_btn(ns("goto_open2"), "Change file", icon = "folder-open"),
    div(style = "flex-grow: 1;"),
    extra
  )
}

# The Countries and Years chips every page shares: drawn from the shared value when the page is shown, written back when
# changed there. Call inside a module server.
pooled_shared_chips <- function(input, output, ns, pooled, shared, is_active, i18n) {
  output$countries_ui <- shiny.react::renderReact({
    is_active()
    cd_chip_multi(ns("countries"), "Countries", options = cd_plain_options(pooled()$countries$country),
                  selected = isolate(shared$countries()), i18n = i18n, all_label = "All countries")
  })
  output$years_ui <- shiny.react::renderReact({
    is_active()
    cd_chip_multi(ns("years"), "Years", options = cd_plain_options(pooled_all_years(pooled()$datasets)), selected = isolate(shared$years()),
                  i18n = i18n, all_label = "All years")
  })
  observeEvent(input$countries, if (is_active()) shared$countries(setdiff(as.character(input$countries), "")), ignoreInit = TRUE)
  observeEvent(input$years, if (is_active()) shared$years(setdiff(as.character(input$years), "")), ignoreInit = TRUE)
}

# ---- Compare countries -----------------------------------------------------------------------------------------------------

POOLED_COMPARE_TABS <- list(care = "Continuum of care", tri = "Source triangulation", change = "Level and change", trends = "Trends over time")
POOLED_COMPARE_BLURB <- list(
  care = "Latest facility coverage. One row per country, one column for each step from the first antenatal visit to the vaccines. Darker is higher, and every cell shows its number.",
  tri = "Do the sources agree? Latest facility coverage, the latest survey and WUENIC for each country. A long line between the shapes means the sources disagree. A missing shape means that source has no value.",
  change = "Where countries are, and where they are heading. Each dot is a country: how much it covers now, and how far that moved over the period. The dashed line is the median level; labels mark the countries furthest from the middle.",
  trends = "How the middle of the group moves. For each indicator, the median across countries and the range the middle half of them fall in."
)

pooled_compare_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "output.loaded", ns = ns,
      cd_filter_bar(shiny.react::reactOutput(ns("countries_ui")), shiny.react::reactOutput(ns("years_ui")), shiny.react::reactOutput(ns("indicators_ui")), i18n = i18n)
    ),
    pooled_page_header("Compare countries", "Put the countries in the file side by side, across indicators, sources and years.", eyebrow = "Explore pooled data"),
    cd_page_content(uiOutput(ns("body")))
  )
}

pooled_compare_server <- function(id, pooled, file_name, just_built, shared, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_active <- reactive(identical(shared$tab(), "explore_compare"))
    loaded <- reactive(!is.null(pooled()))
    output$loaded <- reactive(loaded())
    outputOptions(output, "loaded", suspendWhenHidden = FALSE)
    pooled_shared_chips(input, output, ns, pooled, shared, is_active, i18n)

    tab <- reactiveVal("care")
    for (k in names(POOLED_COMPARE_TABS)) local({
      key <- k
      observeEvent(input[[paste0("tab_", key)]], tab(key))
    })

    datasets <- reactive(pooled()$datasets)
    cov <- reactive({
      d <- datasets()[["Coverage - National"]]
      if (is.null(d)) NULL else pooled_filter(d, shared$countries(), NULL)
    })
    available <- reactive(pooled_cov_indicators(cov()))
    output$indicators_ui <- shiny.react::renderReact({
      avail <- available()
      opts <- lapply(avail, function(a) list(key = a, text = pooled_label_for(a)))
      cd_chip_multi(ns("indicators"), "Indicators", options = opts, i18n = i18n, all_label = "All indicators")
    })
    indicators <- reactive({
      pick <- intersect(available(), setdiff(as.character(input$indicators), ""))
      if (length(pick)) pick else available()
    })
    years <- reactive(shared$years())

    output$body <- renderUI({
      if (!loaded()) return(pooled_empty_state(ns, "a comparison of its countries"))
      p <- pooled()
      if (is.null(cov()) || !length(available())) {
        return(tagList(pooled_file_bar(ns, file_name(), p, just_built()),
                       pooled_banner("info", "This file has no facility coverage to compare", "Compare countries needs the Coverage - National table. Build the file with everything included, or with the standard tables.")))
      }
      wuenic <- !is.null(datasets()[["WUENIC Estimates"]])
      tagList(
        pooled_file_bar(ns, file_name(), p, just_built()),
        cd_tab_strip(ns, POOLED_COMPARE_TABS, tab()),
        div(class = "pooled-note", style = "margin-top: 12px;", POOLED_COMPARE_BLURB[[tab()]]),
        switch(tab(),
          care = pooled_graph_card(paste0("Continuum of care, ", pooled_view_year(cov(), years(), indicators())), "Facility coverage, sorted by the average across the indicators.",
                                   cd_plot_ui(ns("g_care")), 2),
          tri = tagList(
            if (!wuenic) pooled_banner("info", "No WUENIC estimates in this file", "The plots show facility and survey only. Build the file with everything included to add WUENIC."),
            div(class = "pooled-grid",
                pooled_graph_card("DTP3 / Penta3 coverage", "Each shape is one source.", cd_plot_ui(ns("g_tri_a"))),
                pooled_graph_card("MCV1 / Measles 1 coverage", "Each shape is one source.", cd_plot_ui(ns("g_tri_b"))))
          ),
          change = div(class = "pooled-grid", lapply(seq_along(quad_indicators()), function(i) {
            ind <- quad_indicators()[[i]]
            pooled_graph_card(pooled_label_for(ind), "Level now against change over the period.", cd_plot_ui(ns(paste0("g_q", i))))
          })),
          trends = pooled_graph_card("Facility coverage trends", "Median and interquartile range across the countries chosen.",
                                     cd_plot_ui(ns("g_band")), 2)
        )
      )
    })
    observeEvent(input$goto_open, cd_navigate_to(session, "explore_open"))
    observeEvent(input$goto_open2, cd_navigate_to(session, "explore_open"))

    quad_indicators <- reactive({
      pref <- intersect(c("cov_anc4", "cov_ideliv", "cov_instlivebirths", "cov_pnc48h", "cov_penta3", "cov_measles1"), indicators())
      head(if (length(pref) >= 2) pref else indicators(), 4)
    })

    # ---- graphs, each with the chart tools (pooled_plot_server()): what each draws is also its data download -----------
    about <- function(graph, ...) {
      extra <- list(...)
      function() list(kind = paste0("pooled_compare_", graph), options = c(list(indicators = indicators(), years = years()), lapply(extra, function(f) f())))
    }
    pooled_plot_server(
      "g_care", i18n,
      data = reactive(pooled_care_data(cov(), indicators(), pooled_view_year(cov(), years(), indicators()))),
      draw = function(d) pooled_plot_care(cov(), indicators(), pooled_view_year(cov(), years(), indicators())),
      filename = "pooled_continuum_of_care", about = about("care")
    )

    tri_data <- function(cov_col, param_col, wuenic_col) reactive({
      need <- intersect(c("Coverage - National", "Parameters", "WUENIC Estimates"), names(datasets()))
      ds <- lapply(datasets()[need], function(d) pooled_filter(d, shared$countries(), NULL))
      pooled_triangulation_data(ds, cov_col, param_col, wuenic_col, years()) %||% data.frame()
    })
    pooled_plot_server("g_tri_a", i18n, data = tri_data("cov_penta3", "penta3", "cov_penta3_wuenic"),
                       draw = function(d) pooled_plot_triangulation(d, NULL), filename = "pooled_triangulation_penta3", about = about("triangulation_penta3"))
    pooled_plot_server("g_tri_b", i18n, data = tri_data("cov_measles1", "measles1", "cov_measles1_wuenic"),
                       draw = function(d) pooled_plot_triangulation(d, NULL), filename = "pooled_triangulation_measles1", about = about("triangulation_measles1"))

    for (i in 1:4) local({
      k <- i
      indicator <- reactive({
        ind <- quad_indicators()[k]
        req(!is.na(ind))
        ind
      })
      pooled_plot_server(
        paste0("g_q", k), i18n,
        data = reactive(pooled_quadrant_data(cov(), indicator(), years()) %||% data.frame()),
        draw = function(d) pooled_plot_quadrant(d, NULL),
        filename = paste0("pooled_level_change_", k), about = about("level_change", indicator = indicator)
      )
    })
    pooled_plot_server("g_band", i18n, data = reactive(pooled_band_data(cov(), indicators(), years())),
                       draw = function(d) pooled_plot_band(cov(), indicators(), years()), filename = "pooled_trends", about = about("trends"))
  })
}

# "45 rows", or "45 rows, 3 of 5 countries" when some countries have nothing in that table.
pooled_rows_label <- function(df, n_countries) {
  n <- if ("country" %in% names(df)) dplyr::n_distinct(df$country) else n_countries
  paste0(format(nrow(df), big.mark = ","), " rows", if (n < n_countries) paste0(", ", n, " of ", n_countries, " countries") else "")
}

# ---- All datasets ---------------------------------------------------------------------------------------------------------

pooled_all_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "output.loaded", ns = ns,
      cd_filter_bar(shiny.react::reactOutput(ns("countries_ui")), shiny.react::reactOutput(ns("years_ui")), i18n = i18n)
    ),
    pooled_page_header("All datasets", "Everything found in the countries' files, including what has no page of its own.", eyebrow = "Explore pooled data"),
    cd_page_content(uiOutput(ns("body")))
  )
}

pooled_all_server <- function(id, pooled, file_name, just_built, shared, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_active <- reactive(identical(shared$tab(), "explore_all"))
    loaded <- reactive(!is.null(pooled()))
    output$loaded <- reactive(loaded())
    outputOptions(output, "loaded", suspendWhenHidden = FALSE)
    pooled_shared_chips(input, output, ns, pooled, shared, is_active, i18n)

    datasets <- reactive(pooled()$datasets)
    current <- reactive({
      nm <- names(datasets())
      x <- input$dataset
      if (!is.null(x) && length(x) == 1 && x %in% nm) x else nm[[1]]
    })
    dff <- reactive(pooled_filter(datasets()[[current()]], shared$countries(), shared$years()))

    output$body <- renderUI({
      if (!loaded()) return(pooled_empty_state(ns, "all of its datasets"))
      p <- pooled()
      groups <- pooled_dataset_groups(names(datasets()))
      strip <- pooled_strip(dff())
      list_ui <- lapply(names(groups), function(g) {
        div(
          class = "pooled-dsgroup",
          div(class = "pooled-dsgroup__title", g),
          lapply(groups[[g]], function(nm) {
            div(
              class = paste("pooled-ds", if (identical(nm, current())) "pooled-ds--active"),
              tags$a(href = "#", class = "pooled-ds__name", onclick = paste0(pooled_send(ns("dataset"), js_string(nm)), "; return false;"),
                     tags$span(class = "pooled-ds__label", nm), tags$span(class = "pooled-ds__rows", pooled_rows_label(datasets()[[nm]], nrow(p$countries))))
            )
          })
        )
      })
      tagList(
        pooled_file_bar(ns, file_name(), p, just_built()),
        div(
          class = "pooled-explore",
          cd_card(title = "Datasets", subtitle = paste(length(datasets()), "in this file."), i18n = i18n, div(class = "pooled-scroll", list_ui)),
          div(
            class = "pooled-stack", style = "min-width: 0;",
            div(class = "pooled-strip", style = "margin: 0;", lapply(names(strip), function(k) div(class = "pooled-strip__item", div(class = "pooled-strip__label", k), div(class = "pooled-strip__value", strip[[k]])))),
            div(
              class = "pooled-card",
              div(class = "pooled-card__head", style = "justify-content: space-between;",
                  div(tags$h3(current()), tags$div(class = "pooled-muted", paste(format(nrow(dff()), big.mark = ","), "rows"))),
                  div(class = "pooled-tools",
                      tags$a(id = ns("dl_csv"), class = "shiny-download-link cd-button", href = "", target = "_blank", download = NA, tags$i(class = "fa fa-file-csv"), " CSV"),
                      tags$a(id = ns("dl_xlsx"), class = "shiny-download-link cd-button", href = "", target = "_blank", download = NA, tags$i(class = "fa fa-file-excel"), " Excel"))),
              cd_table_spinner(reactableOutput(ns("table")), i18n = i18n)
            )
          )
        )
      )
    })
    observeEvent(input$goto_open, cd_navigate_to(session, "explore_open"))
    observeEvent(input$goto_open2, cd_navigate_to(session, "explore_open"))

    output$table <- renderReactable({
      reactable(dff(), searchable = TRUE, striped = TRUE, highlight = TRUE, compact = TRUE, defaultPageSize = 10, resizable = TRUE,
                defaultColDef = colDef(format = colFormat(digits = 2)))
    })
    output$dl_csv <- downloadHandler(
      filename = function() pooled_export_plan("csv", "view", current(), pooled()$domain)$filename,
      content = function(file) pooled_write_csv(dff(), file)
    )
    output$dl_xlsx <- downloadHandler(
      filename = function() pooled_export_plan("xlsx", "view", current(), pooled()$domain)$filename,
      content = function(file) pooled_write_xlsx(stats::setNames(list(dff()), current()), file)
    )
  })
}
