options(shiny.maxRequestSize = 2000*1024^2)
options(future.globals.maxSize = 1 * 1024 * 1024 ^ 2)

pacman::p_load(
  cd2030.pooled,
  shiny,
  shinydashboard,
  dplyr,
  readr,
  plotly,
  rlang,
  promises,
  purrr,
  future,
  shinyjs,
  htmltools,
  shinyFiles
  # update = TRUE
)

source('ui/content_body.R')
source('ui/content_header.R')
source('ui/download_button.R')
source('ui/report_button.R')
source('ui/directory-input.R')
source('ui/message_box.R')
source('ui/save_cache.R')
source('ui/download_report.R')

source('modules/introduction.R')
source('modules/setup.R')
source('modules/page_object_config.R')
source('modules/national_quality.R')
source('modules/regional_quality.R')
source('modules/national_coverage.R')
source('modules/regional_coverage.R')
source('modules/national_dropout.R')
source('modules/subnational_analysis.R')
source('modules/subnational_dropout.R')

ui <- dashboardPage(
  skin = 'green',
  header = dashboardHeader(title = 'cd2030 Pooled'),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem('Introduction', tabName = 'introduction', icon = icon('info-circle')),
      menuItem('Setup', tabName = 'setup', icon = icon('cogs')),
      menuItem('National Quality', tabName = 'national-quality', icon = icon('chart-line')),
      menuItem('Regional Quality', tabName = 'regional-quality', icon = icon('chart-area')),
      menuItem('National Coverage', tabName = 'national-coverage', icon = icon('globe')),
      menuItem('Regional Coverage', tabName = 'regional-coverage', icon = icon('map-marker-alt')),
      menuItem('National Dropout', tabName = 'national-dropout', icon = icon('user-times')),
      menuItem('Subnational Coverage Analysis', tabName = 'subnational-analysis', icon = icon('chart-bar')),
      menuItem('Subnational Dropout Rate', tabName = 'subnational-dropout', icon = icon('user-slash'))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'rmd-styles.css'),
      tags$link(rel = "stylesheet", type = 'text/css', href = "bootstrap-icons.css")
    ),
    tabItems(
      tabItem(tabName = 'introduction', introductionUI('introduction')),
      tabItem(tabName = 'setup', setupUI('setup')),
      tabItem(tabName = 'national-quality', nationalQualityUI('national-quality')),
      tabItem(tabName = 'regional-quality', regionalQualityUI('regional-quality')),
      tabItem(tabName = 'national-coverage', nationalCoverageUI('national-coverage')),
      tabItem(tabName = 'regional-coverage', regionalCoverageUI('regional-coverage')),
      tabItem(tabName = 'national-dropout', nationalDropoutUI('national-dropout')),
      tabItem(tabName = 'subnational-analysis', subnationalAnalysisUI('subnational-analysis')),
      tabItem(tabName = 'subnational-dropout', subnationalDropoutUI('subnational-dropout'))
    )
  )
)

server <- function(input, output, session) {

  cache <- setupServer('setup')

  # cache <- init_CacheConnection()$reactive()

  observeEvent(cache(), {
    req(cache())
    # data <- read_csv('data/Pooled_National_Coverage_Estimates.csv', show_col_types = FALSE) %>%
    #   mutate(across(where(is.numeric), ~ round(.x, 0)))
    # admin1 <- read_csv('data/Pooled_Admin1_Coverage_Estimates.csv', show_col_types = FALSE) %>%
    #   mutate(across(where(is.numeric), ~ round(.x, 0)))
    # dqa <- read_csv('data/Pooled_National_DQA_overall.csv', show_col_types = FALSE) %>%
    #   mutate(across(where(is.numeric), ~ round(.x, 0)))

    # cache()$set_national_estimates(data)
    # cache()$set_regional_estimates(admin1)
    # cache()$set_quality_data(dqa)

    shinyjs::delay(500, {
      updateHeader()
      shinyjs::addClass(selector = 'body', class = 'fixed')
    })
  })

  introductionServer('introduction')
  nationalQualityServer('national-quality', cache)
  regionalQualityServer('regional-quality', cache)
  nationalCoverageServer('national-coverage', cache)
  regionalCoverageServer('regional-coverage', cache)
  nationalDropoutServer('national-dropout', cache)
  subnationalAnalysisServer('subnational-analysis', cache)
  subnationalDropoutServer('subnational-dropout', cache)
  downloadReportServer('download_report', cache)
  saveCacheServe('save_cache', cache)

  updateHeader <- function() {
    header_title <- div(
      class = 'navbar-header',
      h4(HTML('Pooled Countdown Analysis'), class = 'navbar-brand')
    )

    # Dynamically update the header
    header <- htmltools::tagQuery(
      dashboardHeader(
        title = 'cd2030',
        saveCacheUI('save_cache'),
        downloadReportUI('download_report')
      )
    )
    header <- header$
      find('.navbar.navbar-static-top')$      # find the header right side
      append(header_title)$                     # inject dynamic content
      allTags()

    removeUI(selector = 'header.main-header', immediate = TRUE)

    # Replace the header in the UI
    insertUI(
      selector = 'body',
      where = 'afterBegin',
      ui = header
    )
  }
}

shinyApp(ui = ui, server = server)
