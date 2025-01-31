setupUI <- function(id) {
  ns <- NS(id)

  box(
    title = 'Upload Data',
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, h4(icon('upload'), 'Upload')),
      column(2, offset = 7, helpButtonUI(ns('upload_data')), align = 'right')
    ),
    fluidRow(
      column(
        12,
        directoryInput(
          ns('directory_select'),
          label = 'Upload Country data Data',
          buttonLabel = 'Browse or Drop...',
          accept = c('.csv', '.xls', 'xlsx')
        ),

        messageBoxUI(ns('feedback'))
      )
    ),
    fluidRow(
      column(4, downloadButtonUI(ns('download_data')))
    )
  )
}

setupServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback')

      observeEvent(input$directory_select, {
        req(input$directory_select)

        countries <- c('Ethiopia', 'Uganda', 'Tanzania', 'Rwanda', 'Kenya', 'Senegal', 'Burkina Faso',
                       'Cameroon', 'Cote_dIvoire', 'Chad', 'Ghana', 'CAR', 'DRC', 'Guinea',
                       'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique',
                       'Niger', 'Nigeria', 'Sierra Leone', 'Somalia', 'Zambia', 'Zimbabwe')

        uploaded_files <- input$directory_select #%>%
          # filter(name %in% countries)

        print(uploaded_files)

        # missing_files <- setdiff(required_files, uploaded_files$name)
        #
        # log_messages('Files uploaded')
        #
        # # Log missing files and exit if any are not found
        # if (length(missing_files) > 0) {
        #   log_messages(paste(
        #     log_messages(),
        #     paste('Missing required files:', paste(missing_files, collapse = ', ')),
        #     sep = '\n'
        #   ))
        #   return()  # Exit early if required files are missing
        # }
        #
        # uploaded_files %>%
        #   split(seq_len(nrow(.))) %>%
        #   walk(~ {
        #
        #     tryCatch({
        #       file_path <- .x$datapath
        #
        #       tryCatch(
        #         cd2030:::check_file_path(file_path),
        #         error = function(e) {
        #           new_log <- clean_error_message(e)
        #           log_messages(paste(log_messages(), new_log, sep = '\n'))
        #           return()
        #         }
        #       )
        #
        #       if (grepl('^all_', .x$name)) {
        #         survdata <- load_survey_data(file_path, country_iso())
        #         cache()$set_national_survey(survdata)
        #         new_log <- paste0('Loaded national survey data: '', .x$name, ''.')
        #       } else if (grepl('^gregion_', .x$name)) {
        #         gregion <- load_survey_data(file_path, country_iso(), admin_level = 'adminlevel_1')
        #         cache()$set_regional_survey(gregion)
        #         new_log <- paste0('Loaded regional survey data: '', .x$name, ''.')
        #       } else if (grepl('^area_', .x$name)) {
        #         area <- load_equity_data(file_path)
        #         cache()$set_area_survey(area)
        #         new_log <- paste0('Loaded area survey data: '', .x$name, ''.')
        #       } else if (grepl('^meduc_', .x$name)) {
        #         educ <- load_equity_data(file_path)
        #         cache()$set_education_survey(educ)
        #         new_log <- paste0('Loaded maternal education survey data: '', .x$name, ''.')
        #       } else if (grepl('^wiq_', .x$name)) {
        #         wiq <- load_equity_data(file_path)
        #         cache()$set_wiq_survey(wiq)
        #         new_log <- paste0('Loaded wealth index quintile survey data: '', .x$name, ''.')
        #       } else {
        #         new_log <- paste0('File '', .x$name, '' does not match any known pattern.')
        #       }
        #
        #       log_messages(paste(log_messages(), new_log, sep = '\n'))
        #     },
        #     error = function(e) {
        #       clean_message <- clean_error_message(e)
        #       new_log <- paste0('Error processing file '', .x$name, '': ', clean_message)
        #       log_messages(paste(log_messages(), new_log, sep = '\n'))
        #     })
        #   })
      })

      cache <- eventReactive(input$hfd_file, {
        req(input$hfd_file)

        file_path <- input$hfd_file$datapath
        file_name <- input$hfd_file$name
        file_type <- tools::file_ext(file_name)

        valid_types <- c('xls', 'xlsx', 'dta', 'rds')
        if (!file_type %in% valid_types) {
          messageBox$update_message('Upload failed: Unsupported file format.', 'error')
          return(NULL)
        }

        tryCatch({
          dt <- if (file_type %in% c('xls', 'xlsx')) {
            load_excel_data(file_path)
          } else if (file_type == 'dta') {
            load_data(file_path)
          } else {
            NULL
          }

          rds_path <- if (file_type == 'rds') {
            file_path
          } else {
            NULL
          }

          messageBox$update_message(
            paste('Upload successful: File', file_name, 'is ready.'),
            'success'
          )

          cache_instance <- init_CacheConnection(
            countdown_data = dt,
            rds_path = rds_path
          )$reactive()

          cache_instance()
        },
        error = function(e) {
          clean_message <- clean_error_message(e)
          messageBox$update_message(
            paste('Upload failed: ', clean_message),
            'error'
          )
          NULL
        })
      })

      downloadButtonServer(
        id = 'download_data',
        filename = 'master_dataset',
        extension = 'dta',
        content = function(file) {
          haven::write_dta(cache()$countdown_data, file)
        },
        data = cache,
        label = 'Download Master Dataset'
      )

      helpButtonServer(
        id = 'upload_data',
        title = 'Upload Data',
        md_file = 'load_data_upload_files.md'
      )

      return(cache)
    }
  )
}
