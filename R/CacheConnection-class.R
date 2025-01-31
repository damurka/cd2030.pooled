#' Create a CacheConnection
#'
#' Constructor function for objects of class [CacheConnection].
#'
#' @param rds_path Path to the RDS file (NULL for in-memory only).
#'
#' @return An object of class [CacheConnection]
#' @export
init_CacheConnection <- function(rds_path = NULL) {
  CacheConnection$new(rds_path = rds_path)
}

#' Print Notes for a Specific Page and Object
#'
#' This function retrieves notes from the cache for a given `page_id` and `object_id`,
#' filters them for those marked as `include_in_report`, and prints them to the console.
#' If no notes are found, it prompts the user to enter notes for the given `page_id` and
#' `object_id`, optionally including additional parameters.
#'
#' @param cache An object (e.g., of class `CacheConnection`) that manages cached
#'   data, including notes.
#' @param page_id A character string specifying the page identifier for which to
#'   retrieve notes.
#' @param object_id An optional character string specifying the object identifier
#'   within the page. Defaults to `NULL`.
#' @param parameters An optional named list of additional parameters to filter or
#'   describe the notes.
#'
#' @return None. The function is used for its side effects (printing notes or
#'   prompting the user).
#'
#' @keywords internal
print_notes <- function(cache, page_id, object_id = NULL, parameters = NULL) {
  include_in_report = note = NULL

  # Retrieve the notes from the cache
  page_notes <- cache$get_notes(page_id, object_id, parameters) %>%
    filter(include_in_report == TRUE, !is.null(note))

  # Check if there are any notes
  if (nrow(page_notes) > 0) {
    page_notes %>%
      pull(note) %>%
      walk(~ cat(paste0(.x, '\n\n')))
  } else {
    # Format the parameters for the message
    parameters_str <- if (!is.null(parameters)) {
      paste0(" (parameters: ", paste(names(parameters), parameters, sep = " = ", collapse = ", "), ")")
    } else {
      ""
    }

    cat(paste0("**--- Enter notes related to ", page_id, " ", object_id, parameters_str, ". ---**\n\n"))
  }
}

#' CacheConnection R6 Class
#'
#' An R6 class for handling RDS caching for data analysis and report generation.
#'
#' @keywords internal
CacheConnection <- R6::R6Class(
  'CacheConnection',
  public = list(

    #' Initialize the CacheConnection class
    #'
    #' @param rds_path Path to the RDS file (NULL for in-memory only).
    initialize = function(rds_path = NULL) {

      # Initialize in-memory data using the template
      private$.in_memory_data <- private$.data_template
      private$.in_memory_data$rds_path <- rds_path

      if (!is.null(rds_path)) {
        self$load_from_disk()
      }
    },

    #' Load data from RDS file
    #' @return None
    load_from_disk = function() {
      check_file_path(private$.in_memory_data$rds_path)

      loaded_data <- readRDS(private$.in_memory_data$rds_path)
      required_fields <- names(private$.data_template)
      missing_fields <- setdiff(required_fields, names(loaded_data))
      if (length(missing_fields) > 0) {
        # cd_abort(c('x' = paste('The following required fields are missing in the RDS file:',
        #                        paste(missing_fields, collapse = ', '))))
        stop(paste('The following required fields are missing in the RDS file:', paste(missing_fields, collapse = ', ')))
      }
      private$.in_memory_data <- loaded_data
    },

    #' Save data to disk (only if changed and RDS path is not NULL)
    #' @return None
    save_to_disk = function() {
      if (private$.has_changed && !is.null(private$.in_memory_data$rds_path)) { # Key change here
        saveRDS(private$.in_memory_data, private$.in_memory_data$rds_path)
        private$.has_changed <- FALSE
      }
    },

    append_page_note = function(page_id, object_id, note, parameters = list(), include_in_report = FALSE, include_plot_table = FALSE, single_entry = FALSE) {
      # Validate inputs
      stopifnot(is.character(page_id), is.character(object_id), is.character(note))
      stopifnot(is.list(parameters), is.logical(include_in_report))

      # Create a new note
      new_note <- tibble(
        page_id = page_id,
        object_id = object_id,
        note = note,
        parameters = list(parameters),   # Wrap in list for storage
        include_in_report = include_in_report,  # Single logical value
        include_plot_table = include_plot_table
      )

      if (single_entry) {
        filtered_notes <- private$.in_memory_data$page_notes %>%
          filter(!(page_id == !!page_id & object_id == !!object_id))
      } else {
        filtered_notes <- private$.in_memory_data$page_notes %>%
          filter(
            !(page_id == !!page_id & object_id == !!object_id &
                map_lgl(parameters, ~ identical(.x, !!parameters)))
          )
      }

      # Append the new note to the page_notes tibble
      private$.in_memory_data$page_notes <- bind_rows(filtered_notes, new_note)

      # Mark data as changed and save to disk
      private$.has_changed <- TRUE
      self$save_to_disk()
    },

    get_notes = function(page_id, object_id = NULL, parameters = NULL) {
      private$.in_memory_data$page_notes %>%
        filter(if (is.null(!!object_id)) TRUE else object_id == !!object_id) %>%
        filter(if (is.null(!!parameters)) TRUE else map_lgl(parameters, ~ identical(.x, !!parameters)))
    },

    #' Reactive method for the CacheConnection object
    #' @return A Shiny reactive expression tracking changes
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$.reactiveDep)) {
        private$.reactiveDep <- reactiveValues()  # Initialize as an empty reactiveValues
        for (field_name in names(private$.data_template)) {
          private$.reactiveDep[[field_name]] <- 0  # Create a reactive tracker for each field
        }
      }
      reactive({
        private$depend_all()
        self
      })
    },

    set_cache_path = function(value) private$setter('rds_path', value, ~ !is.null(.x) && length(.x) > 0),
    set_national_estimates = function(value) private$setter('national_estimates', value, is.data.frame),
    set_regional_estimates = function(value) private$setter('regional_estimates', value, is.data.frame),
    set_quality_data = function(value) private$setter('quality_data', value, is.data.frame)
  ),

  active = list(
    cache_path = function(value) private$getter('rds_path', value),
    national_estimates = function(value) private$getter('national_estimates', value),
    regional_estimates = function(value) private$getter('regional_estimates', value),
    quality_data = function(value) private$getter('quality_data', value)
  ),

  private = list(
    .data_template = list(
      rds_path = NULL,
      national_estimates = NULL,
      regional_estimates = NULL,
      quality_data = NULL,
      page_notes = tibble::tibble(
        page_id = character(),
        object_id = character(),
        note = character(),
        parameters = list(),
        include_in_report = logical(),
        include_plot_table = logical()
      )
    ),
    .in_memory_data = NULL,
    .adjusted_data = NULL,
    .has_changed = FALSE,
    .reactiveDep = NULL,
    #' Update a field (with change tracking)
    update_field = function(field_name, value) {
      if (!identical(private$.in_memory_data[[field_name]], value)) {
        private$.in_memory_data[[field_name]] <<- value
        private$.has_changed <<- TRUE
        private$trigger(field_name)
      }
    },
    getter = function(field_name, value) {
      if (is_missing(value)) {
        private$depend(field_name)
        return(private$.in_memory_data[[field_name]])
      }

      # cd_abort(c('x' = '{.field field_name} is readonly'))
      stop(paste0(field_name, ' is readonly'))
    },

    setter = function(field_name, value, validation_exp = NULL) {
      check_required(field_name)
      check_required(value)

      validate_fn <- if (!is.null(validation_exp)) {
        if (rlang::is_formula(validation_exp)) {
          rlang::as_function(validation_exp)
        } else if (rlang::is_function(validation_exp)) {
          validation_exp
        } else {
          # cd_abort(c('x' = "{.arg validation} must be a function or a formula."))
          stop(paste0(validation, ' must be a function or a formula.'))
        }
      } else {
        function(x) TRUE
      }
      if (!validate_fn(value)) {
        # cd_abort(c('x' = 'Invalid value for field {.field field_name}.'))
        stop(paste0('Invalid value for field ', field_name))
      }
      private$update_field(field_name, value)
    },
    depend = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]]
      }
      invisible()
    },
    trigger = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]] <- isolate(private$.reactiveDep[[field_name]] + 1)
      }
    },
    depend_all = function() {
      for (field_name in names(private$.data_template)) {
        private$depend(field_name)  # Establish dependency for each field
      }
      invisible()
    }
  )
)
