# cd2030.pooled

The Countdown to 2030 **Pooled** app: a Shiny app that combines several countries' saved datasets (the `.rds` files
the RMNCAH and Vaxx apps save) into one pooled file, then lets you explore it, compare countries and export tables.

It runs inside DataSuite (through the [countdown-analytics](https://github.com/damurka/countdown-analytics) extension)
or on its own in R. How this package fits with the others, and how a release reaches users:
[countdown-analytics/docs/ARCHITECTURE.md](https://github.com/damurka/countdown-analytics/blob/main/docs/ARCHITECTURE.md).

## Install and run

```r
install.packages("cd2030.pooled", repos = c("https://damurka.r-universe.dev", "https://cloud.r-project.org"))
cd2030.pooled::run_app()                                   # prints (runs) the app
cd2030.pooled::run_app(pre_loaded_dir = "saved-datasets/")
```

`run_app()` returns a Shiny app object; printing it or `shiny::runApp()` runs it. Its arguments default to the
environment variables DataSuite sets when it launches the app:

| Argument | Environment variable | |
| --- | --- | --- |
| `pre_loaded_dir` | `CDSUITE_SHINY_SELECTED_FILE` | a folder of saved datasets (`.rds`) to start from (DataSuite asks for a folder); `NA` for none |
| `language` | `CDSUITE_SHINY_LOCALE` | `"en"`, `"fr"` or `"pt"` |
| `app_name` | `CDSUITE_SHINY_NAME` | the name in the header |
| `app_version` | `CDSUITE_SHINY_VERSION` | the version in the header; defaults to this package's version |

In DataSuite nothing needs installing by hand: DataSuite installs this package when the extension is installed and
updates it when the extension is updated.

## What it does

| Page | |
| --- | --- |
| **Build pooled file** | 1. choose the data domain (RMNCAH or Immunization), what to include (everything, or the standard tables only) and the files or a folder; 2. load each (loaded or failed, with the reason); 3. review; 4. create one pooled `.rds` holding every dataset. |
| **Explore pooled data** | open a pooled file (the one just built, or another); a page per kind of dataset (parameters, overall score, indicator coverage, coverage, mortality, service utilization, ...) with its data and graphs; *Compare countries* and *All datasets* across the whole file; *Extract a piece*: chosen datasets, countries, years and columns. Exports as CSV, Excel or `.rds`. |

Unlike RMNCAH and Vaxx it doesn't use cd2030.core's `cd_app()` (there is no single dataset to load): it builds its own
two pages on the same shell (`cd_app_ui()`, `cd_shell_server()`), in the green "pooled" theme. It reads each country's
file with cd2030.core.

| File | What |
| --- | --- |
| `R/run_app.R` | `run_app()`: translations, the nav, the Build pages, the server |
| `R/pooled-datasets.R` | the tables a pooled file can hold and how each is pulled out of one country's dataset (plain R) |
| `R/pooled-build.R` | loading the files and combining them |
| `R/pooled-kinds.R` | the kinds of dataset Explore has a page for |
| `R/pooled-explore.R`, `R/pooled-compare*.R`, `R/pooled-charts.R` | the Explore pages, comparing countries, the charts |
| `R/pooled-export.R` | CSV, Excel and zip exports |
| `R/pooled-ui.R` | small UI pieces of this app |
| `inst/translation/translation.json` | the app's own texts |
| `inst/www/pooled.css` | the app's styles (served at `cd-pooled/`) |
| `app.R` | runs the app from this folder (see below); not part of the package |

## Develop

```r
shiny::runApp()          # from this folder: app.R loads the source with pkgload::load_all()
devtools::check()        # must stay at 0 errors, 0 warnings, 0 notes
```

`app.R` uses `pkgload::load_all()` when it finds `DESCRIPTION` next to it, otherwise the installed package. Keep `R/`
ASCII (`\uXXXX` escapes in strings); column names used unquoted in dplyr code go in `R/globals.R`.

When cd2030.core changes what a dataset holds, check `R/pooled-datasets.R`: it is where each table is pulled out.

## Release

1. Bump `Version:` in DESCRIPTION, add a NEWS.md entry, `devtools::check()` clean.
2. Commit, tag `vX.Y.Z`, push `main` and the tag. r-universe builds it within about an hour (instantly with its GitHub
   app installed).
3. DataSuite users get it at the next update of the countdown-analytics extension. If they must have this version,
   raise `package.version` for Pooled in the extension's package.json and release the extension.
