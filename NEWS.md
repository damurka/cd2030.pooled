# cd2030.pooled 2.0.4

* The report builder is Quire (datasuite.ui 0.4.0, quire 0.2.17): tables can be aligned (numbers and text apart).
* Opened on a folder, its files are added without the reactive-context error.
* Tables show their own loader.
* Requires cd2030.core 1.3.5, datasuite.ui 0.4.0 and quire 0.2.17.

# cd2030.pooled 2.0.3

* Portuguese: the app reads as Portuguese is written in Mozambique and Angola (European norm) instead of Brazilian Portuguese.

# cd2030.pooled 2.0.2

* The denominator options read "ANC1 population growth" and "Penta1 population growth" (`anc1derived`,
  `penta1derived`), the labels of cd2030.core's data dictionary. Requires cd2030.core 1.3.1.

# cd2030.pooled 2.0.1

Documentation only; no change to the app.

* A README: what the app does, installing and running it (`run_app()` arguments and the `CDSUITE_SHINY_*` variables),
  its files, developing and releasing it.

# cd2030.pooled 2.0.0

* First release as an installable package: `cd2030.pooled::run_app()` starts the app.
* Built on cd2030.core (>= 1.1.0) and datasuite.ui (>= 0.1.0); the shared Countdown pages, report builder and
  translations now come from those packages instead of copied code.
* R CMD check passes with no errors, warnings or notes.
