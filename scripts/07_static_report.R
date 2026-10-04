# Render the no-install HTML version of the app (reports/static/nfl-presnap-static.html).
rmarkdown::render("reports/static/static_report.Rmd", output_file = "nfl-presnap-static.html", quiet = TRUE)
