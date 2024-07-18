
hospitals <- c("Central Hospital","Military Hospital")

for (i in 1:length(hospitals)) {
  hospital <- hospitals[i]
  rmarkdown::render(
    input="report.Rmd",
    output_file = stringr::str_glue("report_{hospital}_{Sys.Date()}"),
    params=list(hospital=hospital)
  )
}

                  