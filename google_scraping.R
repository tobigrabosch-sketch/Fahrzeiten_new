# run_distance_matrix.R
# Pakete laden/ggf. installieren
needed <- c("httr2", "jsonlite", "readr", "dplyr", "lubridate", "openxlsx")
to_install <- needed[!(needed %in% rownames(installed.packages()))]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
lapply(needed, library, character.only = TRUE)

# --- Einstellungen ---
# Koordinaten: Achtung: In Deutschland ist der Längengrad POSITIV (Ost) – also KEIN Minuszeichen!
origin      <- "51.150368,6.893793"   # kein Leerzeichen nach dem Komma ist auch ok
destination <- "51.105461,6.943102"

# Zeitzone und Zeitfenster (lokal)
tz_loc <- "Europe/Berlin"
hour_now_local <- hour(with_tz(now(tzone = "UTC"), tzone = tz_loc))

# Von 07 bis 22 Uhr messen (inkl. 22)
if (!(hour_now_local >= 7 & hour_now_local <= 22)) {
  message(sprintf("Aktuelle lokale Stunde (%s Uhr) liegt außerhalb 07–22. Lauf wird übersprungen.",
                  hour_now_local))
  quit(status = 0) # sauber beenden, damit der Workflow nicht fehlschlägt
}

# API Key aus Umgebungsvariable (in GitHub Actions als Secret setzen)
api_key <- Sys.getenv("GOOGLE_API_KEY")
if (api_key == "") stop("Umgebungsvariable GOOGLE_API_KEY ist nicht gesetzt.")

# --- Anfrage bauen ---
# Live-Verkehr: departure_time=now, traffic_model=best_guess
# Endpoint: https://maps.googleapis.com/maps/api/distancematrix/json
base_url <- "https://maps.googleapis.com/maps/api/distancematrix/json"

req <- request(base_url) |>
  req_url_query(
    origins       = gsub(" ", "", origin),
    destinations  = gsub(" ", "", destination),
    mode          = "driving",
    units         = "metric",
    departure_time= "now",
    traffic_model = "best_guess",
    key           = api_key
  )

resp <- req |> req_perform()
resp |> resp_check_status()

dat <- resp |> resp_body_json()

if (!identical(dat$status, "OK")) stop(paste("API Status nicht OK:", dat$status))

el <- dat$rows[[1]]$elements[[1]]
if (!identical(el$status, "OK")) stop(paste("Element-Status nicht OK:", el$status))

# Felder auslesen (ein paar können fehlen, daher vorsichtig)
distance_m         <- if (!is.null(el$distance$value)) el$distance$value else NA
distance_text      <- if (!is.null(el$distance$text))  el$distance$text  else NA
duration_s         <- if (!is.null(el$duration$value)) el$duration$value else NA
duration_text      <- if (!is.null(el$duration$text))  el$duration$text  else NA
duration_traffic_s <- if (!is.null(el$duration_in_traffic$value)) el$duration_in_traffic$value else NA
duration_traffic_t <- if (!is.null(el$duration_in_traffic$text))  el$duration_in_traffic$text  else NA

# Zeitstempel (lokal & UTC)
ts_utc  <- with_tz(now(tzone = "UTC"), tzone = "UTC")
ts_loc  <- with_tz(ts_utc, tz_loc)

row <- tibble::tibble(
  timestamp_utc        = format(ts_utc, "%Y-%m-%d %H:%M:%S"),
  timestamp_local      = format(ts_loc, "%Y-%m-%d %H:%M:%S"),
  timezone_local       = tz_loc,
  origin               = origin,
  destination          = destination,
  distance_m           = distance_m,
  distance_text        = distance_text,
  duration_s           = duration_s,
  duration_text        = duration_text,
  duration_in_traffic_s= duration_traffic_s,
  duration_in_traffic_text = duration_traffic_t,
  weekday_local        = wday(ts_loc, label = TRUE, abbr = FALSE, week_start = 1),
  hour_local           = hour(ts_loc)
)

# --- Bestehende Datei laden & anhängen ---
dir.create("data", showWarnings = FALSE)
csv_path  <- file.path("data", "travel_times.csv")
xlsx_path <- file.path("data", "travel_times.xlsx")

if (file.exists(csv_path)) {
  old <- readr::read_csv(csv_path, show_col_types = FALSE)

  # force types to character so they match `row`
  old$timestamp_utc   <- as.character(old$timestamp_utc)
  old$timestamp_local <- as.character(old$timestamp_local)

  out <- dplyr::bind_rows(old, row)
} else {
  out <- row
}

# Duplikate nach exakt gleichem UTC-Timestamp vermeiden
out <- out |> distinct(timestamp_utc, .keep_all = TRUE)

# Speichern
readr::write_csv(out, csv_path)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "data")
openxlsx::writeData(wb, "data", out)
openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

message("Messpunkt gespeichert: ", out$timestamp_local)





