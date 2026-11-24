library(gbm)

rating_cols <- c(
  "Inflight_wifi_service",
  "Departure_Arrival_time_convenient",
  "Ease_of_Online_booking",
  "Gate_location",
  "Food_and_drink",
  "Online_boarding",
  "Seat_comfort",
  "Inflight_entertainment",
  "On_board_service",
  "Leg_room_service",
  "Baggage_handling",
  "Checkin_service",
  "Cleanliness"
)

raw_cols <- c(
  "Gender", "Customer_Type", "Age", "Type_of_Travel", "Class",
  "Flight_Distance", rating_cols, "Departure_Delay_in_Minutes"
)

resolve_model_path <- function(filename) {
  kandidati <- c(
    file.path("model", filename),
    file.path("..", "model", filename)
  )
  postojeci <- kandidati[file.exists(kandidati)]
  if (length(postojeci) > 0) {
    return(postojeci[[1]])
  }
  stop("Nije pronadjen fajl modela: ", filename)
}

prepare_passenger <- function(x) {
  x <- as.data.frame(x)
  
  fali <- setdiff(raw_cols, names(x))
  if (length(fali) > 0) {
    stop("Nedostaju kolone: ", paste(fali, collapse = ", "))
  }
  
  # Ocene moraju biti celi brojevi od 0 do 5 (0 = usluga nije ocenjena)
  ocena_matrica <- as.matrix(x[, rating_cols])
  if (any(is.na(ocena_matrica)) || any(!ocena_matrica %in% 0:5)) {
    stop("Ocene usluga moraju biti celi brojevi od 0 do 5.")
  }
  
  # Kasnjenje pri dolasku je opcionalno; ako fali ili je NA, koristi se kasnjenje pri polasku
  if (!"Arrival_Delay_in_Minutes" %in% names(x)) {
    x$Arrival_Delay_in_Minutes <- NA_real_
  }
  arrival <- ifelse(
    is.na(x$Arrival_Delay_in_Minutes),
    x$Departure_Delay_in_Minutes,
    x$Arrival_Delay_in_Minutes
  )
  
  out <- data.frame(
    Gender         = factor(x$Gender, levels = c("Female", "Male")),
    Customer_Type  = factor(x$Customer_Type, levels = c("Disloyal Customer", "Loyal Customer")),
    Type_of_Travel = factor(x$Type_of_Travel, levels = c("Business Travel", "Personal Travel")),
    Class          = factor(x$Class, levels = c("Eco", "Business", "Eco Plus")),
    Age                        = as.integer(x$Age),
    Flight_Distance            = as.integer(x$Flight_Distance),
    Departure_Delay_in_Minutes = as.integer(x$Departure_Delay_in_Minutes),
    Delay_Difference           = as.numeric(arrival - x$Departure_Delay_in_Minutes)
  )
  
  for (kol in rating_cols) {
    out[[kol]] <- as.integer(x[[kol]])
  }
  
  out$Not_Rated_Count <- as.numeric(rowSums(x[, rating_cols] == 0))
  
  if (any(out$Not_Rated_Count > 0)) {
    warning(
      "Neki putnici imaju neocenjene usluge (ocena 0). ",
      "Model je veoma osetljiv na takve slucajeve, pa rezultat tumaciti oprezno."
    )
  }
  
  if (anyNA(out[, c("Gender", "Customer_Type", "Type_of_Travel", "Class")])) {
    stop("Nepoznata vrednost u Gender / Customer_Type / Type_of_Travel / Class.")
  }
  
  out
}

predict_satisfaction <- function(x, model = NULL, prag = 0.5) {
  if (is.null(model)) {
    model <- readRDS(resolve_model_path("gbm_produkcija.rds"))
  }
  
  podaci <- prepare_passenger(x)
  prob <- predict(model, newdata = podaci, n.trees = model$n.trees, type = "response")
  
  data.frame(
    Verovatnoca_zadovoljstva = round(prob, 3),
    Predikcija = factor(
      ifelse(prob >= prag, "Satisfied", "Not Satisfied"),
      levels = c("Satisfied", "Not Satisfied")
    )
  )
}

predict_satisfaction_bez_ocena <- function(x, model = NULL, prag = 0.5) {
  if (is.null(model)) {
    model <- readRDS(resolve_model_path("gbm_produkcija_bez_ocena.rds"))
  }
  
  x <- as.data.frame(x)
  
  # prepare_passenger() trazi sve kolone ocena; za ovaj model se one ne koriste
  # i popunjavaju se neutralnom vrednoscu 3 pre zadrzavanja promenljivih modela
  for (kol in rating_cols) {
    if (!kol %in% names(x)) {
      x[[kol]] <- 3L
    }
  }
  
  podaci <- suppressWarnings(prepare_passenger(x))[, model$var.names, drop = FALSE]
  prob <- predict(model, newdata = podaci, n.trees = model$n.trees, type = "response")
  
  data.frame(
    Verovatnoca_zadovoljstva = round(prob, 3),
    Predikcija = factor(
      ifelse(prob >= prag, "Satisfied", "Not Satisfied"),
      levels = c("Satisfied", "Not Satisfied")
    )
  )
}

# --- Kratak primer poziva ---
if (sys.nframe() == 0) {
  primer_putnik <- data.frame(
    Gender                            = "Female",
    Customer_Type                     = "Loyal Customer",
    Age                               = 42,
    Type_of_Travel                    = "Business Travel",
    Class                             = "Business",
    Flight_Distance                   = 1500,
    Inflight_wifi_service             = 4,
    Departure_Arrival_time_convenient = 4,
    Ease_of_Online_booking            = 4,
    Gate_location                     = 4,
    Food_and_drink                    = 4,
    Online_boarding                   = 5,
    Seat_comfort                      = 4,
    Inflight_entertainment            = 4,
    On_board_service                  = 5,
    Leg_room_service                  = 4,
    Baggage_handling                  = 4,
    Checkin_service                   = 4,
    Cleanliness                       = 5,
    Departure_Delay_in_Minutes        = 5,
    Arrival_Delay_in_Minutes          = 5
  )
  
  print(predict_satisfaction(primer_putnik))
  print(predict_satisfaction_bez_ocena(primer_putnik))
}