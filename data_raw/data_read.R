
dna <- readr::read_csv("secret/T24868 - DNA Machine Learning.csv", 
                      col_names = FALSE)

# X14 is dupicate of X5

dna <- dna |> 
  dplyr::select(-X14)

# I don't know what X2 is!

dna <- dna |> 
  dplyr::select(-X2)

# X9 contains "NULL" strings

dna <- dna |> 
  dplyr::mutate(X9 = as.numeric(X9))

dna <- dna |> 
  dplyr::rename(ClientID = X1,
                AgeatAppointment = X3,
                ServiceTeamClassification = X4,
                RefToAppDays = X5,
                AppointmentType = X6,
                weekday_name = X7,
                GenderAtAppt = X8,
                IMDDecile = X9,
                Appointment_month_name = X10,
                Number_Previous_DNA = X11,
                Number_Previous_Appts = X12,
                DNA = X13) |> 
  dplyr::filter(GenderAtAppt %in% c("Female", "Male"),
         ServiceTeamClassification != "IDD") |> 
  dplyr::filter(!grepl("patient not present", 
                       AppointmentType, ignore.case = TRUE)) |> 
  dplyr::filter(!grepl("telephone", AppointmentType, ignore.case = TRUE)) |> 
  dplyr::mutate(AppointmentType = substr(AppointmentType, 1, 6))|>
  dplyr::mutate_if(is.character, as.factor) |> 
  na.omit() |>
  dplyr::group_by(ClientID) |>
  dplyr::slice(dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::select(-ClientID) |> 
  tibble::as_tibble(.name_repair = "universal")

dna |> 
  dplyr::group_by(DNA) |> 
  skimr::skim()

save(dna, file = "secret/dna.rda")
