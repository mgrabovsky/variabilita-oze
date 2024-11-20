data_file_path <- function(file_name) {
  glue::glue("../../data/{file_name}") 
}

load_dataset_entsoe <- function(country_code, year) {
  glue::glue("entsoe/{country_code}-{year}.csv") |> data_file_path() |>
    readr::read_csv() |>
    dplyr::rename(Onshore = `Wind onshore`, Offshore = `Wind offshore`) |>
    dplyr::group_by(Date = lubridate::floor_date(Date, unit = "hours")) |>
    # Downsample all to hourly frequency by averaging.
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x)),
      .groups = "drop"
    )
}

load_dataset_pecd <- function(source_type) {
  glue::glue("PECD-ERAA2023-{source_type}-2025.parquet") |> data_file_path() |>
    arrow::read_parquet()
}
