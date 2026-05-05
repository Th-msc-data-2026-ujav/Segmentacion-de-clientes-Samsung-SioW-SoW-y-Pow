readr::write_csv(
  tibble::tibble(response_id = train_ids),
  "train_ids.csv"
)

readr::write_csv(
  tibble::tibble(response_id = test_ids),
  "test_ids.csv"
)