# Find all test instances available in the inst/extdata/test_instances folder
file_paths <- list.files("inst/extdata/test_instances", recursive = T, full.names = T) |> as.list()

# use `instance` function to instantiate as instance objects
test_instances <- lapply(file_paths, instance)

# extract names from files paths
names(test_instances) <- do.call(
  c,
  file_paths |>
    lapply(function (x) tools::file_path_sans_ext(x)) |>
    stringr::str_split("/") |>
    lapply(
      function(x) utils::tail(x, n = 2) |>
        rev() |>
        paste(collapse = "_")
    )
)

# order test_instances by names
test_instances <- test_instances[order(names(test_instances))]

usethis::use_data(test_instances, overwrite = TRUE)
