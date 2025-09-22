csv_files <- list.files(
  path = "Data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)

length(csv_files)

df <- read.csv(file.path("Data", "wingspan_vs_mass.csv"), stringsAsFactors = FALSE)

head(df, 5)

b_files <- list.files(
  path = "Data",
  pattern = "^b",
  full.names = TRUE,
  recursive = TRUE
)

for (f in b_files) {
  if (file.exists(f) && !dir.exists(f)) {
    cat("\n--- First line of:", f, "---\n")
    cat(readLines(f, n = 1, warn = FALSE), "\n")
  }
}

all_csv_recursive <- list.files(
  path = "Data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

for (f in all_csv_recursive) {
  if (file.exists(f) && !dir.exists(f)) {
    cat("\n--- First line of CSV:", f, "---\n")
    cat(readLines(f, n = 1, warn = FALSE), "\n")
  }
}
