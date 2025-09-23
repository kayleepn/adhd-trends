library(opencodecounts)
library(here)
library(readr)

# Load codelist
adhd_snomed_codelist <- get_codelist(
  "https://www.opencodelists.org/codelist/user/kayleepn/snomed-adhd-test/2586ce25/"
)

# Write codelist
write_csv(adhd_snomed_codelist, here("codelists", "adhd_codelist.csv"))
