# Declares non-standard evaluation (NSE) column names 
# used in dplyr/tidyr verbs to suppress 'no visible binding' warnings 
# during R CMD check.
utils::globalVariables(c(
  "dataset_type",
  "uuid",
  "sample_category",
  "Key",
  "Value",
  "immediate_ancestor_ids",
  "Age",
  "Body Mass Index",
  "dataset_processing_category",
  "data_type",
  "data_value",
  "preferred_term",
  "units",
  "hubmap_id"
))