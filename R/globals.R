# Declare global variables to avoid "no visible binding for global variable" note
utils::globalVariables(
  c(".data", "blanked_od", "mean_od", "mean_sd", "plate_dims", "write.table",
    "k_off", "k_on", "line_id", "sample_id")
)
