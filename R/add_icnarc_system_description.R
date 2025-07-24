#' add_icnarc_system_description
#'
#' Given a dataframe with a column of ICNARC CMP codes, add_icnarc_system_description
#' will add a new column containing the system labels
#'
#' @param main_df Dataframe to update
#' @param cmp_code_col Name of column containing ICNARC CMP codes
#' @param new_col_name Name of new column
#'
#' @returns The updated dataframe
#' @export
#'
#' @examples
add_icnarc_system_description <- function(main_df, cmp_code_col, new_col_name="") {

  lookups <- structure(
    list(
      description = c("Respiratory", "Cardiovascular", "Gastrointestinal",
                      "Neurological (including eyes)", "Trauma", "Poisoning",
                      "Genito-urinary", "Endocrine, Metabolic, Thermoregulation and Poisoning",
                      "Haematological/Immunological", "Musculoskeletal", "Dermatological",
                      "Psychiatric", "Trauma"),
      code = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")
    ),
    row.names = c(NA, -13L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  if(new_col_name=="") {
    new_col_name = paste0(cmp_code_col, "_system")
  }
  # Extract the second digit (System) from the code
  system_digits <- sapply(strsplit(main_df[, cmp_code_col], "\\."), `[`, 2)

  # Convert to numeric for matching
  system_digits <- as.numeric(system_digits)


  # Merge with lookup table (assuming lookup_df has columns 'code' and 'description')
  main_df[, new_col_name] <- lookups$description[match(system_digits, lookups$code)]

  return(main_df)
}
