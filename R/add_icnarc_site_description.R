#' add_icnarc_site_description
#'
#' Given a dataframe with a column of ICNARC CMP codes, add_icnarc_site_description()
#' will add a new column containing the site descriptions
#'
#' @param main_df Dataframe to update
#' @param cmp_code_col Name of column containing ICNARC CMP codes
#' @param new_col_name Name of new column
#'
#' @returns The updated dataframe
#' @export
#'
#' @examples
add_icnarc_site_description <- function(main_df, cmp_code_col, new_col_name="") {

  lookups <- structure(
    list(
      description = c("Upper airway or trachea", "Brain lesions causing respiratory failure",
               "Bronchi or lower airways", "Pulmonary vasculature", "Lungs",
               "Pleura or mediastinum", "Spinal cord lesions causing respiratory failure",
               "Peripheral nervous system disorders causing respiratory failure",
               "Neuro-muscular junction disorders causing respiratory failure",
               "Chest wall or diaphragm disorders causing respiratory failure",
               "Coronary arteries", "Limb or limb girdle vessels", "Great veins",
               "Peripheral vasculature, shock or hypertension", "Pulmonary vasculature",
               "Uterine or ovarian vessels", "Abdominal aorta", "Iliac vessels",
               "Myocardium or cardiac chambers", "Pericardium, pericardial space or mediastinum",
               "Heart valves", "Conducting system or rhythm disturbances", "Thoracic aorta",
               "Abdominal aorta or iliac vessels", "Splanchnic or renal vessels",
               "Neck or extracranial vessels", "Mouth or pharynx", "Abdominal wall or peritoneum",
               "Oesophagus", "Stomach", "Duodenum", "Small bowel", "Large bowel, rectum or anus",
               "Liver or biliary tree", "Spleen", "Pancreas", "Head, neck (extracranial) or eyes",
               "Brain, CSF, meninges or skull vault", "Spinal cord", "Peripheral nervous system",
               "Neuro-muscular junction", "Respiratory", "Cardiovascular", "Gastrointestinal",
               "Neurological (including eyes)", "Genito-urinary", "Musculoskeletal",
               "Dermatological", "Poisoning", "Kidney or ureter", "Bladder or urethra",
               "Ovary, fallopian tubes, uterus or genitalia (non-obstetric)",
               "Ovary, fallopian tubes, uterus or genitalia (obstetric)", "Testes, prostate or penis",
               "Thyroid", "Chromosomal abnormalities", "Pituitary", "Adrenal",
               "Endocrine pancreas", "Parathyroids", "Ovaries", "Thermoregulation",
               "Body fluids or tissues", "Body composition", "Body composition",
               "Blood", "Marrow", "Vertebral column", "Pelvis, long bones or joints",
               "Muscles or connective tissue", "Skin", "Psychiatric"),
      code = c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9", "1", "10", "11",
               "12", "13", "14", "15", "16", "2", "3", "4", "5", "6", "7", "8",
               "9", "1", "10", "2", "3", "4", "5", "6", "7", "8", "9", "1",
               "2", "3", "4", "5", "1", "2", "3", "4", "5", "6", "7", "8", "1",
               "2", "3", "4", "5", "1", "11", "2", "3", "4", "5", "6", "7",
               "8", "9", "10", "1", "2", "1", "2", "3", "1", "1")
      ),
    row.names = c(NA, -72L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  if(new_col_name=="") {
    new_col_name = paste0(cmp_code_col, "_site")
  }
  # Extract the second digit (System) from the code
  system_digits <- sapply(strsplit(main_df[, cmp_code_col], "\\."), `[`, 3)

  # Convert to numeric for matching
  system_digits <- as.numeric(system_digits)


  # Merge with lookup table
  main_df[, new_col_name] <- lookups$description[match(system_digits, lookups$code)]

  return(main_df)
}
