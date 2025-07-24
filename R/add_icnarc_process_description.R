#' add_icnarc_process_description
#'
#' Given a dataframe with a column of ICNARC CMP codes, add_icnarc_process_description()
#' will add a new column containing the corresponding process descriptions
#'
#' @param main_df Dataframe to update
#' @param cmp_code_col Name of column containing ICNARC CMP codes
#' @param new_col_name Name of new column
#'
#' @returns The updated dataframe
#' @export
#'
#' @examples
add_icnarc_process_description <- function(main_df, cmp_code_col, new_col_name="") {

  lookups <- structure(
    list(
      description = c("Haemorrhage", "Infection", "Inflammation",
                      "Obstruction", "Trauma, perforation or rupture", "Tumour or malignancy",
                      "Vascular", "Trauma (or traumatic perforation or fistula)", "Non-traumatic perforation or fistula",
                      "Congenital or acquired deformity or abnormality", "Seizures",
                      "Trauma", "Trauma", "Non-traumatic aneurysm", "Coma or encephalopathy",
                      "Trauma (or traumatic perforation or rupture)", "Non-traumatic perforation, rupture or fistula",
                      "Hypertension", "inflammation (or oedema, fibrosis or inhalation)",
                      "Oedema, inflammation, fibrosis or inhalation", "Transplant (or related)",
                      "Collapse", "Trauma (or traumatic fistula)", "Non-traumatic fistula",
                      "Epidural or spinal drug administration", "Degeneration", "Trauma (or traumatic rupture)",
                      "Dissection or aneurysm", "Non-traumatic aneurysm, dissection or rupture",
                      "Shock or hypotension", "Non-traumatic rupture or fistula",
                      "Trauma (or traumatic dissection, rupture or fistula)",
                      "Non-traumatic aneurysm, dissection, rupture or fistula", "Failure",
                      "Non-traumatic aneurysm, perforation or rupture", "Trauma, rupture and instrumental damage",
                      "Non-traumatic rupture", "Over-activity (tachyarrhythmia or ectopy)",
                      "Under-activity (bradyarrhythmia)", "Trauma (or traumatic dissection, perforation or rupture)",
                      "Non-traumatic aneurysm, dissection, perforation or rupture",
                      "Trauma (or traumatic aneurysm, dissection, rupture or puncture)",
                      "Trauma, perforation, fistula or rupture", "Trauma (or traumatic rupture or fistula)",
                      "Trauma (or traumatic perforation, rupture or fistula)",
                      "Trauma (or traumatic perforation, laceration, rupture or fistula)",
                      "Non-traumatic aneurysm, perforation, rupture or fistula", "Trauma (or traumatic rupture or laceration)",
                      "Trauma (or trauma related fistula)", "Non-traumatic aneurysm or dissection",
                      "Upper airway or trachea", "Bronchi or airways", "Pulmonary vasculature",
                      "Lungs", "Pleura or mediastinum", "Spinal cord lesions causing respiratory failure",
                      "Chest wall or diaphragm disorders causing respiratory failure",
                      "Coronary arteries", "Pulmonary vasculature", "Myocardium and septum",
                      "Heart valves", "Thoracic aorta", "Abdominal aorta or iliac vessels",
                      "Splanchnic or renal vessels", "Neck or extracranial vessels",
                      "Limb or limb girdle vessels", "Great veins", "Mouth and pharynx",
                      "Abdominal wall or peritoneum", "Oesophagus", "Stomach", "Duodenum",
                      "Small bowel", "Large bowel, rectum or anus", "Liver or biliary tree",
                      "Spleen", "Pancreas", "Head and neck (extracranial)", "Brain, CSF, meninges or skull vault",
                      "Spinal cord", "Peripheral nervous system", "Kidney or ureter",
                      "Bladder or urethra", "Ovary, fallopian tubes, uterus or genitalia (non-obstetric)",
                      "Ovary, fallopian tubes, uterus or genitalia (obstetric)", "Testes, prostate or penis",
                      "Vertebral column", "Pelvis, long bones or joints", "Muscles or connective tissue",
                      "Skin", "Accidental intoxication, poisoning or medication event",
                      "Self-harm or self poisoning", "Non-traumatic aneurysm, dissection or fistula",
                      "Over-activity", "Inflammation (intrauterine death)", "Pregnancy",
                      "Tumour or malignancy (or hyperplasia)", "Under-activity", "Sex chromosome disorders",
                      "Contiguous gene syndromes", "Trisomy", "Chromosomal deletion syndromes",
                      "Diabetes mellitus", "Hyperthermia", "Hypothermia", "Hyperglycaemia",
                      "Hyperkalaemia", "Hypernatraemia", "Acidaemia", "Hypocalcaemia",
                      "Hypoglycaemia", "Hypokalaemia", "Hyponatraemia", "Alcohol related disorders",
                      "Alkalaemia", "Hypercalcaemia", "Fluid overload", "Envenomation",
                      "Inborn errors of metabolism", "Obesity", "Starvation", "Haemolysis or thrombocytopaenia",
                      "Hypoplasia or dysplasia", "Non-traumatic", "Inflammation (immune-mediated)",
                      "Burns or hyperthermia", "Alcohol or drug dependence", "Affective disorder",
                      "Neurosis or personality disorder", "Schizophrenia", "Learning disability",
                      "Bronchi or lower airways", "Brain lesions causing respiratory failure",
                      "Great veins", "Pulmonary vasculature", "Uterine or ovarian vessels",
                      "Myocardium or cardiac chambers", "Abdominal aorta", "Iliac vessels",
                      "Splanchnic or renal vessels", "Neck or extracranial vessels",
                      "Limb or limb girdle vessels", "Mouth or pharynx", "Head, neck (extracranial) or eyes"),
      code = c("15", "27", "28", "30", "38", "39", "41", "56",
                   "57", "8", "33", "38", "56", "57", "7", "56", "57", "19", "28",
                   "31", "37", "6", "56", "57", "12", "9", "56", "11", "57", "35",
                   "57", "56", "57", "13", "57", "38", "57", "32", "40", "56", "57",
                   "56", "38", "56", "56", "56", "57", "56", "56", "57", "1", "2",
                   "3", "4", "5", "6", "7", "1", "10", "2", "3", "4", "5", "6",
                   "7", "8", "9", "1", "10", "2", "3", "4", "5", "6", "7", "8",
                   "9", "1", "2", "3", "4", "1", "2", "3", "4", "5", "1", "2", "3",
                   "1", "1", "34", "57", "32", "28", "55", "39", "40", "46", "47",
                   "53", "54", "10", "20", "26", "16", "17", "18", "2", "21", "22",
                   "23", "24", "3", "4", "42", "43", "44", "45", "29", "36", "14",
                   "25", "57", "28", "5", "48", "49", "50", "51", "52", "2", "8",
                   "10", "11", "12", "2", "5", "6", "7", "8", "9", "1", "1")
      ),
    row.names = c(NA, -144L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  if(new_col_name=="") {
    new_col_name = paste0(cmp_code_col, "_process")
  }
  # Extract the second digit (System) from the code
  system_digits <- sapply(strsplit(main_df[, cmp_code_col], "\\."), `[`, 4)

  # Convert to numeric for matching
  system_digits <- as.numeric(system_digits)


  # Merge with lookup table (assuming lookup_df has columns 'code' and 'description')
  main_df[, new_col_name] <- lookups$description[match(system_digits, lookups$code)]

  return(main_df)
}
