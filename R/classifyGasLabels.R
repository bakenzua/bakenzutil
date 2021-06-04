#' classifyGasLabels classifies raw blood gas sample type labels
#'
#' classifyGasLabels classifies vectors of raw blood gas sample type labels.
#' A vector of the following classifications is returned.
#'
#' * ARETERIAL
#' * VENOUS
#' * MIXED_VENOUS
#' * PRE_ECMO
#' * POST_ECMO
#' * UNKNOWN
#'
#' @export
#'
#' @param labels
#'
#' @return label_classifications
#'
classifyGasLabels <- function(labels) {

  exact_match_arterial_strings <- c('ART', 'ARTERIAL', 'ART.', 'ARTERIA', 'ARTERIAL BLOOD', 'BLOOD A',
                                    'A', 'ABG', 'AART', 'AERT', 'AET', 'AR', 'ARETRIAL', 'ARRT', 'RRA',
                                    'RADIAL ART', 'RIGHT RADIAL', 'FEMORAL ART', 'RFA', 'FEM ART', 'AT',
                                    'R RADIAL', 'BRACHIAL', 'ATERIAL', 'RADIAL', 'ART/NIV', 'ART/NIPPY',
                                    'R BRACHIAL', 'RT', 'ART/HFNC', 'ART/CPAP', 'AER', 'ART/NC', 'RT RADIAL',
                                    'ART#', 'right RADIAL', '100% TEST', '100%TEST', '100% ART',
                                    'ARTERIAL (100% TEST)', '100% TEST (ARTERIAL)', 'ART 100% TEST',
                                    '100% TEST ART', "I-STAT ART", "I-STAT-ART", "ART I-STAT", "ART ISTAT",
                                    "ISTART _ ART", "i-STAT CG8+ Art", "I-STAT CG4+ ART", "I STAT ART",
                                    'ART I_STAT', 'I-STAT /ART', 'i-STAT ARTERIAL', "ART- STAT",
                                    "I STAT - ART", "I- STAT ART",  "I-STAT", "ISTAT A\rT", "I-STAT ART"

  )


  #' exact_match_venous_strings is a vector of strings
  #'
  #' exact_match_venous_strings is a vector of strings for use in the exact matching of sample labels
  #' for classification of venous samples
  #'

  exact_match_venous_strings  <- c('VENOUS', 'VEN', 'VEN/RIJ CVC', 'V', 'VBG', 'VENUS', 'VEN/RIJ CVC', 'VEIN',
                                   'VEN/RIJ', 'VEN/LIJ CVC', 'CVC', 'VENOU', 'RIJ CVC', 'VEN/LIJ')

  #' exact_match_mixed_venous_strings is a vector of strings
  #'
  #' exact_match_mixed_venous_strings is a vector of strings for use in the exact matching of sample labels
  #' for classification of mixed venous samples
  #'

  exact_match_mixed_venous_strings <- c('MIX', 'MIXED VENOUS', 'MIXED VEN', 'MIX VEN', 'MIXED VEN', 'MIX VENOUS', 'MIXED',
                                        'MIX-VEN', 'MIXVEN', 'MIXVENOUS', 'MIXED V')

  #' exact_match_pre_ecmo_strings is a vector of strings
  #'
  #' exact_match_pre_ecmo_strings is a vector of strings for use in the exact matching of sample labels
  #' for classification of post ecmo membrane samples
  #'

  exact_match_pre_ecmo_strings <- c('PRE OXY', 'PRE_ECMO', 'PRE-OXY', 'ECMO (PRE)', 'ECMO PRE',
                                    'PREOXY', 'PPRE OXY', 'PRE-OXYGENATOR', 'PRE - OXY', 'PRE -OXY',
                                    'PRE OXY', 'PRE ECMO', 'PRE MEMBRANE', 'PRE OXYGENATOR', 'PREOXY')

  #' exact_match_post_ecmo_strings is a vector of strings
  #'
  #' exact_match_post_ecmo_strings is a vector of strings for use in the exact matching of sample labels
  #' for classification of post ecmo membrane samples
  #'

  exact_match_post_ecmo_strings <- c('POST_ECMO', 'POST OXY', 'POST-OXY', "POST OXY ABG",
                                     'ECMO POST OXY', 'ECMO (POST)', 'POSTOXY', 'POST OXY SAMPLE',
                                     'POST OXY.', 'POST-OXY ABG', 'POST - OXY', 'POST MEMBRANE',
                                     'POST OX', 'POST OXYGENATOR', 'POST-MEMBRANE', 'POST-O2 ECMO',
                                     'POST-OX', 'POST-OXY-', 'POST-OXY.', 'POST -OXY', 'POST OXY')

  #' arterial_time_regexp is a regular expression
  #'
  #' arterial_time_regexp is a regular expression for use in matching of sample labels
  #' for classification of arterial samples. Specifically it matches the various patterns
  #' of an arterial label suffixed with a time
  #'

  arterial_time_regexp <- '^(ART|ARTERIAL)[:space:]*(AT)*@*:*/*[:space:]*[:digit:]{1,2}[.:\\s]*[:digit:]{1,2}$'

  #' venous_time_regexp is a regular expression
  #'
  #' venous_time_regexp is a regular expression for use in matching of sample labels
  #' for classification of venous samples. Specifically it matches the various patterns
  #' of an venous label suffixed with a time
  #'

  venous_time_regexp <- '^(VEN|VENOUS)[:space:]*(AT)*@*:*/*[:space:]*[:digit:]{1,2}[.:\\s]*[:digit:]{1,2}$'


  # create a working vector of labels in upper case
  temp_labels <- stringr::str_to_upper(labels)

  # replace NA with 'UNKNOWN'
  temp_labels <- ifelse(
    is.na(temp_labels),
    'UNKNOWN',
    temp_labels
  )

  # strip whitespace
  temp_labels = stringr::str_trim(temp_labels)

  # create a vector of default 'unknown' values
  label_classifications <- rep('UNKNOWN', length(temp_labels))

  # Arterial
  label_classifications <- dplyr::if_else(
    temp_labels %in% exact_match_arterial_strings,
    'ARTERIAL',
    label_classifications
  )
  label_classifications <- dplyr::if_else(
    stringr::str_detect(
      temp_labels,
      arterial_time_regexp
    ),
    'ARTERIAL',
    label_classifications
  )

  # venous
  label_classifications <- dplyr::if_else(
    temp_labels %in% exact_match_venous_strings,
    'VENOUS',
    label_classifications
  )
  label_classifications <- dplyr::if_else(
    stringr::str_detect(
      temp_labels,
      venous_time_regexp
    ),
    'VENOUS',
    label_classifications
  )

  # Mixed
  label_classifications <- dplyr::if_else(
    temp_labels %in% exact_match_mixed_venous_strings,
    'MIXED_VENOUS',
    label_classifications
  )

  # pre ecmo
  label_classifications <- dplyr::if_else(
    temp_labels %in% exact_match_pre_ecmo_strings,
    'PRE_ECMO',
    label_classifications
  )

  # post ecmo
  label_classifications <- dplyr::if_else(
    temp_labels %in% exact_match_post_ecmo_strings,
    'POST_ECMO',
    label_classifications
  )

  return(label_classifications)

}
