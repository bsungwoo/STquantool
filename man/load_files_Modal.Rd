% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{load_files_Modal}
\alias{load_files_Modal}
\title{Modal to load csv or txt files}
\usage{
load_files_Modal(
  input,
  output,
  session,
  text = "csv or txt",
  text_for_purpose = "Will you load the ",
  text_for_add = " file?",
  text_input_name = "file_load_name",
  text_input_explain = "Name of the folder",
  file_save_name = "Data_1",
  radio_input_name = "radio_delim_type",
  radio_input_explain = "Delimiter",
  numeric_input_name1 = "col_num_to_rowname",
  numeric_input_explain1 = "Select column number to transfer to rownames",
  numeric_input_name2 = "row_num_to_skip",
  numeric_input_explain2 = "Select row numbers to skip",
  check_input_name1 = "file_load_header_check",
  check_input_name2 = "file_load_transpose_check",
  check_input_name3 = "file_load_shift_col_check",
  files_button_name = "choose_file_to_convert",
  files_button_explain = "Choose",
  files_button_text = "Select files to convert to 10X format (*.txt/*.csv)",
  action_button_name1 = "check_load",
  action_button_name2 = "ok_load",
  result_table = "table_check"
)
}
\description{
Modal to load csv or txt files
}
\keyword{internal}
