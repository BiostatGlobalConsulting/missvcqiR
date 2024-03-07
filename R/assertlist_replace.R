#' This program mimics Stata's assertlist_replace program. This program is used after the user has reviewed the output of assertlist and entered corrected values to pull the corrected values into a new corrected dataset.
#'
#' @param originaldata Path to the dataset that assertlist was initially run on; assertlist_replace will create a new dataset by changing values in originaldata
#' @param excel The path to the Excel file that holds the assertlist_clean output with the user's replacement values
#' @param sheetname The name of the worksheet that holds the assertlist_clean output with the user's replacement values
#'
#' @return A dataset containing the user-specified replacement values
#'
#' @import dplyr
#' @import tidyselect
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import openxlsx
#'
#' @export

# assertlist_replace R version 1.00 - Biostat Global Consulting - 2023-11-02
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-11-02  1.00      Mia Yu          Original R package version
# *******************************************************************************

assertlist_replace <-
  function(originaldata, excel, sheetname = "checks") {

    dataset <- read.xlsx(excel, sheet = sheetname)
    summary_sheet <- read.xlsx(excel, sheet = "assertlist summary")
    id <-
      unlist(strsplit(summary_sheet$Variables.Provided.in.IDLIST.Option[1], ","))
    original <- vcqi_read(originaldata)
    dataset <- rename_with(
      dataset,
      ~ gsub(
        "Blank.Space.for.User.to.Provide.Correct.Value.of.Variable.",
        "correction",
        .x,
        fixed = TRUE
      ),
      .cols = starts_with("Blank")
    )
    dataset <- rename_with(
      dataset,
      ~ gsub("Variable.used.Assertion.Syntax.", "var", .x, fixed = TRUE),
      .cols = starts_with("Blank")
    )
    var_to_change <- dataset %>% select(starts_with("var"))
    correct_values <- dataset %>% select(starts_with("correction"))
    for (k in 1:nrow(dataset)){
      condition = ""

      for (p in seq_along(id)){
        colnum = which(names(dataset) == id[p])
        if (any(class(original[, colnum]) == "character")) {
          idvalue = paste0("'", dataset[k, colnum], "'")
        } else{
          idvalue = dataset[k, colnum]
        }
        condition = paste0(condition, " & original$", id[p], " == ", idvalue)
      } #end of p look

      condition = substring(condition, 4)

      rownum <- which(eval(parse_expr(condition)) == TRUE)
      for (m in 1:ncol(var_to_change)) {
        if (!is.na(var_to_change[k, m]) & var_to_change[k, m] != "") {
          colnum <- which(names(original) == var_to_change[k, m])

          orgvar <- get(var_to_change[k, m],original)
          numval <- as.numeric(correct_values[k, m]) %>% suppressWarnings()

          if (any("character" %in% class(orgvar)) & class(correct_values[k, m]) == "character"){
            #case one: original var character, open ended Q, replacement is character
            original[rownum, colnum] = correct_values[k, m]
          } else if ((any("numeric" %in% class(orgvar)) | any("double" %in% class(orgvar))) &
                     (any("numeric" %in% class(correct_values[k, m])) | any("double" %in% class(correct_values[k, m])))){
            #case two: original var numeric, replacement is numeric
            original[rownum, colnum] = correct_values[k, m]
          } else if ((any("numeric" %in% class(orgvar)) | any("double" %in% class(orgvar))) &
                     class(correct_values[k, m]) == "character" & is.na(numval)){
            #case three: original var numeric, but actually open ended Q, numeric due to all missing,
            #            replacement is character; but really we just have to hope...
            varconver <- rlang::sym(var_to_change[k, m])
            original <- original %>% mutate(!!varconver := as.character(!!varconver))
            original[rownum, colnum] = correct_values[k, m]
          } else if ((any("numeric" %in% class(orgvar)) | any("double" %in% class(orgvar))) &
                     class(correct_values[k, m]) == "character" & !is.na(numval)){
            #case four: original var numeric, replacement is character but can be converted to number...hopefully it's mc
            original[rownum, colnum] = numval
          } else if (any("character" %in% class(orgvar)) &
                     (any("numeric" %in% class(correct_values[k, m])) | any("double" %in% class(correct_values[k, m])))){
            original[rownum, colnum] = as.character(correct_values[k, m])
          }

        }
      } #end of m loop
    } #end of k loop
    return(original)
  }
