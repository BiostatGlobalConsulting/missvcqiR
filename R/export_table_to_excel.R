#' Export analysis table(s) to Excel with styles
#'
#' @param indicator Name of the analysis indicator
#' @param tablename Name of the table
#' @param sheet Name of the sheet
#' @param brief Produce a brief table or not, default to be TRUE
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) with formatted output in an Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @import openxlsx
#' @import stringr
#' @import haven

# export_table_to_excel R version 1.06 - Biostat Global Consulting - 2023-02-07
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-07  1.00      Mia Yu          Original R version
# 2022-09-07  1.01      Mia Yu          Add R additional part that formats the
#                                       numeric variables
# 2022-09-07  1.02      Mia Yu          Add parts that read excel file if already existed
# 2022-09-13  1.03      Mia Yu          Add parts that delete duplicat sheets
# 2022-10-09  1.04      Mia Yu          Package version
# 2022-10-11  1.05      Mia Yu          Update the usage of vcqi_object_exists
# 2023-02-07  1.06      Mia Yu          Update to implement level4 customized cell style
# *******************************************************************************



export_table_to_excel <- function(indicator, tablename = NULL, sheet = NULL, brief = TRUE, VCP = "export_stable_to_excel"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (is.null(tablename)){
    tablename <- paste0("TO_",indicator)
  }

  if (is.null(sheet)){
    sheet <- indicator
  }

  sheet1 <- paste0(sheet, " ", ANALYSIS_COUNTER)
  sheet2 <- paste0(sheet, "_BRIEF ", ANALYSIS_COUNTER)

  exporttb <- get(tablename, envir = .GlobalEnv)
  exporttb <- exporttb %>% arrange(level4id) %>% select(-c(level4id))

  #get the cell format from level4_layout
  cellformat <- level4_layout

  if (!"fmtid_for_first_column_r" %in% names(cellformat)){
    cellformat <- cellformat %>% mutate(fmtid_for_first_column_r = NA)
  }

  if (!"fmtid_for_other_columns_r" %in% names(cellformat)){
    cellformat <- cellformat %>% mutate(fmtid_for_other_columns_r = NA)
  }

  cellformat <- cellformat %>% select(order,label,rowtype,fmtid_for_first_column_r,fmtid_for_other_columns_r)

  colnames <- get(paste0(tablename,"_columnlabel"), envir = .GlobalEnv)
  colnames <- data.frame(t(as.matrix(colnames)))

  # calculate number of columns and rows in the table
  ncols = ncol(exporttb)
  nrows = nrow(exporttb)

  # for now we leave two blank rows at the top for title and subtitle
  startrow <- 3

  # replace SMCL tag for bold face or italics, if present
  exporttb <- exporttb %>%
    mutate(name = gsub("{bf:","", name, fixed=TRUE)) %>%
    mutate(name = gsub("{it:","", name, fixed=TRUE)) %>%
    mutate(name = gsub("}"   ,"", name, fixed=TRUE))

  if (file.exists(paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"))){
    wb <- loadWorkbook(paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"))
    sheetnames <- getSheetNames(paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"))

    if (sheet1 %in% sheetnames){
      removeWorksheet(wb, sheet = sheet1)
    }

    if (sheet2 %in% sheetnames){
      removeWorksheet(wb, sheet = sheet2)
    }

  } else{
    wb <- createWorkbook()
  }

  addWorksheet(wb, sheetName = sheet1)

  exporttb <- haven::zap_label(exporttb)
  exporttb <- haven::zap_labels(exporttb)

  writeData(wb, sheet = sheet1, colnames, startCol = 2, startRow = 3, colNames = FALSE)
  writeData(wb, sheet = sheet1, exporttb, startRow = 4, colNames = FALSE)


  if (vcqi_object_exists(paste0(indicator,"_TO_TITLE"))){
    title <- get(paste0(indicator,"_TO_TITLE"), envir = .GlobalEnv)
    title <- data.frame(t(as.matrix(title)))
    writeData(wb, sheet = sheet1, title, startRow = 1, colNames = FALSE)
    addStyle(wb, sheet = sheet1, style = table_title, rows = 1, cols = 1)
  }

  if (vcqi_object_exists(paste0(indicator,"_TO_SUBTITLE"))){
    subtitle <- get(paste0(indicator,"_TO_SUBTITLE"), envir = .GlobalEnv)
    subtitle <- data.frame(t(as.matrix(subtitle)))
    writeData(wb, sheet = sheet1, subtitle, startRow = 2, colNames = FALSE)
    addStyle(wb, sheet = sheet1, style = table_subtitle, rows = 2, cols = 1)
  }

  footnoterow = startrow+nrows+3
  i = 1
  while (vcqi_object_exists(paste0(indicator,"_TO_FOOTNOTE_",i))){
    footnote = get(paste0(indicator,"_TO_FOOTNOTE_",i), envir = .GlobalEnv)
    footnote <- data.frame(t(as.matrix(footnote)))
    writeData(wb, sheet = sheet1, footnote, startRow = footnoterow, colNames = FALSE)
    addStyle(wb, sheet = sheet1, style = table_footnote, rows = footnoterow, cols = 1)

    footnoterow = footnoterow +1
    i = i + 1
  }

  # Use the two regular fmtids that we set up in vcqi_open_log.ado
  # We expect to update this later with fmtids established by MK's new level 4 fmtid code
	# For now this is a placeholer to left align the first column and right-align the others

	# Cells in cols 2 - end use format regular_right
  addStyle(wb, sheet = sheet1, style = regular_right, rows = startrow:(startrow+nrows), cols = 2:ncols, gridExpand = TRUE)
  # Format the column headers as right align and text wrapped
  addStyle(wb, sheet = sheet1, style = col_header, rows = 3, cols = 2:ncols, gridExpand = TRUE)

  # Cells in column 1 (in rows below the table title) use format regular_left
  addStyle(wb, sheet = sheet1, style = regular_left, rows = 2:(startrow+nrows), cols = 1, gridExpand = TRUE)
  setColWidths(wb, sheet = 1, cols = 1, widths = 25)

  # *************************************************
  # Now go through each row to overwrite with any costumized style
  if (use_basic_fmtids == 0){
    for (l in 1:nrow(cellformat)){

      #only go through the line that's not blank row
      if (cellformat$rowtype[l] != "BLANK_ROW"){
        rownum <- which(exporttb$name == cellformat$label[l])

        firstid <- cellformat$fmtid_for_first_column_r[l]
        otherid <- cellformat$fmtid_for_other_columns_r[l]

        #first column
        if (firstid != "" & !is.na(firstid) & !is.null(firstid)){
          if (exists(firstid)){
            firststyle <- get(firstid, envir = .GlobalEnv)
            addStyle(wb, sheet = sheet1, style = firststyle, rows = rownum+3, cols = 1, gridExpand = TRUE)
          }
        }

        #other columns
        if (otherid != "" & !is.na(otherid) & !is.null(otherid)){
          if (exists(otherid)){
            otherstyle <- get(otherid, envir = .GlobalEnv)
            addStyle(wb, sheet = sheet1, style = otherstyle, rows = rownum+3, cols = 2:ncols, gridExpand = TRUE)
          }
        }

      } #end of checking rowtype

    } #end of l loop
  }

  # *************************************************
  # R unique parts that formats numeric variables

  formatnum <- get(paste0(tablename,"_formatnum"), envir = .GlobalEnv)

  format1col <- which(formatnum %in% 1) + 1
  format2col <- which(formatnum %in% 2) + 1
  format3col <- which(formatnum %in% 3) + 1
  format4col <- which(formatnum %in% 4) + 1
  format5col <- which(formatnum %in% 5) + 1

  format1 <- createStyle(numFmt = paste0("0.",rep(0,VCQI_NUM_DECIMAL_DIGITS)))
  format2 <- createStyle(numFmt = "0.0000")
  format3 <- createStyle(numFmt = "#,##0")
  format4 <- createStyle(numFmt = "0.0")

  if (length(format1col) > 0){
    addStyle(wb, sheet = sheet1, style = format1, rows = startrow:(startrow+nrows), cols = format1col, gridExpand = TRUE, stack = TRUE)
  }

  if (length(format2col) > 0){
    addStyle(wb, sheet = sheet1, style = format2, rows = startrow:(startrow+nrows), cols = format2col, gridExpand = TRUE, stack = TRUE)
  }

  if (length(format3col) > 0){
    addStyle(wb, sheet = sheet1, style = format3, rows = startrow:(startrow+nrows), cols = format3col, gridExpand = TRUE, stack = TRUE)
  }

  if (length(format4col) > 0){
    addStyle(wb, sheet = sheet1, style = format4, rows = startrow:(startrow+nrows), cols = format4col, gridExpand = TRUE, stack = TRUE)
  }

  if (length(format5col) > 0){
    colformat <- get(paste0(tablename,"_colformat"), envir = .GlobalEnv)
    for (i in seq_along(format5col)){
      currentformat <- colformat[[format5col[i] - 1]]
      format5 <- createStyle(numFmt = currentformat[[1]])
      addStyle(wb, sheet = sheet1, style = format5, rows = startrow:(startrow+nrows), cols = format5col[i], gridExpand = TRUE, stack = TRUE)
    }
  }

  # *************************************************
  # TOST Step 3b - Export RI_COVG_01_BRIEF to Excel
  #             - I believe there are only three lines of code to change below, per table

  # Export to Excel
  if (brief == TRUE){
    brieftb <- get(paste0(tablename,"_BRIEF"), envir = .GlobalEnv)
    brieftb <- brieftb %>% arrange(level4id) %>% select(-c(level4id))

    # calculate number of columns and rows in the table
    ncols = ncol(brieftb)
    nrows = nrow(brieftb)

    # for now we leave two blank rows at the top for title and subtitle
    startrow <- 3

    # replace SMCL tag for bold face or italics, if present
    brieftb <- brieftb %>%
      mutate(name = gsub("{bf:","",name,fixed=TRUE)) %>%
      mutate(name = gsub("{it:","",name,fixed=TRUE)) %>%
      mutate(name = gsub("}"   ,"",name,fixed=TRUE))

    briefcol <- get(paste0(tablename,"_BRIEF_columnlabel"), envir = .GlobalEnv)
    briefcol <- data.frame(t(as.matrix(briefcol)))

    addWorksheet(wb, sheetName = sheet2)

    brieftb <- haven::zap_label(brieftb)
    brieftb <- haven::zap_labels(brieftb)

    writeData(wb, sheet = sheet2, briefcol, startCol = 2, startRow = 3, colNames = FALSE)
    writeData(wb, sheet = sheet2, brieftb,  startRow = 4, colNames = FALSE)

    if (vcqi_object_exists(paste0(indicator,"_TO_TITLE"))){
      title <- get(paste0(indicator,"_TO_TITLE"), envir = .GlobalEnv)
      title <- data.frame(t(as.matrix(title)))
      writeData(wb, sheet = sheet2, title, startRow = 1, colNames = FALSE)
      addStyle(wb, sheet = sheet2, style = table_title, rows = 1, cols = 1)
    }

    if (vcqi_object_exists(paste0(indicator,"_TO_SUBTITLE"))){
      subtitle <- get(paste0(indicator,"_TO_SUBTITLE"), envir = .GlobalEnv)
      subtitle <- data.frame(t(as.matrix(subtitle)))
      writeData(wb, sheet = sheet2, subtitle, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = sheet2, style = table_subtitle, rows = 2, cols = 1)
    }

    footnoterow = startrow+nrows+3
    i = 1
    while (vcqi_object_exists(paste0(indicator,"_TO_FOOTNOTE_",i))){
      footnote = get(paste0(indicator,"_TO_FOOTNOTE_",i), envir = .GlobalEnv)
      footnote <- data.frame(t(as.matrix(footnote)))
      writeData(wb, sheet = sheet2, footnote, startRow = footnoterow, colNames = FALSE)
      addStyle(wb, sheet = sheet2, style = table_footnote, rows = footnoterow, cols = 1)

      footnoterow = footnoterow +1
      i = i + 1
    }
    # Cells in cols 2 - end use format regular_right
    addStyle(wb, sheet = sheet2, style = regular_right, rows = startrow:(startrow+nrows), cols = 2:ncols, gridExpand = TRUE)
    # Format the column headers as right align and text wrapped
    addStyle(wb, sheet = sheet2, style = col_header, rows = 3, cols = 2:ncols, gridExpand = TRUE)

    # Cells in column 1 (in rows below the table title) use format regular_left
    addStyle(wb, sheet = sheet2, style = regular_left, rows = 2:(startrow+nrows), cols = 1, gridExpand = TRUE)

    # *************************************************
    # Now go through each row to overwrite with any costumized style
    # if the use_basic_fmtids is set to 0 which means that we have imported customized design
    if (use_basic_fmtids == 0){
      for (l in 1:nrow(cellformat)){

        rownum <- which(brieftb$name == cellformat$label[l])

        firstid <- cellformat$fmtid_for_first_column_r[l]
        otherid <- cellformat$fmtid_for_other_columns_r[l]

        #first column
        if (firstid != "" & !is.na(firstid) & !is.null(firstid)){
          if (exists(firstid)){
            firststyle <- get(firstid, envir = .GlobalEnv)
            addStyle(wb, sheet = sheet2, style = firststyle, rows = rownum+3, cols = 1, gridExpand = TRUE)
          }
        }

        #other columns
        if (otherid != "" & !is.na(otherid) & !is.null(otherid)){
          if (exists(otherid)){
            otherstyle <- get(otherid, envir = .GlobalEnv)
            addStyle(wb, sheet = sheet2, style = otherstyle, rows = rownum+3, cols = 2:ncols, gridExpand = TRUE)
          }
        }

      } #end of l loop
    } #end of use_basic_fmtids == 0

    # *************************************************
    # R unique parts that formats numeric variables

    formatnum <- get(paste0(tablename,"_BRIEF_formatnum"), envir = .GlobalEnv)

    format1col <- which(formatnum %in% 1) + 1
    format2col <- which(formatnum %in% 2) + 1
    format3col <- which(formatnum %in% 3) + 1
    format4col <- which(formatnum %in% 4) + 1
    format5col <- which(formatnum %in% 5) + 1

    format1 <- createStyle(numFmt = paste0("0.",rep(0,VCQI_NUM_DECIMAL_DIGITS)))
    format2 <- createStyle(numFmt = "0.0000")
    format3 <- createStyle(numFmt = "#,##0")
    format4 <- createStyle(numFmt = "0.0")

    if (length(format1col) > 0){
      addStyle(wb, sheet = sheet2, style = format1, rows = startrow:(startrow+nrows), cols = format1col, gridExpand = TRUE, stack = TRUE)
    }

    if (length(format2col) > 0){
      addStyle(wb, sheet = sheet2, style = format2, rows = startrow:(startrow+nrows), cols = format2col, gridExpand = TRUE, stack = TRUE)
    }

    if (length(format3col) > 0){
      addStyle(wb, sheet = sheet2, style = format3, rows = startrow:(startrow+nrows), cols = format3col, gridExpand = TRUE, stack = TRUE)
    }

    if (length(format4col) > 0){
      addStyle(wb, sheet = sheet2, style = format4, rows = startrow:(startrow+nrows), cols = format4col, gridExpand = TRUE, stack = TRUE)
    }

    if (length(format5col) > 0){
      colformat <- get(paste0(tablename,"_colformat"), envir = .GlobalEnv)
      for (i in seq_along(format5col)){
        currentformat <- colformat[[format5col[i]]]
        format5 <- createStyle(numFmt = currentformat[[1]])
        addStyle(wb, sheet = sheet2, style = format5, rows = startrow:(startrow+nrows), cols = format5col[i], gridExpand = TRUE, stack = TRUE)
      }
    }

  } #end of exporting brief table

  saveWorkbook(wb, file = paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_ANALYSIS_NAME,"_TO.xlsx"), overwrite = TRUE)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
