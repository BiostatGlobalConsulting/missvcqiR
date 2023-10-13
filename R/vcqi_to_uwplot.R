#' Create barplot with unweighted data
#'
#' @param database Database used to create the plot
#' @param title Title of the plot
#' @param name Plot identifier in file name
#' @param savedata Path to save the mutated data that created the plot
#' @param savew Width of the plot file in inches
#' @param saveh Height of the plot file in inches
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A plot
#'
#' @import ggplot2
#' @import dplyr

# vcqi_to_uwplot R version 1.06 - Biostat Global Consulting - 2023-02-03
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-29  1.00      Mia Yu          Original R version
# 2022-10-12  1.01      Mia Yu          Update ANNOTATE logic
# 2022-10-13  1.02      Mia Yu          Package version
# 2022-10-19  1.03      Caitlin Clary   Update error message handling, add calls
#                                       to vcqi_halt_immediately
# 2022-12-15  1.04      Mia Yu          Add title etc. to the dataset
# 2023-01-12  1.05      Mia Yu          Add parts to allow users customize level4 plots
# 2023-02-03  1.06      Mia Yu          Updated level4 plots customization
# *******************************************************************************

vcqi_to_uwplot <- function(
    database,
    title = NULL,
    name = NULL,
    savedata = NA,
    savew = 7,
    saveh = 7,
    VCP = "vcqi_to_uwplot"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (file.exists(database)){
    dat <- vcqi_read(database)

    if (!is.data.frame(dat)){
      errormsgs <- c(
        "vcqi_to_uwplot: The VCQI database passed in to this program is not in the right format.",
        paste0("vcqi_to_uwplot: The database named was ",database,".")
      )

      vcqi_log_comment(
        VCP, 1, "Error",
        "vcqi_to_uwplot: The VCQI database passed in to this program is not in the right format.")
      vcqi_log_comment(
        VCP, 1, "Error",
        paste0("vcqi_to_uwplot: The database named was ", database, "."))

      vcqi_global(VCQI_ERROR, 1)
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    }

  } else {
    errormsgs <- c(
      "vcqi_to_uwplot: The VCQI database passed in to this program does not seem to exist.",
      paste0("vcqi_to_uwplot: The database named was ", database, ".")
    )

    vcqi_log_comment(
      VCP, 1, "Error",
      "vcqi_to_uwplot: The VCQI database passed in to this program does not seem to exist.")
    vcqi_log_comment(
      VCP, 1, "Error",
      paste0("vcqi_to_uwplot: The database named was ", database, "."))

    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  } #end of check database exists

  # Sort proportions based on user request
  # Default is sorting proportions low at bottom of plot to high at top of plot

  if (SORT_PLOT_LOW_TO_HIGH == 0){
    # meaning, sort prop high to low
    dat <- arrange(dat, desc(estimate))
  } else{
    dat <- arrange(dat, estimate)
  }

  # If user wants strata plotted in table order, merge the table order
  # and sort accordingly

  if (PLOT_OUTCOMES_IN_TABLE_ORDER == 1){
    vcqi_log_comment(
      VCP, 3, "Comment",
      "User has requested that outcomes be plotted in table order instead of sorting by indicator outcome.")
    dat <- arrange(dat, desc(level4id))
  }

  dat <- dat %>%
    subset(!is.na(estimate)) %>%
    mutate(rowid = row_number())

  if (UWPLOT_ANNOTATE_LOW_MED != 1) {
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS,"f"), estimate*100),"% ",
        language_string(language_use = language_use, str = "OS_48"),
        " = ", prettyNum(n,big.mark=",")))

    #note <- "Text at right: Unweighted sample proportion (%) and N"
    note <- language_string(language_use = language_use, str = "OS_71")
  } else if (UWPLOT_ANNOTATE_LOW_MED == 1) {

    if (!vcqi_object_exists("UWPLOT_ANNOTATE_MED_N")) {
      vcqi_global(UWPLOT_ANNOTATE_MED_N, 50)}
    if (!vcqi_object_exists("UWPLOT_ANNOTATE_LOW_N")) {
      vcqi_global(UWPLOT_ANNOTATE_LOW_N, 25)}

    if (UWPLOT_ANNOTATE_MED_N < UWPLOT_ANNOTATE_LOW_N){
      vcqi_log_comment(
        VCP, 3, "Comment",
        paste0("The user-entered parameter UWPLOT_ANNOTATE_MED_N is set to ",
               UWPLOT_ANNOTATE_MED_N,
               " and UWPLOT_ANNOTATE_LOW_N is set to ", UWPLOT_ANNOTATE_LOW_N,
               ". This is not a logical combination so the values are being reset to UWPLOT_ANNOTATE_LOW_N = 25 and UWPLOT_ANNOTATE_MED_N = 50."))

      warning(paste0(
        "The user-entered parameter UWPLOT_ANNOTATE_MED_N is set to ",
        UWPLOT_ANNOTATE_MED_N,
        " and UWPLOT_ANNOTATE_LOW_N is set to ", UWPLOT_ANNOTATE_LOW_N,
        ". This is not a logical combination so the values are being reset to UWPLOT_ANNOTATE_LOW_N = 25 and UWPLOT_ANNOTATE_MED_N = 50."),
        immediate. = TRUE)

      vcqi_global(UWPLOT_ANNOTATE_MED_N, 50)
      vcqi_global(UWPLOT_ANNOTATE_LOW_N, 25)
    }

    dat <- mutate(dat,
                  text = paste0(
                    sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"),
                            estimate * 100),"% ",
                    language_string(language_use = language_use, str = "OS_48"),
                    " = ",
                    prettyNum(n, big.mark = ",")))

    dat <- mutate(
      dat,
      text = ifelse((n < UWPLOT_ANNOTATE_LOW_N) %in% TRUE,
                    paste0(sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"),
                                   estimate*100), "% ",
                           language_string(language_use = language_use, str = "OS_48"),
                           " = ", prettyNum(n, big.mark = ","), " \u2021"),text))

    dat <- mutate(
      dat,
      text = ifelse((n >= UWPLOT_ANNOTATE_LOW_N & n < UWPLOT_ANNOTATE_MED_N) %in% TRUE,
                    paste0("(", sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"),
                                       estimate*100), "%) ",
                           language_string(language_use = language_use, str = "OS_48"),
                           " = ", prettyNum(n, big.mark = ",")),text))

    note <- paste0(
      "Text at right: Unweighted sample proportion (%) and N \n Parentheses () mean ", UWPLOT_ANNOTATE_LOW_N,
      " \u2264 N < ", UWPLOT_ANNOTATE_MED_N, ". \u2021 means N < ", UWPLOT_ANNOTATE_LOW_N)
  }

  #DEC 15: add title etc. to the dataset
  dat <- dat %>% mutate(graphtitle = NA, graphcaption = NA)
  if (!is.null(title)){
    dat <- dat %>% mutate(graphtitle = title)
  }

  if (!is.null(note)){
    dat <- dat %>% mutate(graphcaption = note)
  }

  if (!is.na(savedata)){
    saveRDS(dat, file = paste0(savedata, ".rds"))
  }

  filename <- paste0(VCQI_OUTPUT_FOLDER, "/Plots_IW_UW/", name)

  if (IWPLOT_SHOWBARS == 1){
    extraspace <- max(nchar(dat$text))

    if (is.na(dat$graphtitle[1])){
      title <- NULL
    } else {
      title <- dat$graphtitle[1]
    }

    if (is.na(dat$graphcaption[1])){
      note <- NULL
    } else {
      note <- dat$graphcaption[1]
    }

    #first bring the columns to the data
    dat <- dat %>% mutate(order = level4id)
    dat <- left_join(dat, level4_layout, by = "order")
    dat <- dat %>% select(-c(order, label, condition, rowtype))

    if ("outlinecolor1_r" %in% names(dat)) {
      dat <-
        dat %>% mutate(outlinecolor1_r = ifelse((is.na(outlinecolor1_r) | is.null(outlinecolor1_r) | outlinecolor1_r == "") %in% TRUE,
                                                "#0000ff" , outlinecolor1_r))
    } else {
      dat <- dat %>% mutate(outlinecolor1_r = "#0000ff")
    }

    if ("bar_fillcolor1_r" %in% names(dat)) {
      dat <- dat %>% mutate(bar_fillcolor1_r = ifelse((is.na(bar_fillcolor1_r) | is.null(bar_fillcolor1_r) | bar_fillcolor1_r == "") %in% TRUE,
                                                  "#2b92be" ,bar_fillcolor1_r))
    } else {
      dat <- dat %>% mutate(bar_fillcolor1_r = "#2b92be")
    }

    gap <-  1

    #change the rowid to have extra space if line added
    if ("addline" %in% names(dat) | "shadecolor1_r" %in% names(dat)) {
      dat <- dat %>% mutate(rowid = rowid * 2)
      gap <- 2
    }

    baseplot <- ggplot(dat, aes(x = rowid, y = estimate * 100)) +
      theme_bw(base_family = "sans") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    for (l in 1:nrow(dat)) {
      if ("shadecolor1_r" %in% names(dat)) {
        shade <- "TRUE"
        shadecolor1_r <- dat$shadecolor1_r[l]
        if (shadecolor1_r == "" | is.na(shadecolor1_r) | is.null(shadecolor1_r)) {
          shade <- "FALSE"
        }
      } else {
        shade <- "FALSE"
      }

      if ("addline" %in% names(dat)) {
        addl <- "TRUE"
        addline <- dat$addline[l]
        if (addline == "" | is.na(addline) | is.null(addline)) {
          addl <- "FALSE"
        }

      } else {
        addl <- "FALSE"
      }

      if (shade == "TRUE") {
        xminimum <- dat$rowid[l] - 0.9
        xmaximum <- dat$rowid[l] + 0.9
        yminloc = 0
        ymaxloc = 100

        baseplot <- baseplot +
          geom_rect(aes_string(xmin = xminimum, xmax = xmaximum, ymin = yminloc, ymax = ymaxloc), fill = shadecolor1_r)
      }

      if (addl == "TRUE") {
        xlocation <- dat$rowid[l]
        yminloc = 0
        ymaxloc = 100

        if (addline == "below") {
          baseplot <-
            baseplot + geom_linerange(aes_string(x = xlocation - 1,ymin = yminloc,ymax = ymaxloc), color = "lightgrey")
        }

        if (addline == "above") {
          baseplot <-
            baseplot + geom_linerange(aes_string(x = xlocation + 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

        if (addline == "both") {
          baseplot <- baseplot +
            geom_linerange(aes_string(x = xlocation + 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey") +
            geom_linerange(aes_string(x = xlocation - 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

      }

    } #end of nrow l loop

    baseplot <- baseplot +
      geom_col(width = 1,fill = dat$bar_fillcolor1_r,color = dat$outlinecolor1_r,size = 0.3) +
      geom_text(aes(x = rowid,y = 100 + 1.25 * extraspace,label = text),colour = "black",family = "sans") +
      coord_flip() +
      labs(y = language_string(language_use = language_use, str = "OS_68"), #Sample Proportion %
           x = "",title = title,caption = note) +
      scale_x_continuous(breaks = seq(min(dat$rowid), max(dat$rowid), by = gap), labels = dat$name) +
      #Note: could find a better way to check the space we need for text
      scale_y_continuous(limits = c(0, 100 + 1.75 * extraspace),
                         breaks = c(0, 25, 50, 75, 100)) +
      theme(plot.caption = element_text(hjust = 0),
            text = element_text(family = "sans", colour = "black"))

    ggsave(plot = baseplot,paste0(filename, ".png"),width = savew,height = saveh,units = "in")

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
