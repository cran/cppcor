#' Graphical User Interface for PCCor function
#'
#' \code{GUI_cppcor} A Graphical User Interface (GUI) for function that returns the confusion matrix and parameters of classification analysis
#' @param gui Logical argument, \code{TRUE} or \code{FALSE}. The default is \code{FALSE}
#' @return \code{GUI_cppcor} return a GUI for the confusion matriz and parameters of classification analysis
#' @examples
#' # Loading package
#' library(cppcor)
#' if (interactive()) {
#'   GUI_cppcor()
#' }
#'
#' @import "gWidgets"
#' @importFrom "stats" "cor" "var"
#' @importFrom "pacman" "p_load"
#' @export

GUI_cppcor <- function(gui = TRUE) {
  if (gui) {
    #nocov start
    if (!pacman::p_exists("gWidgetsRGtk2", local = TRUE)) {
      pacman::p_load("gWidgetsRGtk2")
    }
    oldoptions <- options("guiToolkit" = "RGtk2")
    on.exit(options(oldoptions))
  }

  #Main window
  GUI_cppcor <- gwindow("PCCor", visible = FALSE, height = 600)

  # Notebook
  notePCCor <- gnotebook(container = GUI_cppcor)

  # Groups
  g1PCCor <- ggroup(container = notePCCor, label = gettext("Analysis"), horizontal = FALSE, expand = TRUE)
  g2PCCor <- ggroup(container = notePCCor, label = gettext("Help"),
                    horizontal = FALSE)

  # Lab current
  svalue(notePCCor) <- 1

  # Group2
  g21PCCor <- ggroup(container = g2PCCor)
  addSpring(g21PCCor)
  glabel("Help on the functions of the PCCor package:", container = g21PCCor)
  e <- gedit("", container = g21PCCor)
  e[] <- c("PCCor", "GUI_cppcor")

  helpWidget <- ghelp(container = g2PCCor, expand = TRUE, package = "PCCor")
  addHandlerChanged(e, handler=function(h,...) {
    add(helpWidget, svalue(h$obj))
  })

  # Group1

  Dataset <- gframe("Dataset", container = g1PCCor, horizontal = FALSE)

  # Label
  glabel("Search the data file (.txt or .csv)", container = Dataset, anchor = c(-1, 0))

  # Auxiliar function
  f.txt <- NULL
  f.txt <- function(file){
    if (grepl("\\.txt$", svalue(dataset))) {
      if (svalue(group_cbox_1)) {
        read.table(file, header = TRUE, dec = ",", sep = svalue(group_cbox_2))
      }
      if (svalue(group_cbox_1) == FALSE) {
        read.table(file, header = TRUE, sep = svalue(group_cbox_2))
      }
    }
    if(grepl("\\.csv$", svalue(dataset))) {
      if (svalue(group_cbox_1)) {
        read.table(file, header = TRUE, dec = ",", sep = svalue(group_cbox_2))
      }
      if (svalue(group_cbox_1) == FALSE) {
        read.table(file, header = TRUE, sep = svalue(group_cbox_2))
      }
    }
  }

  # Open data.frame and save
  dataset <- gfilebrowse(text = "Select a file ...",
                          quote = FALSE, container = Dataset,
                          type = c("open"),
                          filter = c("txt"="txt", "csv"="csv"),
                          handler = function(h, ...){
                            dataPCCor <- NULL
                            dataPCCor <<- f.txt(svalue(dataset)); dataPCCor
                            gmessage("Verify that the last column of data is a class of individuals.
                                    For details use the View button.", icon = "info")
                          })


  # Group of buttons
  group_cbox <- ggroup(container = Dataset)

  # Group of checkbox
  group_cbox_1 <- gcheckbox(
    text      = "Comma as decimal points",
    checked   = FALSE,
    container = group_cbox
  )
  # delete(group_buttons, group_buttons[1,3])

  # Separator
  gseparator(horizontal = FALSE, container = group_cbox)

  glabel("Separator of variables:", container = group_cbox)
  group_cbox_2 <- gedit("", container = group_cbox, width = 5)

  # Group of buttons
  group_buttons <- ggroup(container = Dataset)

  # Button of View
  group_buttons_1 <- gbutton("View", container = group_buttons,
                             handler = function(h, ...){
                               dataPCCor <- NULL
                               dataPCCor <<- edit(f.txt(svalue(dataset))); dataPCCor
                             })

  # # Button Console
  # group_buttons_2 <- console_button <- gbutton("Choosing the directory", container = group_buttons,
  #                                              handler = function(h, ...){
  #                                               newdir1 <- NULL; newdir2 <- NULL
  #                                               newdir1 <<- getwd()
  #                                               newdir2 <<- setwd(gfile(container = group_buttons, type = "selectdir"))
  #                                               on.exit(setwd(newdir2))
  #                                              })

  Conditions <- gframe("Conditions", container = g1PCCor, horizontal = FALSE)

  g1results <- ggroup(container = Conditions, horizontal = TRUE)

  glabel("The data is independent:", container = g1results, anchor = c(-1, 0))
  dataind <- NULL
  dataind <<- gradio(c("TRUE", "FALSE"), horizontal = TRUE,
                     selected = 1, container = g1results); dataind
  ##



  Console.frame <- gframe("Console", container = g1PCCor, expand = TRUE)
  console <- gcommandline(container = Console.frame, expand = TRUE)


  gbutton("Calculate", container = g1PCCor, handler = function(h, ...){
    start_message <- galert("Wait, we are analyzing the data ...", icon = "info",
                            delay = 2)
    svalue(console) <- "PCCor::PCCor(dataset = dataPCCor, ID = eval(parse(text = svalue(dataind))))"
    end_message <- gmessage("Sorry for the delay, but the results are ready on your screen!",
                            icon = "info")
  })


  ##########
  # Messages
  ##########
  close <- addHandlerUnrealize(GUI_cppcor, handler = function(h , ...) {
    !gconfirm("Really close", parent = h$obj)
  })

  visible(GUI_cppcor) <- TRUE
}


