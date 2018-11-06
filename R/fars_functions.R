#' Loads csv file into memory
#'
#' This function will import a csv file. If the file does
#' not exist, an error will be generated. If the file does
#' exist, it will be imported as a tbl_df, or tibble data frame.
#'
#' @parameter The filepath to the csv file is required
#'
#' @return A tbl_df (tibble) will be returned containing the
#' information in the csv file.
#'
#' @examples \dontrun{
#' foobar_data <- fars_read("c:/User/foobar/foobar.csv")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- readr::read_csv(filename, progress = FALSE)
  dplyr::tbl_df(data)
}

#' Generate date-specific filename
#'
#' This function accepts an input and returns a filename that
#' meets a proper templated format while including the unique
#' date of the input string. Given the as.integer function
#' within our function, the input must be numeric, otherwise an
#' error will be thrown.
#'
#' @parameter An integer that should correlate to the year of the
#' dataset being worked on
#'
#' @return This function returns a string that will serve as
#' a filename for the dataset in question. The main purpose of
#' this function is to ensure the unique year is added to the
#' filename.
#'
#' @examples
#' \dontrun{
#' make_filename(2018)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Pulls month and year from an accident report csv
#'
#' @description This function accepts a numberic input
#' of either one or multipe years, then
#' collects the month and year columns from the corresponding
#' data file. If the year or one of the years are not searchable,
#' the function will throw a warning and a NULL output.
#'
#' @parameter years a vector of year(s)
#'
#' @return uses fars_read() function to create one or more
#' tibble data frames that matches the year of the file name
#' created with the make_filename() function. each tibble df
#' will contain two columns, month and year. In the event that
#' the year entered does not pull a valid data file, a warning
#' will be generated as well as a "NULL" value.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013,2014)
#'
#' fars_read_years(2018)
#'
#' # This function will generate a warning
#' }
#'
#' @importFrom dplyr %>% mutate select
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' generates a tibble that tallies the number of accidents
#' per year, segmented by month.
#'
#' This function accepts a list of qualifying years and
#' will create a new tibble dataframe that
#' separates the twelve months into rows, and the years
#' into columns. The function then tallies the number of
#' accidents in each month.
#'
#' @parameter years A list of numbers that will be used as
#' reference to seach the datafile for the desired data.
#'
#' @return Returns a tibble dataframe with the months sorted
#' vertically by row and the years organized horizontally as
#' column headers. This function is also designed to return a
#' warning if the year is not available as source data (from
#' fars_read_years()) and an error if the input argument is not
#' numeric.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2014)
#' }
#'
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr %>% spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots the noted accidents on a map of the US by state
#'
#' This function takes the numeric identifier of each state
#' from our source data files and plots the accidents on a map of
#' the United States. A stop clause exists if any of the state
#' numbers are not unique. A warning also exists in th event
#' that a row has 0 data points.
#'
#' @parameter state.num The unique state identifier listed
#' in the source data files.
#' @parameter year the year being input from the source file.
#'
#' @return This function returns a plotted chart of the accidents on a
#' map. If the state or year input is invalide, and error will be given.
#'
#' @examples \dontrun{
#' fars_map_state(22, 2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
