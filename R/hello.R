#' Clean dataset
#'
#' This function tidies the dataset up by creating a DATE column from the YEAR,
#' MONTH and DAY columns (with date format) and converting the columns LONGITUDE
#' and LATITUDE into numeric.
#'
#' @param x A messy datasets with columns as described
#'
#' @return This function returns a (hopefully cleaner) dataset
#'
#' @importFrom magrittr `%>%`
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' eq_clean_data(messy_data)
#' }
#'
#' @export

eq_clean_data <- function(x) {

  x %>%
    unite("DATE", YEAR:DAY) %>%
    mutate(DATE = as.Date(DATE, format = "%Y_%m_%d"),
           across(c(LONGITUDE, LATITUDE), as.numeric))

}

#' Clean dataset
#'
#' This function extracts locations from the hellspawn column LOCATION_NAME using
#' a deadly regex, and various trimming and beauty treatments from strings.
#'
#' @param x A dataset with a LOCATION_NAME column filled in by a monkey
#'
#' @return This function returns a (hopefully cleaner) dataset
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#' @importFrom stringr str_extract
#'
#' @examples
#' \dontrun{
#' eq_location_clean(messy_data)
#' }
#'
#' @export

eq_location_clean <- function(x) {

  x %>%
    mutate(LOCATION =
             str_trim(
               str_to_title(
                 str_extract(LOCATION_NAME,
                             pattern = "((?<=:\\s{1,4})[:alnum:]{4,})(?!:)"))
             )
    )

}

############ Definition of geom_timeline ##################

#' Timeline Geom
#'
#' This geom plots a timeline of hurricanes in a country. The only thing you really
#' need to input is the \code{x} coordinate, which should be a date. You can optionally
#' provide a \code{y} coordinate, e.g. a country, to have more timelines stacked
#' vertically. The usual aes for geom_point apply.
#'
#' @param x Dates (date format)
#' @param y Country (a factor)
#'
#' @return This function returns a Geom.
#'
#' @importFrom magrittr `%<>%`
#' @importFrom dplyr filter
#' @importFrom grid pointsGrob
#' @importFrom ggplot2 ggproto
#'

GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c("x"),
                        default_aes = ggplot2::aes(y = 0,
                                          size = 1,
                                          colour = "Black",
                                          fill = "Black",
                                          shape = 19,
                                          alpha = 0.5,
                                          xmin = -Inf,
                                          xmax = +Inf),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_params, coord) {

                          data %<>% filter(x >= xmin,
                                           x <= xmax)

                          coords <- coord$transform(data, panel_params)

                          grid::pointsGrob(
                            coords$x, coords$y,
                            pch = coords$shape,
                            size = unit(coords$size, "char"),
                            gp = grid::gpar(col = coords$colour,
                                            alpha = coords$alpha)
                          )

                        }
)

#' geom_timeline
#'
#' This geom plots a timeline of hurricanes in a country. The only thing you really
#' need to input is the \code{x} coordinate, which should be a date. You can optionally
#' provide a \code{y} coordinate, e.g. a country, to have more timelines stacked
#' vertically. The usual aes for geom_point apply.
#'
#' @inheritParams GeomTimeline
#'
#' @return This function returns a plot.
#'
#' @importFrom magrittr `%<>%`
#' @importFrom dplyr filter
#' @importFrom grid pointsGrob
#' @importFrom ggplot2 ggproto
#'
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

############ Definition of geom_timeline_label ##################

#' TimelineLabel Geom
#'
#' This geom plots label over a timeline of hurricanes in a country. You need to input
#' the \code{x} coordinate, which should be a date, and a \code{label} parameter giving
#' the column from which you want to fish the labels. You can optionally
#' provide a \code{y} coordinate, e.g. a country, to have more timelines stacked
#' vertically. The usual aes for geom_point apply.
#'
#' @param x Dates (date format)
#' @param label Labels (char format)
#' @param y Country (a factor)
#'
#' @return This function returns a Geom.
#'
#' @importFrom magrittr `%>%`
#' @importFrom grid textGrob
#' @importFrom ggplot2 ggproto
#'

GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                             required_aes = c("x", "label"),
                             default_aes = ggplot2::aes(y = 0,
                                               angle = 45,
                                               nmax = 10000,
                                               size = 1),
                             draw_key = ggplot2::draw_key_point,
                             draw_panel = function(data, panel_params, coord) {

                               plot_data <- data %>%
                                 extract(1:8,)

                               coords <- coord$transform(plot_data, panel_params)

                               labels <- grid::textGrob(
                                 label = coords$label,
                                 x = coords$x,
                                 y = coords$y + 0.1,
                                 just = "left",
                                 rot = coords$angle,
                                 check.overlap = FALSE)

                               segments <- grid::segmentsGrob(x0 = coords$x,
                                                              y0 = coords$y,
                                                              x1 = coords$x,
                                                              y1 = coords$y + 0.1)

                               grid::gTree(children = grid::gList(labels, segments))

                             }
)

#' geom_timeline
#'
#' This geom plots label over a timeline of hurricanes in a country. You need to input
#' the \code{x} coordinate, which should be a date, and a \code{label} parameter giving
#' the column from which you want to fish the labels. You can optionally
#' provide a \code{y} coordinate, e.g. a country, to have more timelines stacked
#' vertically. The usual aes for geom_point apply.
#'
#' @inheritParams GeomTimelineLabel
#'
#' @return This function returns a plot.
#'
#' @importFrom magrittr `%>%`
#' @importFrom grid textGrob
#' @importFrom ggplot2 ggproto
#'
#' @export

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

######### Leaflet stuff #######

#' Create leaflet map
#'
#' This function takes in input a dataset with longitude and latitude columns
#' and a column to extract labels from. Then creates an interactive leaflet map
#' with markers annotated by the given column.
#'
#' @param data A dataset with geographic information and a column with annotations
#' @param annot_col A column with annotations in the dataset
#'
#' @return This function returns a leaflet interactive map
#'
#' @importFrom magrittr `%>%`
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_map(geographic_data)
#' }
#'

eq_map <- function(data, annot_col) {

  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = data,
      radius = ~ EQ_PRIMARY,
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      popup = ~ data[[annot_col]]
    )

}


#' Create additional column combining information from the dataset
#'
#' This function takes in input a dataset with LOCATION, EQ_PRIMARY and TOTAL_DEATHS columns.
#' Then creates an additional html formatted column combining the information from these three
#' column.
#'
#' @param data A dataset with columns as described above
#'
#' @return This function returns the original dataset plus an additional column as
#' described above.
#'
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_c
#'
#' @examples
#' \dontrun{
#' mutate(data, label = eq_create_label(.))
#' }
#'

eq_create_label <- function(data) {

  str_c(ifelse(is.na(data$LOCATION),
               "",
               str_c("<b>Location: </b>", data$LOCATION, "<br>")),
        ifelse(is.na(data$EQ_PRIMARY),
               "",
               str_c("<b>Magnitude: </b>", data$EQ_PRIMARY, "<br>")),
        ifelse(is.na(data$TOTAL_DEATHS),
               "",
               str_c("<b>Total deaths: </b>", data$TOTAL_DEATHS))
  )

}

