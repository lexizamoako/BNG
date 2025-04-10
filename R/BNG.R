#' Calculate BNG Units
#'
#' This function calculates total Biodiversity Net Gain (BNG) units from an sf object of polygons by joining an inbuilt lookup
#' (using the column "HabCode_B") which supplies values for UKHab, Distinctiveness, condition and strategic_significance.
#' If the lookup does not include the columns "condition" or "strategic_significance", default values (3 and 1, respectively) are added.
#'
#' @param polygons An sf object containing polygons. Must include a column "HabCode_B".
#' @param lookup_path Optional character path to the lookup CSV file. If not provided, the package's default file in inst/extdata is used.
#'
#' @return A numeric value representing the total BNG units.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'   # Assuming 'polys' is an sf object with a column "HabCode_B"
#'   total_units <- run_BNG(polys)
#' }
run_BNG <- function(polygons, lookup_path = NULL) {

  if (is.null(lookup_path)) {
    lookup_path <- system.file("extdata", "BNG_lookup.csv", package = "BNG")
    if (lookup_path == "") stop("BNG lookup file not found in package extdata.")
  }

  lookup_data <- read.csv(lookup_path, stringsAsFactors = FALSE)

  # lookup includes the essential column for calculation.
  if (!"Distinctiveness" %in% names(lookup_data)) {
    stop("Lookup file must contain a column 'Distinctiveness'.")
  }

  # Add default values if missing in the lookup.
  if (!"condition" %in% names(lookup_data)) {
    lookup_data$condition <- 3
  }
  if (!"strategic_significance" %in% names(lookup_data)) {
    lookup_data$strategic_significance <- 1
  }

  # Ensure polygons have the join key.
  if (!"HabCode_B" %in% names(polygons)) {
    stop("Polygons must have the column 'HabCode_B'.")
  }

  # Join lookup data to polygons based on HabCode_B.
  polygons <- dplyr::left_join(polygons, lookup_data, by = "HabCode_B")

  # Calculate area in hectares.
  polygons$area_ha <- as.numeric(sf::st_area(polygons)) / 10000

  # Verify that all required attributes now exist.
  required_cols <- c("condition", "strategic_significance", "Distinctiveness", "area_ha")
  missing_cols <- setdiff(required_cols, names(polygons))
  if (length(missing_cols) > 0) {
    stop("Polygons are missing the following required attributes: ", paste(missing_cols, collapse = ", "))
  }

  # Compute BNG units per polygon and sum.
  polygons$BNG_units <- polygons$condition * polygons$strategic_significance *
    polygons$Distinctiveness * polygons$area_ha

  total_units <- sum(polygons$BNG_units, na.rm = TRUE)
  return(total_units)
}
