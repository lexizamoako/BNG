#' Calculate BNG Units with Conservative Defaults
#'
#' This function calculates total Biodiversity Net Gain (BNG) units from an sf object of polygons by joining an inbuilt lookup
#' (using the column "HabCode_B") which supplies values for UKHab and Distinctiveness.
#' It then applies conservative defaults for condition (Poor = 1) and strategic significance (Low = 1.0)
#' unless the user explicitly specifies other values.
#'
#' @param polygons An sf object containing polygons. Must include a column "HabCode_B".
#' @param lookup_path Optional character path to the lookup CSV file. If not provided, the package's default file in inst/extdata is used.
#' @param user_condition Numeric value (1–3) to apply to all habitats; defaults to 1 (Poor).
#' @param user_strategic_significance Numeric value (e.g., 1.0, 1.1, 1.15) to apply to all habitats; defaults to 1.0 (Low).
#'
#' @return A numeric value representing the total BNG units.
#' @export
#'
#' @examples
#' \dontrun{
#'   total_units <- run_BNG(polys)
#'   total_units <- run_BNG(polys, user_condition = 2, user_strategic_significance = 1.1)
#' }
run_BNG <- function(polygons,
                    lookup_path = NULL,
                    user_condition = 1,
                    user_strategic_significance = 1.0) {
  
  if (is.null(lookup_path)) {
    lookup_path <- system.file("extdata", "BNG_lookup.csv", package = "BNG")
    if (lookup_path == "") stop("BNG lookup file not found in package extdata.")
  }
  lookup_data <- read.csv(lookup_path, stringsAsFactors = FALSE)
  
  # ensure Distinctiveness present
  if (!"Distinctiveness" %in% names(lookup_data)) {
    stop("Lookup file must contain a column 'Distinctiveness'.")
  }
  
  # Join lookup to polygons on HabCode_B
  polygons <- dplyr::left_join(polygons, lookup_data, by = "HabCode_B")
  
  # Apply user-specified or default condition and strategic significance
  polygons$condition <- user_condition
  polygons$strategic_significance <- user_strategic_significance
  
  # Compute area in hectares
  polygons$area_ha <- as.numeric(sf::st_area(polygons)) / 10000
  
  # Validate presence of required columns
  required_cols <- c("condition", "strategic_significance", "Distinctiveness", "area_ha")
  missing_cols <- setdiff(required_cols, names(polygons))
  if (length(missing_cols)) {
    stop("Missing: ", paste(missing_cols, collapse = ", "))
  }
  
  # Calculate and sum BNG units
  polygons$BNG_units <- with(polygons,
                             condition * strategic_significance * Distinctiveness * area_ha
  )
  
  sum(polygons$BNG_units, na.rm = TRUE)
}
