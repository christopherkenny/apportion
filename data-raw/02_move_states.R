# From replication data for
# Widespread Partisan Gerrymandering Mostly Cancels Nationally,
# but Reduces Electoral Competition

place_geometry <- function(geometry, position, scale = 1) {
  if (missing(position))
    position <- suppressWarnings(sf::st_coordinates(sf::st_centroid(geometry))) |> c()
  (geometry - sf::st_centroid(st_union(geometry))) * scale +
    sf::st_sfc(sf::st_point(position))
}

move_states <- function(shp, epsg = 'ESRI:102003', trim_hi = FALSE) {
  crs_lower48 <- epsg#102003
  crs_alaska <- 3338
  crs_hawaii <- 'ESRI:102007'
  shp_lower48 <- shp %>%
    filter(!state %in% c('AK', 'HI')) %>%
    sf::st_transform(crs_lower48)
  bb <- sf::st_bbox(shp_lower48)
  us_alaska_cong <- shp %>%
    dplyr::filter(state == 'AK') %>%
    sf::st_transform(crs_alaska)

  sf::st_geometry(us_alaska_cong) <- place_geometry(
    sf::st_geometry(us_alaska_cong),
    c(
      bb$xmin + 0.2 * (bb$xmax - bb$xmin),
      bb$ymin - 0 * (bb$ymax - bb$ymin)
    ),
    scale = 0.5
  )
  sf::st_crs(us_alaska_cong) <- crs_lower48

  # move Hawaii
  us_hawaii_cong <- shp %>%
    dplyr::filter(state == 'HI') %>%
    sf::st_transform(crs_hawaii)
  sf::st_geometry(us_hawaii_cong) <- place_geometry(
    sf::st_geometry(us_hawaii_cong),
    c(
      bb$xmin + 0.4 * (bb$xmax - bb$xmin),
      bb$ymin + -0.05 * (bb$ymax - bb$ymin)
    ),
    scale = 1.3
  )

  sf::st_crs(us_hawaii_cong) <- crs_lower48

  if (trim_hi) {
    trim_to <- st_bbox(us_hawaii_cong)
    r <- 0.3
    trim_to['xmin'] <- r * trim_to['xmin'] + (1 - r) * trim_to['xmax']
    trim_to <- st_as_sfc(trim_to)

    us_hawaii_cong$geometry[!st_intersects(us_hawaii_cong, y = trim_to, sparse = FALSE)] <- st_sfc(st_polygon())

    us_hawaii_cong <- us_hawaii_cong |>
      st_crop(y = trim_to) |>
      suppressWarnings()
  }



  rbind(shp_lower48, us_alaska_cong, us_hawaii_cong)
}

trim_bbox <- function() {
  structure(
    list(
      structure(
        list(
          structure(
            c(-17872362.3799765,
              -17232687.1535802, -17232687.1535802, -17872362.3799765, -17872362.3799765,
              2143818.20477186, 2143818.20477186, 2540291.4781892, 2540291.4781892,
              2143818.20477186), dim = c(5L, 2L))), class = c("XY", "POLYGON",
                                                              "sfg"))),
    class = c("sfc_POLYGON", "sfc"), precision = 0, bbox = structure(c(xmin = -17872362.3799765,
                                                                       ymin = 2143818.20477186, xmax = -17232687.1535802, ymax = 2540291.4781892
    ), class = "bbox"),
    crs = structure(list(input = "EPSG:3857",
                         wkt = "PROJCRS[\"WGS 84 / Pseudo-Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Popular Visualisation Pseudo-Mercator\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting (X)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing (Y)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Web mapping and visualisation.\"],\n        AREA[\"World between 85.06\302\260S and 85.06\302\260N.\"],\n        BBOX[-85.06,-180,85.06,180]],\n    ID[\"EPSG\",3857]]"), class = "crs"), n_empty = 0L)


}

rotate_shp <- function(shp, rot_rads, scale = 1) {
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  shpg <- sf::st_geometry(shp)
  crs <- sf::st_crs(shp)
  cntrd <- sf::st_centroid(sf::st_union(shpg))
  shpg2 <- (shpg - cntrd) * rot(rot_rads) * 1 + cntrd
  sf::st_geometry(shp) <- shpg2
  sf::st_crs(shp) <- crs
  shp
}
