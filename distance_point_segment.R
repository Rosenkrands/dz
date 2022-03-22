library(tidyverse)

distancePointSegment <- function(px, py, x1, y1, x2, y2) {
  ## px,py is the point to test.
  ## x1,y1,x2,y2 is the line to check distance.
  ##
  ## Returns distance from the line, or if the intersecting point on the line nearest
  ## the point tested is outside the endpoints of the line, the distance to the
  ## nearest endpoint.
  ##
  ## Returns 9999 on 0 denominator conditions.
  lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2-x1)^2+(y2-y1)^2)
  ans <- NULL
  ix <- iy <- 0   # intersecting point
  lineMag <- lineMagnitude(x1, y1, x2, y2)
  if( lineMag < 0.00000001) {
    warning("short segment")
    return(9999)
  }
  u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
  u <- u / (lineMag * lineMag)
  if((u < 0.00001) || (u > 1)) {
    ## closest point does not fall within the line segment, take the shorter distance
    ## to an endpoint
    ix <- lineMagnitude(px, py, x1, y1)
    iy <- lineMagnitude(px, py, x2, y2)
    if(ix > iy)  ans <- iy
    else ans <- ix
  } else {
    ## Intersecting point is on the line, use the formula
    ix <- x1 + u * (x2 - x1)
    iy <- y1 + u * (y2 - y1)
    ans <- lineMagnitude(px, py, ix, iy)
  }
  ans
}

x1 <- .25; y1 <- 0.25; x2 <- .75; y2 <- .75

expand.grid(x = seq(0,1,.01), y = seq(0,1,.01)) |>
  tibble::as_tibble() |>
  dplyr::rowwise() |>
  dplyr::mutate(dist_to_line = ifelse(distancePointSegment(x,y,x1,y1,x2,y2) < .1, 1, 0)) |>
  ggplot() +
  geom_point(aes(x, y, color = dist_to_line), shape = 3) +
  coord_fixed() + theme_void() + guides(color = "none")
