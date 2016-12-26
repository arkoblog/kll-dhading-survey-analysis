

# Plot map using leaflet
ddng_poly %>% leaflet() %>% addTiles() %>% addPolygons(popup=~vdc_label)

qtm(ddng_poly)


ddng_poly_agg<-aggregate(x<-ddng_sch["no_students_total"], by=ddng_poly, FUN="length")
tm_shape(ddng_poly_agg)+tm_fill(col = "no_students_total", title="Total Schools Surveyed", palette="Blues") + tm_borders()


ddng_poly_agg %>% leaflet() %>% addTiles() %>% addPolygons(popup=~vdc_label)
pal <- colorNumeric(
  palette = c("green","red"),
  domain = ddng_poly_agg$elev
)

ddng_poly_agg %>% leaflet() %>% addTiles() %>% addPolygons(color = ~pal(elev))
