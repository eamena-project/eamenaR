# ***eamenaR*** a R package for front-end statistical analysis <br> of the EAMENA database <img src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/www/eamenaR_logo.png" width='100px' align="right"/>

The ***eamenaR*** allows to analyse GeoJSON files exported from [EAMEANA searches](https://github.com/eamena-oxford/eamena-arches-dev/tree/main/data/geojson#readme), or connect directly the Postgres DB of EAMENA

# Install and load package

Install the R package

```
devtools::install_github("eamena-oxford/eamenaR")
```

And load the package

```
library(eamenaR)
```

# Main functions

## Prepare your data

### Geometries

Return the Grid ID of an Heritage Place by comparing their geometry to a GeoJSON of Grid Squares (gs)

```
library(dplyr)

grid.id <- geom_within_gs(resource.wkt = "POINT(0.9 35.8)")
grid.id
```
Will return `"E00N35-44"`

## Spatial

Distribution map of the default GeoJSON file

```
geojson_map(map.name = "caravanserail", export.plot = T)
```

<p align="center">
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamenaR/main/results/caravanserail.png" width="500">
</p>

## Cultural Periods
### Plot cultural period from a GeoJSON file

Create a hash dictonnary named `d` to store all data

```
library(hash)

d <- hash()
```

Store all periods and subperiods represented in the GeoJSON to the `d` dictonnary, and plot them by EAMENA ID

```
d <- list_cultural_periods(db = "geojson", 
                           d = d)
plot_cultural_periods(d = d, field = "periods", plot.type = "by.eamenaid", export.plot = T)
plot_cultural_periods(d = d, field = "subperiods", plot.type = "by.eamenaid", export.plot = T)
```
Here, the `plot_cultural_periods()` function will export two PNG diagrams for the [caravanserail.geojson](https://github.com/eamena-oxford/eamena-arches-dev/blob/main/data/geojson/caravanserail.geojson) file (by default). These graphics are created by default in the `results/` folder. For periods

<p align="center">
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamenaR/main/results/cultural_periods_byeamenaid.png" width="500">
<br><br>
and superiods
<br><br>
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamenaR/main/results/cultural_subperiods_byeamenaid.png" width="500">
</p>

Periods and subperiods represented in a GeoJSON file can also be summed in a histogram

```
plot_cultural_periods(d = d, field = "subperiods", plot.type = "histogram", export.plot = T)
```
<p align="center">
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamenaR/main/results/cultural_subperiods_histog.png" width="500">
</p>

