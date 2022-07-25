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

# Functions

## Geometries

Return the Grid ID of an Heritage Place by comparing their geometry

```
grid.id <- geom_within_gs(resource.wkt = "POINT(0.9 35.8)")
grid.id
```
Will return `"E00N35-44"`
## Cultural Periods
### Plot cultural period from a GeoJSON file

Create a hash dictonnary named `d` to store all data

```
library(hash)

d <- hash()
```

Store all periods represented in the GeoJSON to the `d` dictonnary, and plot the periods

```
d <- list_culturalper(db = "geojson", 
                      d = d, 
                      geojson.path = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson")
plot_cultural_periods(d = d, field = "period", export.plot = T)
```
Here, the `plot_cultural_periods()` function will export two PNG diagrams for the [caravanserail.geojson](https://github.com/eamena-oxford/eamena-arches-dev/blob/main/data/geojson/caravanserail.geojson) file. These files are created by default in the `results/` folder.

<p align="center">
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/time/results/cultural_period_byeamenaid.png" width="500">
<br><br>
and 
<br><br>
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/time/results/cultural_period_histog.png" width="500">
</p>

