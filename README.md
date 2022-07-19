# ***eamenaR*** a R package for front-end statistical analysis <br> of the EAMENA database <img src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/www/eamenaR_logo.png" width='100px' align="right"/>

Install the R package

```
devtools::install_github("eamena-oxford/eamenaR")
```

And load the package

```
library(eamenaR)
```

# Functions

## Plot cultural period from a GeoJSON file

Create a hash dictonnary to store all data

```
d_sql <- hash()
```

List all periods represented in a GeoJSON and plot them

```
d_sql <- list_culturalper(db = "geojson", 
                          d = d_sql, 
                          field = "culturalper", 
                          geojson.path = "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson")
plot_cultural_periods(d = d_sql, field = "period", export.plot = T)
```
The `plot_cultural_periods()` function will export two PNG diagrams for the [caravanserail.geojson](https://github.com/eamena-oxford/eamena-arches-dev/tree/main/data/geojson#readme) file

<p align="center">
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/time/results/cultural_period_byeamenaid.png" width="500">
<br>
and 
<br>
  <img alt="img-name" src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/time/results/cultural_period_histog.png" width="500">
</p>

