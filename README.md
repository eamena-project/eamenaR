# ***eamenaR*** R package <br> for front-end statistical analysis of the EAMENA database <img src="https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/www/eamenaR_logo.png" width='100px' align="right"/>

Install the R package

```
devtools::install_github("eamena-oxford/eamenaR")
```

# Functions

Plot cultural period of a GeoJSON file

```
geojson.path <- "https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/geojson/caravanserail.geojson"
d_sql <- list_culturalper(db = "geojson", d = d_sql, field = "culturalper", geojson.path)
plot_cultural_periods(d = d_sql, field = "period", export.plot = T)
```

Will export two PNG diagrams for the [caravanserail.geojson](https://github.com/eamena-oxford/eamena-arches-dev/tree/main/data/geojson#readme) file

![](https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/time/results/cultural_period_byeamenaid.png)

and 

![](https://raw.githubusercontent.com/eamena-oxford/eamena-arches-dev/main/data/time/results/cultural_period_histog.png)
