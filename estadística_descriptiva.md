Estadística descriptiva
================
Jorge_Montero
2024-07-25

\#Procesar la infromación

``` r
library(openxlsx)
library(dplyr)
```

    ## 
    ## Adjuntando el paquete: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data<-read.csv("C:/Users/Montero/Desktop/Betametrica/Iowa_Liquor_Sales.csv", stringsAsFactors = F,header = T)
datos<-data %>% 
  mutate(Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
         City=toupper(City),
         Store.Name=toupper(Store.Name),
         Date=as.Date(Date,format="%m/%d/%Y"),
         anio=lubridate::year(Date)) %>%  
  rename(ventas=Sale..Dollars., 
         ciudad=City,
         categoria=Category.Name,
         nombre_tienda=Store.Name)

datos %>% group_by(ciudad) %>% 
  summarise(suma=sum(ventas))
```

    ## # A tibble: 3 × 2
    ##   ciudad           suma
    ##   <chr>           <dbl>
    ## 1 CEDAR RAPIDS 2378280.
    ## 2 DAVENPORT    1884349.
    ## 3 WATERLOO     1149095.

\#promedio de ventas por cuidad

``` r
datos %>% group_by(ciudad) %>% 
  summarise(promedio_ventas=mean(ventas))
```

    ## # A tibble: 3 × 2
    ##   ciudad       promedio_ventas
    ##   <chr>                  <dbl>
    ## 1 CEDAR RAPIDS            85.4
    ## 2 DAVENPORT               98.2
    ## 3 WATERLOO               101.

\#Promedio de ventas por sucursal

``` r
datos %>% group_by(nombre_tienda) %>% 
  summarise(promedio_ventas=mean(ventas)) %>% 
  arrange(-promedio_ventas)
```

    ## # A tibble: 188 × 2
    ##    nombre_tienda                      promedio_ventas
    ##    <chr>                                        <dbl>
    ##  1 SAM'S CLUB 6514 / WATERLOO                    516.
    ##  2 SAM'S CLUB 8238 / DAVENPORT                   486.
    ##  3 SAM'S CLUB 8162 / CEDAR RAPIDS                468.
    ##  4 ARTISAN GRAIN DISTILLERY                      419.
    ##  5 HILLTOP GROCERY                               302.
    ##  6 SAM'S FOOD                                    209.
    ##  7 SYCAMORE CONVENIENCE                          187.
    ##  8 FAREWAY STORES #151 / CEDAR RAPIDS            186.
    ##  9 EZ STOP / DAVENPORT                           178.
    ## 10 LOGAN CONVENIENCE STORE                       175.
    ## # ℹ 178 more rows

\#Expandiendo resultados

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
datos %>% 
group_by(ciudad,anio) %>%  #para todaslas variables
summarise_each(.,funs (maximo=max(.,na.rm=T),
                      minimo=min(.,na.rm=T)))
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 9 × 48
    ## # Groups:   ciudad [3]
    ##   ciudad        anio Invoice.Item.Number_maximo Date_maximo Store.Number_maximo
    ##   <chr>        <dbl> <chr>                      <date>                    <int>
    ## 1 CEDAR RAPIDS  2015 S29962800027               2015-12-30                 5207
    ## 2 CEDAR RAPIDS  2016 S34108600005               2016-12-28                 5305
    ## 3 CEDAR RAPIDS  2017 INV-03519900024            2017-02-28                 5341
    ## 4 DAVENPORT     2015 S29969300010               2015-12-30                 5198
    ## 5 DAVENPORT     2016 S34122500013               2016-12-29                 9022
    ## 6 DAVENPORT     2017 INV-03488700052            2017-02-27                 5360
    ## 7 WATERLOO      2015 S29962200091               2015-12-30                 5174
    ## 8 WATERLOO      2016 S34115600001               2016-12-29                 5295
    ## 9 WATERLOO      2017 INV-03526600036            2017-02-28                 5295
    ## # ℹ 43 more variables: nombre_tienda_maximo <chr>, Address_maximo <chr>,
    ## #   Zip.Code_maximo <int>, Store.Location_maximo <chr>,
    ## #   County.Number_maximo <int>, County_maximo <chr>, Category_maximo <int>,
    ## #   categoria_maximo <chr>, Vendor.Number_maximo <int>,
    ## #   Vendor.Name_maximo <chr>, Item.Number_maximo <int>,
    ## #   Item.Description_maximo <chr>, Pack_maximo <int>,
    ## #   Bottle.Volume..ml._maximo <int>, State.Bottle.Cost_maximo <chr>, …

``` r
datos %>% 
group_by(ciudad,anio) %>%  #para la variables ventas
summarise_each(.,funs (maximo=max(.,na.rm=T),
                      minimo=min(.,na.rm=T),
                      media=mean(.,na.rm=T),
                    mediana=median(.,na.rm=T)),ventas) 
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 9 × 6
    ## # Groups:   ciudad [3]
    ##   ciudad        anio maximo minimo media mediana
    ##   <chr>        <dbl>  <dbl>  <dbl> <dbl>   <dbl>
    ## 1 CEDAR RAPIDS  2015  8971.   3.36  90.5    44.0
    ## 2 CEDAR RAPIDS  2016  8250    0     80.7    37.1
    ## 3 CEDAR RAPIDS  2017   297    6.75  67.8    15  
    ## 4 DAVENPORT     2015  7555.   5.18 104.     55.6
    ## 5 DAVENPORT     2016  5882.   3.36  91.8    45  
    ## 6 DAVENPORT     2017   306    6.75  81.5    24.8
    ## 7 WATERLOO      2015  7555.   3.36 104.     51  
    ## 8 WATERLOO      2016  5666.   0     99.7    40.5
    ## 9 WATERLOO      2017   306    6.75  73.3    18.8

``` r
#Pivoteando la tabla
```

``` r
datos %>% 
group_by(ciudad,anio) %>%  #para la variables ventas
summarise_each(.,funs (maximo=max(.,na.rm=T),
                      minimo=min(.,na.rm=T),
                      media=mean(.,na.rm=T),                    mediana=median(.,na.rm=T)),ventas)%>% 
pivot_wider(names_from = ciudad,
            values_from = media,
            values_fill = 0)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 9 × 7
    ##    anio maximo minimo mediana `CEDAR RAPIDS` DAVENPORT WATERLOO
    ##   <dbl>  <dbl>  <dbl>   <dbl>          <dbl>     <dbl>    <dbl>
    ## 1  2015  8971.   3.36    44.0           90.5       0        0  
    ## 2  2016  8250    0       37.1           80.7       0        0  
    ## 3  2017   297    6.75    15             67.8       0        0  
    ## 4  2015  7555.   5.18    55.6            0       104.       0  
    ## 5  2016  5882.   3.36    45              0        91.8      0  
    ## 6  2017   306    6.75    24.8            0        81.5      0  
    ## 7  2015  7555.   3.36    51              0         0      104. 
    ## 8  2016  5666.   0       40.5            0         0       99.7
    ## 9  2017   306    6.75    18.8            0         0       73.3

\#Pivoteando y agregando valores

``` r
datos %>% 
group_by(ciudad,anio) %>%  #para la variables ventas
summarise_each(.,funs (maximo=max(.,na.rm=T),
                      minimo=min(.,na.rm=T),
                      media=mean(.,na.rm=T,trim = 0.5),                          mediana=median(.,na.rm=T)),ventas)%>% 
pivot_wider(names_from = ciudad,
            values_from = c(media,mediana),names_prefix = "empresas_",
            values_fill = 0)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 8 × 9
    ##    anio maximo minimo `media_empresas_CEDAR RAPIDS` media_empresas_DAVENPORT
    ##   <dbl>  <dbl>  <dbl>                         <dbl>                    <dbl>
    ## 1  2015  8971.   3.36                          44.0                      0  
    ## 2  2016  8250    0                             37.1                      0  
    ## 3  2017   297    6.75                          15                        0  
    ## 4  2015  7555.   5.18                           0                       55.6
    ## 5  2016  5882.   3.36                           0                       45  
    ## 6  2017   306    6.75                           0                       24.8
    ## 7  2015  7555.   3.36                           0                        0  
    ## 8  2016  5666.   0                              0                        0  
    ## # ℹ 4 more variables: media_empresas_WATERLOO <dbl>,
    ## #   `mediana_empresas_CEDAR RAPIDS` <dbl>, mediana_empresas_DAVENPORT <dbl>,
    ## #   mediana_empresas_WATERLOO <dbl>

\#Cuantiles

``` r
quantile(datos$ventas,probs = seq(0,1,0.2))
```

    ##      0%     20%     40%     60%     80%    100% 
    ##    0.00   14.99   31.05   80.64  135.00 8970.66

``` r
quantile(datos$ventas)
```

    ##      0%     25%     50%     75%    100% 
    ##    0.00   20.16   44.04  126.00 8970.66

``` r
#ahora usando dplyr
datos %>% 
group_by(ciudad,anio) %>%  #para la variables ventas
summarise_each(.,funs (maximo=max(.,na.rm=T),
                      minimo=min(.,na.rm=T),
                      media=mean(.,na.rm=T,trim = 0.2),
                      p_25=quantile(.,probs=0.25),
                      mediana=median(.,probs=0.5)),ventas)%>%  
                      select(ciudad,media,mediana,anio,maximo,p_25)%>% 
pivot_wider(names_from = ciudad,id_cols=anio,
            values_from = c(media,mediana,p_25),names_prefix = "empresas_",
            values_fill = 0)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 3 × 10
    ##    anio media_empresas_CEDAR RAP…¹ media_empresas_DAVEN…² media_empresas_WATER…³
    ##   <dbl>                      <dbl>                  <dbl>                  <dbl>
    ## 1  2015                       59.0                   68.5                   68.5
    ## 2  2016                       53.3                   64.0                   63.3
    ## 3  2017                       44.8                   62.7                   53.6
    ## # ℹ abbreviated names: ¹​`media_empresas_CEDAR RAPIDS`,
    ## #   ²​media_empresas_DAVENPORT, ³​media_empresas_WATERLOO
    ## # ℹ 6 more variables: `mediana_empresas_CEDAR RAPIDS` <dbl>,
    ## #   mediana_empresas_DAVENPORT <dbl>, mediana_empresas_WATERLOO <dbl>,
    ## #   `p_25_empresas_CEDAR RAPIDS` <dbl>, p_25_empresas_DAVENPORT <dbl>,
    ## #   p_25_empresas_WATERLOO <dbl>

``` r
datos %>% 
 select(ciudad,ventas) %>% 
  group_by(ciudad) %>% 
  mutate(n_tile=ntile(ventas,25)) %>% 
  group_by(ciudad,n_tile) %>% 
  summarise(promedio_ventas=mean(ventas)) %>% 
  pivot_wider(names_from = ciudad,
            values_from = c(promedio_ventas),names_prefix = "empresas_",
            values_fill = 0)
```

    ## `summarise()` has grouped output by 'ciudad'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 25 × 4
    ##    n_tile `empresas_CEDAR RAPIDS` empresas_DAVENPORT empresas_WATERLOO
    ##     <int>                   <dbl>              <dbl>             <dbl>
    ##  1      1                    6.51               7.06              7.13
    ##  2      2                    8.76               9.36             10.2 
    ##  3      3                   10.4               10.7              11.8 
    ##  4      4                   11.8               12.4              13.2 
    ##  5      5                   13.5               14.2              16.0 
    ##  6      6                   17.1               17.5              19.7 
    ##  7      7                   20.1               20.7              22.3 
    ##  8      8                   22.2               24.0              24.8 
    ##  9      9                   24.8               27.3              27.3 
    ## 10     10                   27.6               32.0              30.7 
    ## # ℹ 15 more rows
