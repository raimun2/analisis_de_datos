Redes Neuronales
================

Para el capitulo de redes neuronales vamos a seguir utilizando la data
de Churn, o fuga de clientes. Antes de cargar la data, invocamos la
tibreria Tidyverse. Luego de cargar la data, la limpiamos un poco y le
echamos un vistazo.

``` r
library(tidyverse)

data <- read_csv("../data/Churn_Modelling.csv") %>% 
  mutate(is_female = ifelse(Gender == "Female",1,0),
         Exited = as.factor(Exited)) %>% 
        select(-RowNumber, -Surname, -Geography, -Gender, -CustomerId) %>% 
  relocate(Exited)
 
data %>% glimpse()
```

    ## Rows: 10,000
    ## Columns: 10
    ## $ Exited          <fct> 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, ~
    ## $ CreditScore     <dbl> 619, 608, 502, 699, 850, 645, 822, 376, 501, 684, 528,~
    ## $ Age             <dbl> 42, 41, 42, 39, 43, 44, 50, 29, 44, 27, 31, 24, 34, 25~
    ## $ Tenure          <dbl> 2, 1, 8, 1, 2, 8, 7, 4, 4, 2, 6, 3, 10, 5, 7, 3, 1, 9,~
    ## $ Balance         <dbl> 0.00, 83807.86, 159660.80, 0.00, 125510.82, 113755.78,~
    ## $ NumOfProducts   <dbl> 1, 1, 3, 2, 1, 2, 2, 4, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, ~
    ## $ HasCrCard       <dbl> 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, ~
    ## $ IsActiveMember  <dbl> 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, ~
    ## $ EstimatedSalary <dbl> 101348.88, 112542.58, 113931.57, 93826.63, 79084.10, 1~
    ## $ is_female       <dbl> 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, ~

Vamos a implementar las RRNN utilizando la libreria tidymodels, para
mantener la sintaxis que hemos utilizado hasta ahora

``` r
library(tidymodels)
library(discrim) 
set.seed(42)
data_split <- initial_split(data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
```

    ## [1] 2500

``` r
train_data %>% nrow()
```

    ## [1] 7500

Tambien utilizaremos la funcion fitea que creamos la clase pasada

``` r
fitea <- function(mod){
  
  modelo_fit <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) 

return(model_pred %>% 
  roc_auc(truth = Exited, .pred_0))
}
```

Ahora creamos la receta, igual que los casos anteriores, y finalmente el
modelo de perpectron multicala (multi-layer perceptron o mlp)

``` r
receta <- 
  recipe(Exited ~ ., data = train_data)

receta
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          9

``` r
modelo <- mlp(hidden_units  = 5) %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  translate()


fitea(modelo)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.564

Vemos el AUC es bien deficiente. Esto se debe a que las redes neuronales
necesitan que la data de entrada venga escalada entre -1 y 1, dada la
naturaleza de sus funciones de activacion. agregamos los pasos de
escalamiento en la receta y probamos el mismo modelo.

``` r
receta <- 
  recipe(Exited ~ ., data = train_data) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

receta
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          9
    ## 
    ## Operations:
    ## 
    ## Centering for all_predictors()
    ## Scaling for all_predictors()

``` r
modelo <- mlp(hidden_units  = 5) %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  translate()


fitea(modelo)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.846

Mejoro bastante el AUC, ahora veremos si el numero de neuronas permite
mejorar el resultado obtenido hasta ahora. Para esto, crearemos una
funcion que fitea el modelo dependiendo del numero de neuronas en la
capa oculta.

``` r
fiteaNN <- function(hid){
  
  mod <- mlp(hidden_units  = hid) %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  translate()
  
  modelo_fit <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) 

return(model_pred %>% 
  roc_auc(truth = Exited, .pred_0))
}


fiteaNN(5)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.836

``` r
fiteaNN(6)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.847

``` r
fiteaNN(7)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.842

``` r
fiteaNN(8)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.820

``` r
fiteaNN(9)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.843

``` r
fiteaNN(10)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.843

Vemos que un modelo con 5 capas ocultas obtiene mejores resultados que
otros modelos mas complejos, por lo que deberiamos preservar este modelo
ya que cuenta con menos parametros y por lo tanto es mas estable.

Para configuraciones de redes mas complejas deberiamos utilizar la
libreria tensorflow, la cual dejare de tarea para la casa

``` r
library(tensorflow)
```

    ## Warning: package 'tensorflow' was built under R version 4.0.5

``` r
# https://colorado.rstudio.com/rsc/churn/modeling/tensorflow-w-r.nb.html
```
