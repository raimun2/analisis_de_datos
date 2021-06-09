Maquinas de soporte vectorial
================

Para el capitulo de Maquinas de soporte vectorial vamos a seguir
utilizando la data de Churn, o fuga de clientes. Antes de cargar la
data, invocamos la tibreria Tidyverse. Luego de cargar la data, la
limpiamos un poco y le echamos un vistazo.

``` r
#tidymodels
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

Vamos a implementar las SVM utilizando la libreria tidymodels, para
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

Vamos a crear la receta de la maquina de soporte vectorial, que es igual
a la clase pasada.

Para el modelo utilizamos la funcion svm\_poly que permite crear kernels
polinomicos. Para el caso lineal es equivalente a un polinomio de grado
1.

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
modelo <- svm_poly(degree = 1) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()


modelo
```

    ## Polynomial Support Vector Machine Specification (classification)
    ## 
    ## Main Arguments:
    ##   degree = 1
    ## 
    ## Computational engine: kernlab 
    ## 
    ## Model fit template:
    ## kernlab::ksvm(x = missing_arg(), data = missing_arg(), kernel = "polydot", 
    ##     prob.model = TRUE, kpar = list(degree = ~1))

Ahora probaremos la funcion fitea que creamos la clase pasada con este
modelo polinomico de grado 1 (lineal)

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

fitea(modelo)
```

    ## maximum number of iterations reached 0.01494827 0.01225111

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.659

Ahora modifiquemos la funcion fitea para probar diferentes grados de
polinomio, con grado 1, 2 o 3

``` r
fitea_polySVM <- function(grado){
  
  mod <- svm_poly(degree = grado) %>% 
  set_engine("kernlab") %>% 
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

fitea_polySVM(1)
```

    ## maximum number of iterations reached 0.01370332 0.01124681

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.659

``` r
fitea_polySVM(2)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.809

``` r
fitea_polySVM(3)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.807

Vemos que el modelo de grado 2 tiene un AUC de .80, equivalente al de
grado 3
