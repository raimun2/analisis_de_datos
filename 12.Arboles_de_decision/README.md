12\. Arboles de decision
================

Para el capitulo de Arboles de Decision vamos a seguir utilizando la
data de Churn, o fuga de clientes. Antes de cargar la data, invocamos la
tibreria Tidyverse. Luego de cargar la data, la limpiamos un poco y le
echamos un vistazo.

``` r
#tidymodels
library(tidyverse)

data <- read_csv("Churn_Modelling.csv") %>% 
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

Generamos el mismo dataset de la clase anterior pero con una sola linea
de codigo.

Ahora, para implementar el modelo vamos a utilizar una libreria llamada
tidymodels, que nos permite unificar diferentes librerias de Machine
Learning presentes en R. Tambien cargamos la libreria discrim, la cual
tiene modelos de clasificación complementarios a tidymodels.

El primer paso que hacemos es separar la data en conjunto de
entrenamiento y de prueba, donde tidymodels tiene la funcion
initial\_split

``` r
library(tidymodels)
library(discrim) 

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

Luego, creamos la “receta” del modelo, que consiste en la relacion de
“caja negra” entre las variables de entrada y las variables de salida.
En este caso, la receta será modelar Exited en funcion de todas las
variables presentes en el conjunto de datos.

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

Ahora si creamos el modelo, donde utilizaremos un arbol de decision con
5 capas de decision, y un minimo numero de entidades por hoja (poda) de
10. La libreria que se utiliza para calcular este modelo sera la de
rpart, que viene precargada en los paquetes que estamos utilizando. Con
este paso solo definimos el modelo, aun lo calculamos.

``` r
modelo <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo
```

    ## Decision Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   tree_depth = 5
    ##   min_n = 10
    ## 
    ## Computational engine: rpart

Ahora hacemos el fit del modelo, calculamos sus predicciones y
calculamos el valor de AUC

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

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.764

Ahora veremos la magia de tidymodels, haremos una comparacion con otros
modelos, como el modelo de regresion logistica, naive bayes o Knn. Para
esto, lo unico que debemos cambiar es el modelo, ya que la receta es la
misma, y el flujo de validacion tambien es el mismo. Por lo tanto
podemos utilizar la funcion que creamos mas arriba para evaluar los
diferentes modelos y compararlos.

``` r
modelo_rl <- 
  logistic_reg() %>% 
  set_engine("glm")

fitea(modelo_rl)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.743

``` r
modelo_nb <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("naivebayes")

fitea(modelo_nb)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.846

``` r
modelo_knn <-
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

fitea(modelo_knn)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.772

Podemos ver que en este caso el modelo de Naive Bayes es que obtiene los
mejores resultados al clasificar con un AUC de .84.
