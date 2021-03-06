# -*- coding: utf-8 -*-
"""Equipo_Núm_21.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1ewhoDzbjRjEWN3Ud-nMQnKRtmAm5AmV4

*Versión 0.1*

"Análisis de proyecto: **Ventas por inversión en anuncios**"

Elaboró:
- 
-
-
-

# **Ventas por inversión en anuncios**
![](https://images.unsplash.com/photo-1511268559489-34b624fbfcf5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1050&q=80)

# POSTWORK SESIÓN 1

## **Identificación del problema**

En la actualidad, los mecanismos de comunicación han ido evolucionando y con la llegada del internet, la manera en que nos acercamos a las personas ha ido cambiando. Anteriormente, para conocer el resultado de un partido o encontrar trabajo, se tenía que ir a comprar un periódico en los kioskos del metro o en los puestos de revistas, de esta forma podíamos observar las noticias del día anterior, un puesto de trabajo o incluso encontramos anuncios que promocionaban algún producto. De la misma forma, los anuncios llegaban a las personas cuando escuchaba la radio o en dado caso observaba el televisor, incluso en el caso de los conductores, estos llegaban a  promocionaban una marca, pues a partir de una remuneracion económica los conductores aunciaban productos por toda la ciudad con sus autos.

A pesar de que esto ha ido cambiando y ahora muchos negocios pagan a *Google Adworks*, o se promocionan por plataformas como Facebook, los medios tradicionales se mantienen y muchos comercios siguen promocionando su marca y sus productos.

Sin embargo, el costo que se tiene para llevar a cabo las campañas de marketing ya sea por un medio u otro es distinto, sobre todo, por los resultados que estos llegan a tener, cada medio en cuestión de ventas es distintos. Para poder lanzar una campaña es conveniente realizar una investigación personalizada del producto con el que se cuenta y del mercado objetivo, es decir, a quien va dirigido. 

### Usuarios en México

Según datos de la IFT, en México el 92.5% de los hogares, posee almenos un televisor. Así mismo, únicamente el 49% de la población posee un radio en su hogar.

**Referencias:**
- IFT. (2020, 17 febrero). En México hay 80.6 millones de usuarios de internet y 86.5 millones de usuarios de teléfonos celulares: ENDUTIH 2019. 17 de febrero | Instituto Federal de Telecomunicaciones - IFT. Recuperado 18 de febrero de 2021, de https://bit.ly/3dBHGeb.

- Martínez, C. (2019, 26 diciembre). IFT: apenas 39% de los mexicanos escucha radio. Recuperado 18 de febrero de 2021, de https://bit.ly/37zF0JV.

# POSTWORK SESIÓN 2

## **Planteamiento de preguntas**

**Planteamiento del problema**
  * Identificar, con base en un objetivo de ventas, ¿Cuál es el medio de comunicación tradicional en el que es más conveniente invertir? 

Considerando el problema anterior, surgen las siguientes preguntas: 

*   ¿Cuál es el medio de comuniación en el que suele invertirse mayor cantidad de dinero?
*   ¿Se puede considerar únicamente un par de medios que permitan alcanzar el objetivo de ventas?
*   Si solo se quisiera invertir en un solo medio de comunicacion ¿Cuál es medio en el que conviene invertir más?
*   ¿El medio de comunicación, es el mismo en el que la mayoría de las personas invierte en el marketing?, suponiendo que este medio, al ser el que más ventas genera, también podría ser el más costoso, entonces ¿Cuánto se tendría que invertir en los otros dos medios de comunicación para alcanzar el margen de ventas del medio de difusión, para poder saber que más ingresos genera?

# POSTWORK SESIÓN 3

## **Colección de datos**

Para el desarrollo de este proyecto, se hace uso de los datos obtenidos de la plataforma de kaggle, dichos datos poseen información que permitirá responder a las preguntas planteadas con anterioridad.

# POSTWORK SESIÓN 4

## **EDA**

- ¿El conjunto de datos que tengo realmente me sirve para responder algunas de las preguntas que me planté?
  - (Está pregunta considero que se debe de contestar después de responder las preguntas planteadas)
- ¿Qué tamaño tiene mi conjunto de datos? ¿Serán datos suficientes?
  - Las dimensiones del dataset es de 4 columnas, 200 registros.
  - Para poder realizar un análisis demostrativo, son suficientes, sin embargo, para realizar un análisis real, con enfoque a alguna campaña de marketing, sería conveniente contar con una muestra mas grande y algunas variables enfocadas al mercado objetivo y del producto.
- ¿Qué columnas tengo y qué información contiene cada una de esas columnas?
  - Se tienen columnas de los tres distintos medios de comunicación, como son: la TV, Radio y periódico. Cada una de estas columnas contiene información relacionada a la inversión económica realizada. Además, contiene la columna objetivo, que viene a ser las ventas realizadas, en el que marca los beneficios económicos obtenidos después de la inversión realizada.
- Los nombres que tienen mis columnas, ¿son el nombre más apropiado?
  - No, es necesario renombrarlas, para que estén acorde al objeto del análisis.
- ¿Qué tipos de datos tengo en cada columna? ¿Parecen ser el tipo correcto de datos? ¿O es un tipo de datos "incorrecto"?
  - Se tienen datos de tipo flotantes, al ser un formato númerico, es posible trabajar con ellos.
- Si selecciono algunas filas al azar y las observo, ¿estoy obteniendo los datos que debería? ¿O hay datos que parecen estar "sucios" o "incorrectos"?
  - La base de datos se encuentra en un estado óptimo, y aparentemente los datos no presentan errores.
"""

#Importado librerias
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

#Lectura del dataset omitiendo la columna numero 1
Ventas = pd.read_csv("/content/sample_data/Anuncions_ventas.csv", index_col = 0)

dimensiones=Ventas.shape
print('El tamaño del conjunto de los datos es de {} renglones por {} columnas'.format(dimensiones[0],dimensiones[1]))

Ventas.info()

Ventas.head()

#Se renombra columnas
Ventas=Ventas.rename(columns = { 
    "radio": "Radio", 
    "newspaper": "Periodico", 
    "sales": "Ventas"
    } )

"""Para estas celdas, primero se realiza una transformación de los datos, para que con apoyo de los cuartiles, definir niveles de inversion entre bajo, medio y alto."""

def niveles(Ventas):
  '''
  
  Funcion para definir niveles de inversion en cada medio.
  Requiere la libreria numpy y funciona unicamente con el 
  dataset "anuncios_ventas.csv" con las columnas renombradas
  
  '''
  
  Ventas = Ventas.assign(
      TV_nivel = lambda dataframe: dataframe['TV'].map(lambda maximo: "Alto" if maximo >= (np.percentile(Ventas['TV'], 75)) else ("Medio" if maximo >= (np.percentile(Ventas['TV'], 30))   else 'bajo')))
  Ventas=Ventas.assign(
      Periodico_nivel = lambda dataframe: dataframe['Radio'].map(lambda maximo: "Alto" if maximo >= (np.percentile(Ventas['Radio'], 75)) else ("Medio" if maximo >= (np.percentile    (Ventas['Radio'], 30)) else 'bajo')))
  Ventas=Ventas.assign(
      Radio_nivel = lambda dataframe: dataframe['Periodico'].map(lambda maximo: "Alto" if maximo >= (np.percentile(Ventas['Periodico'], 75)) else ("Medio" if maximo >= (np.percentile    (Ventas['Periodico'], 30)) else 'bajo')))
  return Ventas

ventas_tf=niveles(Ventas)
ventas_tf

"""Se realiza un conteo de numeros nulos para conocer con cuantos cuenta el conjunto de datos, a pesar de que esto se observa con la función info, es posible que convenga hacer una descripción para el usuario"""

def nulos(var_nan):
  '''

  Función que realiza el conteo de valores NaN
  
  '''
  suma = sum(var_nan.isna())
  return suma


def impresion_nulos(calc_nan):
  '''

  Función que imprime el numero de NaN, funciona con el 
  dataset "anuncios_ventas.csv" con las columnas renombradas
    
  '''
  
  print('La cantidad de datos nulos en TV es de: {}'.format(calc_nan[0][0]))
  print('La cantidad de datos nulos en Radio es de: {}'.format(calc_nan[0][1]))
  print('La cantidad de datos nulos en Periodico es de: {}'.format(calc_nan[0][2]))

sum_nulos=[]
sum_nulos.append(ventas_tf[['TV',"Radio","Periodico"]].apply(nulos))
impresion_nulos(sum_nulos)

"""Si se requiere conocer los valores medios de de cada una de las columnas de intéres"""

def medias(medio):
  '''
  
  Calculo de las medias

  '''
  media = medio.mean()
  return media


def impresion_media(calc_med):
  '''
  
  Funcion para la impresion de las medias

  '''
  print('La media de inversión en TV es de: {}'.format(list_medias[0][0]))
  print('La media de inversión en Radio es de: {}'.format(list_medias[0][1]))
  print('La media de inversión en Periodico es de: {}'.format(list_medias[0][2]))

list_medias=[]
list_medias.append(ventas_tf[['TV',"Radio","Periodico"]].apply(medias))
impresion_media(list_medias)

"""Al conjunto de ventas creado se le realizá una nueva transformación, que consiste en crear un conjunto de datos nuevo, creando una nueva columna que posee los medios de comunicación."""

transformacion = pd.melt(ventas_tf, id_vars = ['Ventas'], value_vars  = ['TV','Radio','Periodico'],
        var_name = 'Medio', value_name = 'Inversion')

transformacion

g = sns.FacetGrid(transformacion, hue = "Medio", height = 5)
g.map(sns.scatterplot, "Inversion", "Ventas", s = 100, alpha = 0.5)
g.add_legend()

g = sns.FacetGrid(transformacion, col = "Medio")
g.map(sns.scatterplot, "Inversion", "Ventas", alpha = 0.7)
g.add_legend()

sns.pairplot(Ventas, palette = "Set2", height = 2.5)

"""Nuestro dataset se encuentra realmente limpio desde que se descargo en Kaggle, puede servirnos para realizar algunas inferencias, sin embargo, para el proposito de la limpieza de datos es muy pobre. Es por eso que en el extra, presentamos la exploración de un API fue fundamental para poder obtener datos con mayor desorden y limpiarlos, así como para filtrar los mismos.

En conclusión, nuestra dataset podría funcionar para realizar inferencias y constestar las preguntas planteadas, realizando el análisis de marketing, sin embargo, un dataset más grande sin duda sería mucho más funcional. Con respecto la API, presentamos la siguiente sección.

# Exploración de API's

*   Encuentra un API que quieras explorar.  
*   Crea una cuenta si es necesario.
*   Lee la documentación.
*   Realiza algunas peticiones de prueba para entender la estructura de los datos.
*   Automatiza el proceso de realizar peticiones para obtener un dataset considerablemente grande.
*   Explora y limpia tu dataset.
"""

import requests
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

"""Usaremos la API del tiempo, la cual es una API pública que no necesita una llave API y se encuentra albergada [aquí](https://www.metaweather.com/api/) y que fue descargada de esta [lista de APIs públicas](https://github.com/public-apis/public-apis#machine-learning).

A partir de la documentación sabemos que antes de obtener los datos de un lugar debemos obtener código del lugar, obtenidas por la modificación del URL:

*   A partir del nombre de la ciudad

```
/api/location/search/?query=london
```
*   A partir de las coordenadas geográficas

```
/api/location/search/?lattlong=50.068,-5.316
```


Lo primero que debemos realizar es consegir una localización geográfica, ya sea mediante el uso de coordenadas o el nombre de la ciudad. Después, cuando tengamos el código de la ciudad, procederemos a obtener los datos.
"""

# Obtenemos un url para la CDMX
url = 'https://www.metaweather.com/api/location/search/?query=mexico'

# Se realiza el request a la API
r = requests.get(url)

# Verificamos el estado a partir de su código
r.status_code

# Informacion del contenido en bytes
r.content

"""Obtenemos el 'woeid', y de esta forma poder poder obtener los datos del tiempo para fecha especifica. Para acceder a los datos necesitamos modificar el el url de la siguiente forma:

```
/api/location/woeid/año/mes/dia/
```

Por ejemplo, para obtener los datos del 27/4/2013 para CDMX podemos usar la siguiente modificación de URL:
 
```
/api/location/116545/2013/4/27/
```
"""

# Generamos un url para un día especifico

url2 = 'https://www.metaweather.com/api/location/116545/2013/4/27'
r = requests.get(url2)

# Verificamos el estado
r.status_code

# Vemos su contenido
r.content

# Extraemos el json
response = r.json() 
response

"""El tipo de datos no luce como algo parecido a lo visto durante las sesiones, por lo que a partir de un ``` type() ``` podemos determinar el tipo de dato que tenemos, eso nos ayudo a ver que teníamos una lista de diccionarios, y a partir del análisis del primer diccionario obtuvimos las llaves de los datos."""

print(type(response))
response[0].keys()

# Al revisar los datos podemos observar que tenemos una lista de diccionarios con los 
# pronosticos del tiempo para el día que se consulto (27/04/2013). Es simple conversion
datos_normalizados = pd.json_normalize(response)
datos_normalizados

# Obtenemos nuestro DF
df = pd.DataFrame.from_dict(datos_normalizados)
df.head()

"""Observamos que el proceso de obtención de datos es relativamente sencillo. Ahora, nos vamos a plantear un problema. "Supongamos" que los primeros quince días del mes son los más visitados por gente que viene de fuera de la CDMX, esto pues muchos realizan compras para algunos de sus negocios. Por esta razón realizaremos la obtención de datos para los primeros 15 dias de cada mes, del año 2018."""

# A partir de un for loop podemos obtener y pegar los datos solicitados a la API

weather_append = []

for i in range(1,13):
  url = f'https://www.metaweather.com/api/location/116545/2018/{i}/'
  for j in range(1,15):
    # url = f'https://www.metaweather.com/api/location/116545/2018/4/{i}'
    r = requests.get(url+f'{j}')
    response = r.json()
    datos_normalizados = pd.json_normalize(response)
    df = pd.DataFrame.from_dict(datos_normalizados)
  weather_append.append(df)

weater_data = pd.concat(weather_append)

# Ponemos un nuevo index
weater_data = weater_data.reset_index()

# Revisamos como quedó nuestro dataset
weater_data

# Revisamos los valores nulos
weater_data.isnull().sum()

# Para tener un dataframe limpio, eliminamos los nulos
weater_data_dropped = weater_data.dropna()

# Verificamos nuevamente por valores nulos
weater_data_dropped.isnull().sum()

"""## Transformaciones

Se presentan las dimensiones de los datos, utilizando la funcion shape, que retorna un arreglo de una longitud de 2. Se utiliza la funcion format y print para desplegar la información
"""

dimensiones = weater_data_dropped.shape
print( 'El tamaño del conjunto de los datos es de {} renglones por {} columnas'.format(dimensiones[0], dimensiones[1]) )

"""Para realizar algunas transformaciones, primero se visualiza el tipo de datos que se tienen, para posteriormente realizar algunos cambios."""

weater_data_dropped.dtypes

"""Obervamos que la columna 'index' son los índices anteriores a realizar el merge de los datos obtenidos a partir de API. Además, algunos datos **NaN** fueron descartados teniendo que el índice del dataframe no es continuo, por lo que se realizara un reset."""

# Procedemos a eliminar la columna con los índices erroneos
del weater_data_dropped['index']

#realizamos un reset del index pars que correspnda con el numero de datos 
weater_data_dropped = weater_data_dropped.reset_index(drop = True)

weater_data_dropped.head()

"""Para hacer mas homogeneo el datafare, ll texto de la columna 'weather_state_abbr.' se transforma a mayúsculas. De esta forma logramos que el Dataframe sea homogeneo al resto de las columnas con el mismo tipo."""

weater_data_dropped['weather_state_abbr'] = weater_data_dropped['weather_state_abbr'].str.upper()

"""Observamos que las columnas 'created' y 'applicable_date' corresponden a fechas pero estan en un formato de objeto. Es por eso, que se define un diccionario para transformar las dos columnas tipo objeto, que tienen datos correspondientes a las fechas."""

diccionario_de_conversion = {
    'created': 'datetime64[ns]',
    'applicable_date': 'datetime64[ns]'
}

# Realizamos un archivo temporal con la convesion de las columnas dentro del
# diccionario, el cual corresponde a las fechas
temp = weater_data_dropped.astype(diccionario_de_conversion)

"""Se corrobora el cambio de tipo de datos"""

temp.head()

# Vemos los tipos de datos
temp.dtypes

# Reescribimos el dataset
weater_data = temp

"""Se realiza un gráfico para representar los datos numéricos para cada par de columnas, definiendo una función que reciba al dataframe, extraiga las columnas correspondientes a los datos numéricos y su posterior representación."""

def pairplot(df):
  '''

  Función que toma solo los valores numericos en un dataframe para
  así poder realizar un pairplot

  '''
  numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
  newdf = df.select_dtypes(include=numerics)
  colors = ["#5EC1A9"]
  sns.set_palette(sns.color_palette(colors))
  ax=sns.pairplot(newdf,palette="Set3", height=2.5)
  plt.show()

pairplot(weater_data)

"""Se realizan agregaciones, el cálculo de algunas medidas  como son los valores mínimos y maximos de algunas columnas, media y mediana.
Para esto se crea un par de funciones, que realizrán las operaciones.
"""

def toma_col_num(weater):
  '''
  Funcion que las columnas de un dataframe y unicamente
  toma las columnas con valores numéricos
  '''    
  numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
  newdf = weater.select_dtypes(include=numerics)
  newdf = newdf.drop(['id'],axis=1)
  indice = newdf.columns
  return indice,newdf

def agrega(weater):
  '''
  Funcion que calcula los parametos estadísticos de un dataframe o una serie 
  de pandas, lista para usarse con apply. 

  NOTA: si se usa apply, los resultados se guardan en el prímer indice, es 
  decir resultado[0] 
  '''
  media = weater.mean()
  mediana = weater.median()
  maximo = weater.max()
  minimo = weater.min()
  desviacion = weater.std()
  varianza = weater.var()
  agregaciones['media'].append(media)
  agregaciones['mediana'].append(mediana)
  agregaciones['max'].append(maximo)
  agregaciones['min'].append(minimo)
  agregaciones['std'].append(desviacion)
  agregaciones['var'].append(varianza)
  return agregaciones

# Realizamos la aplicacion de funciones para obtener un nuevo DF y sus indices
# de esta forma poder realizar el analisis de los parametros estadisticos
# relacionados a los pronosticos del tiempo

agregaciones = {'media':[],'mediana':[],'max':[],'min':[],'std':[],'var':[]}
indice,newdf = toma_col_num(weater_data_dropped)
agregacion_num = newdf.apply(agrega)

# Realizamos un df para la correcta presentacion de los resultados
agregacion_num = pd.DataFrame(agregacion_num[0],index=indice)
agregacion_num

"""## Agrupamientos y ordenamientos

Se realizan agrupamientos con respecto al estado del tiempo, el día que se realizo el pronostico y el nivel (con respecto al 100) en el que los pronosticadores estuvieron de acuerdo entre sí.
"""

# creamos la consulta
stat_grped_sort = weater_data_dropped.groupby(
    ['weather_state_name', 'applicable_date'])['predictability'].value_counts().sort_values(ascending=False)
# creamos un datafarme de la consulta
stat_grped_sort = stat_grped_sort.to_frame()
# Finalmente hacemos un sort donde de mayor a menor sobre los valores repetidos
stat_grped_sort

"""## Filtros

Finalmente realizaremos un filtro a nuestro dataframe. Relizaremos el filtrado de todas las fechas que tuvieron un pronostico del tiempo con tormentas electricas (Tunder), para eso usaremos su abeviatura (T).
"""

weater_data_dropped[ weater_data_dropped['weather_state_abbr'] == 'T'].head(5)

"""Para hacer mas interesante nuestra consulta, agregaremos al filtro la direccion del viento (wind_direction_compass) y obtendremos todos los valores que sean con direccion al SE y al E. Gurdamos nuestro dataframe."""

tunder_df = weater_data_dropped[ ( weater_data_dropped['weather_state_abbr'] == 'T' ) & 
                    ( (weater_data_dropped['wind_direction_compass'] == 'E') | 
                     (weater_data_dropped['wind_direction_compass'] == 'SE') )
                    ]

"""Finalmente nuestro resultados lo ordenamos por fechas, del más reciente al más antiguo."""

tunder_df_sorted = tunder_df.sort_values('created', ascending = False)
tunder_df_sorted.head(5)

"""De esta forma es como concluimos la extracción de datos de una API, la cual va relacionada desde la consulta de la API a partir de su documentación, acceso a la API, obtención de los datos, limpieza, creación de un dataframe con diferentes datos (para este caso días y meses). El resultado fue un dataframe de los días donde el tiempo presentaba diferentes direcciones del viento (E y SE) y además estos fueron tormentas eléctricas. Seguro que esos días los visitantes de la CDMX pudieron no pasarla del todo bien..."""