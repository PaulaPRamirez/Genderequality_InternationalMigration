# Genderequality_InternationalMigration
EDA y modelos de cluterización y regresión con R para profundizar en la relación entre igualdad de género y los patrones migratorios internacionales. 


Sobre el proyecto:
-------------------

-   Este proyecto forma parte de la tesina final del máster en Estudios Avanzados en Derechos Humanos, de la UC3M. 
-   El objetivo del análisis es entener la relación multidimensional entre igualdad de género, trabajo y los patrones migratorios de diversas regiones. 


La estructura del directorio es la siguiente: 
- Data: contiene todos los datasets
- Scripts: contiene los scripts de R
- Visualization: contiene las gráficas obtenidas a través del análisis explotatorio de variables así como el mapa realizado con la clusterización. 

En el siguiente enlace encontrarás un panel de visualización que ha sido creado con Power Bi: 

https://app.powerbi.com/links/mtofbEw5LI?ctid=bcd2701c-aa9b-4d12-ba20-f3e3b83070c1&pbi_source=linkShare


Sobre los datos:
-------------------

### Raw data

**GDP.csv**: CSV obtenido del Banco Mundial con el PIB basado en el PPP de 266 países

**Remittances.xlsx**: archivo de excel del Banco Mundial con el porcentaje de remesas en el PIB de 266 países

**Labour_participation.csv**: CSV con la participación laboral de mujeres y hombres de la Organización Mundial del Trabajo. Cada país cuenta con dos filas: una para mujeres y otra para hombres. 

**Labour_participation_bycountry.csv**: CSV simplificado con la participación laboral de mujeres y hombres de la Organización Mundial del Trabajo. Cada país cuenta con dos filas: una para mujeres y otra para hombres. 

**labourforce_female.csv**: CSV obtenido de la OIT con el porcentaje de participación laboral de mujeres respecto al total

**Migrant_stock_age_2019.csv**: CSV descargado de Naciones Unidas en el que se publican estimaciones sobre el número (o stock) de migrantes internacionales, desagregados por edad, sexo y país de origen o destino a través de las estadísticas nacionales, como censos de población. También tiene en cuenta registros y encuestas que proporcionan información sobre el número y la composición de los migrantes internacionales y que mejora la representatividad de los datos. 

**Migrant_stock_origin_2019.csv**: CSV obtenido de Naciones Unidas sobre el stock de migrantes según el país de origen. 

**Violence_against_women.csv**: CSV descargada de la base de datos “Género, Instituciones y Desarrollo” de la OECD, la cual proporciona a los investigadores datos esenciales en discriminación basada en género en las instituciones sociales.

- Leyes hereditarias: nivel de igualdad en derechos de herencia de la tierra y otros bienes, siendo 0 los mismos derechos y 1 una desigualdad reconocida en la ley
- Leyes en violencia contra las mujeres: nivel de protección de la ley, siendo 0 la mayor y 1 una protección nula
- Violencia de género en actitudes: representa el % de muejres entre 15 y 49 que piensa que está justificado que un hombre pegue a su mujer por alguna razón (como quemar la comida, discutir, salir sin avisar, rechazar a los hijos o se niega a tener relaciones sexuales)
- Violence_against_women% : porcentaje de mujeres que han sufrido violencia de género
- FGM_law: legalidad sobre la mutilación genital femenina, siendo 1 legal
- Citizenship_rights: igualdad en las leyes de nacionalidad 
- Iequality_voting_rights: desigualdad en el derecho a voto
- %men_parliament: % de de hombres en la camara baja o única del Parlamento 
- Insecurity_feeling: % de mujeres que piensan que piensan que es inseguro andar sola por la noche en la ciudad o área donde viven
- Access_to_justice: leyes igualitarias de acceso a la justicia
- Child_marriage_law: igual edad de matrimonio o no, siendo 0 igual y 1 desigual
- Girls_married: % de niñas casadas, divorciadas, viudas o en unión informal


### Processed data

Una vez se llevó a cabo la limpieza de los datasets se obtuvieron los siguientes para el análisis y los modelos

**df_countries_total.csv**: con todas las variables del total de países coincidentes Un total de 162 países y 28 variables sobre economía, género y migración. 

**df_migrantes_total.csv**: con el número de emigrantes e inmigrantes por país. Un total de 229 países y 11 variables. 

**df_economy**: con el PIB, remesas y participación laboral de mujeres y hombres por país. Un total de 172 países y 5 variables. 

**df_genderviolence**: con todas las variables de género por país. Un total de 175 países y 15 variables. 

**Migration_regions**: con el número de migrantes por region de destino. Este fue usado posteriormente para las visualizaciones en Power Bi. 


### Final data

Finalmente se creó un excel con cuatro hojas donde se incluyó los datos a limpio del proyecto. Este fue usado para las visualizaciones en Power Bi. 

**Migration_genderinequality.xlsx**: las hojas del excel fueron las siguientes: 

- Economy: incluyendo los datos económicos sobre PIB, remesas y participación laboral. 
- Gender_violence: datos sobre igualdad de género recogidos del SIGI. 
- Migrants: número de inmigrantes y emigrantes por país del año 2019. 
- Total: todas las variables de género, economía y migración de un total de 162 países. 








