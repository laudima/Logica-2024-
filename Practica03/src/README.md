# Práctica1 

|Integrantes                        |          |
|:---------------------------------:|:---------|
|Laura Itzel Rodríguez Dimayuga     | 422013628|
|Andrea Valeria Figueroa Barrientos |          |
|Rafael Vázquez Franco              |          | 

## Funciones (descripción) 
1. Forma Normal Negativa    

Queremos que la negacion solo quede a terminos atomicos por lo que 
hacemos recursion sobre todos los otros casos. 

2. Forma Normal Conjuntiva 

Primero aplicamos la funcion de FNN y luego hacemo una auxiliar dentro de la funcion tambien tenemos una 
funcion para aplicar las leyes de distribucion. 

```
formaNormalConjuntiva = formaNormalConjuntiva' . formaNormalNegativa
```

3. Forma Normal Disyuntiva 

La FND es muy analoga a la FNC solo cambian un poco la manera en como aplicamos distributividad. 

## Análisis de complejidad  

## Dudas o comentarios 