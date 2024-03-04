# Práctica2 


|Integrantes                        |          |
|:---------------------------------:|:---------|
|Rodríguez Dimayuga Laura Itzel     | 422013628|
|Figueroa Barrientos Andrea Valeria |          |
|Vázquez Franco Rafael              |          | 


## Funciones (descripción) 

1. Conjunto potencia 

Hicimos una funcion que dentro de la lista por compresion, llama recursivamente a
la funcion y agrega x (el primer elmento), a eso le concatenamos la potencia de 
la lista sin x. 

2. Lista que regresa los numeros de 0 a n 

Solo utilizamos la funcion de listas por compresion normal 

3. Lista que regresa los elementos pares hasta n 

Utilizamos la funcion de listas por compresion, pero utlizamos la funcion `even` que solo regresa los 
pares 

4. Lista que regresa los multiplos de n hasta k

Similar a la de pares, solo que ocupamos el modulo n, que queremos que sea igual a 0. 

5. Lista que regresa la multiplicacion de todos los elementos de una lista con otra lista. 

Agreagamos la multiplicacion de los primeros elementos de la lista y luego hacemos eso con el resto 
de las dos listas. 

## Análisis de complejidad  

1. Como la llamada recursiva se hace por cada subconjunto. Si nuestra lista es de tamaño `n`, 
entonces la complejidad seria $2^n + 2^{n-1} + \cdots + 1 = 2^{n+1}-1$, esto nos deja en $O(2^n)$

2. Es lineal sobre la lista, $O(n)$ 

3. Para todos los elementos de lsita tenemos que verificar si son pares, por lo que es lineal $O(n)$ 

4. Analogamente, tenemos que verificar el modulo para cada elemento, es lineal $O(n)$

5. Tenemos que elevar al cauadrado cada elemento, es lineal $O(n)$ 

6. Cada elemento se multica por uno de la otra lista, por lo que si la longitud de la primera y la tecera es $n$ y $m$ respectivamente entonces la complejidad es $O(n*m)$ 

## Dudas y comentarios 
