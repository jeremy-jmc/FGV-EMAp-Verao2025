

# CLASE 1

# CLASE 2

# CLASE 3

```
1. Conjuntos
- C: conjunto de cementos
- R: conjunto de recursos

2. Parametros
- U_i: utilidad de cemento i
- r_ij: # recurso j \in R, para cemento i \in C
- R_j: # max de recurso j \in R

3. Variables de decision
- X_i: # cemento i por producir
```

# CLASE 4

# CLASE 5

Solucion Optima
Factible / Infactible (el area espacio factible es vacio)

Problema no acotado -> (implica) Espacio Factible No Acotado

Multiples soluciones optimas

Forma canonica, forma estandar

https://blog.nekomath.com/investigacion-de-operaciones-forma-canonica-y-forma-estandar-de-un-problema-lineal/

Transformar a negativo para pasar de min a max

Variable Libre == Diferencia entre 2 Variables no negativas

Restricciones redundantes -> Restriccion que no cambia el espacio factible

Buscar todos los sistemas de ecuaciones que te den potenciales puntos extremos


```
M -> kg de maiz
S -> kg de soya

Min C_t = 0.3 * M + 0.9 x S

S.t.
	800 <= M + S
	30/100 (M + S) <= 9/100 * M + 60/100 * S
	2/100 * M + 6/100 * S <= 5/100 (M + S)

M -> kg de maiz
S -> kg de soya

Min C_t = 0.3 * M + 0.9 x S

S.t.
	800 <= M + S
	30 (M + S) <= 9 * M + 60 * S
	2 * M + 6 * S <= 5 (M + S)
	M, S >= 0



Sea i el numero de insumos
Conjunto Kg por Insumo
	I = {I_1, ... , I_i}
Costo de kg por Insumo
	K = {K_1, ... , K_i}
Limite inferior de alimento diario (kg)
	L

Conjunto de Nutrientes por Insumo (Proteinas, Lipidos, Fibras, etc)
Para cada nutriente tiene que estar entre un intervalo (limite superiro e inferior)
	N = {
		N^{1}_{1}, .... , N^{1}_{j}
		...					...
		N^{i}_{1}, .... , N^{i}_{j}
	}
Sea j el numero de nutrientes por insumo
Sea N^{i}_{v} el porcentaje del v-esimo nutriente del insumo "i"

Sea b_v y a_v el limite inferior y superior del nutriente v



Min C_T = \sum_{i} K_i * I_i

S.t.
	L <= \sum_{i} I_i

	para todo 1 <= v <= j
		b_v <= N^{i}_{v} * I_{i} <= a_v

	b_v >= 0 para todo v



Mayusculas para conjuntos
x para variables de decision
ijkv indices y variables mudas


1 Conjuntos
	
	I: Insumos
	N: Nutrientes

2 Parametros
	c_i: costo minimio i \in I
	Q: # total
	p_i_j: propr de nuetirente j \in N en insumo i \in I

	P^{min}_{j}, P^{max}_{j}: limites inferiores y superiores para proporcion de nutriente j \in N

3 Variables de Decision
```

1. Conjuntos
	- $I$: Insumos
	- $N$: Nutrientes

2. Parametros
	- $c_i$: costo minimo $i \in I$
	- $Q$: # total
	- $p_{ij}$: propr de nutriente $j \in N$ en insumo $i \in I$.

	- $P^{min}_{j}, P^{max}_{j}$: limites inferiores y superiores para proporcion de nutriente $j \in N$.

3. Variables de Decision
    - $x_i$: kg de insumo $i \in I$ en la mezcla.

Formulacion del problema:

$$
\begin{align*}
    \text{Minimizar } & Z = \sum_{i \in I} c_i x_i \\
    \text{sujeto a } & \sum_{i \in I} x_i \geq Q \\
    & P^{min}_{j} * \sum_{i \in I} c_i x_i \leq \sum_{i \in I} p_{ij} x_i \leq P^{max}_{j} * \sum_{i \in I} c_i x_i, \quad \forall j \in N \\
    & x_i \geq 0, \quad \forall i \in I
\end{align*}
$$

# CLASE 6

1. Conjuntos:
	- $O$: Objetos
2. Parametros:
	- $c_i$: costo del objeto $i \in O$
	- $w_i$: peso del objeto $i \in O$
	- $W$: peso maximo de la mochila
3. Variables de Decision:
	- $x_i$: variable binaria que indica si el objeto $i \in C$ es seleccionado (1) o no (0).

$$
\begin{align*}
	\text{Maximizar } & Z = \sum_{i \in C} c_i x_i \\
	\text{sujeto a } & \sum_{i \in C} w_i x_i \leq W \\
	& x_i \in \{0, 1\}, \quad \forall i \in C
\end{align*}
$$

TAREA: Problema de particionamiento de multiples conjuntos


# CLASE 7

```
Matriz de numeros enteros: 1 x N

[5, 6, 7, 8, 9, 10]

Matriz de ubicacion entre los grupos
	N x G (grupos = 5)

[
1 0 0 0 0 
0 0 1 0 0
1 0 0 0 0
0 1 0 0 0
0 0 0 1 0
0 0 0 0 1
]

Vector Resultante de la multiplicacion de matrices:
	1 x G -> Representa la suma de elementos asignados a cada grupo

Restricciones
	Las columnas deben sumar minimo 1
	Y las filas deben sumar exactamente 1

Minimizar la diferencia entre el maximo del Vector Resultante y el minimo del Vector Resultante
```


1. Conjuntos:
	- $S$: Conjunto de numeros enteros
	- $G$: Conjunto de grupos
2. Parametros:
3. Variables de Decision:
	- $x_{ij}$: variable binaria que indica si el numero $i \in S$ es asignado al grupo $j \in G$ (1) o no (0).


- Quiz 1 de IO1 la prox sem
	- Moodle -> Safe Exam Browser (Dar Quiz de Prueba)
	- Todo excepto formulacion con variables binarias
		- Revision de los conceptos: puntos extremos, etc

Caso 3: Si se invierte en P1, se debe de invertir en P2
x1 + x2 >= 1

Caso 4: P1 y P2 son incompatibles
x1 + x2 <= 1

Caso 6: Si se invierte en P1, entonces se debe invertir en P2 y P3
x1 <= x2
x1 <= x3



Region Factible - Espacio Factible
Espacio de Busqueda

Algoritmos de Busqueda Local vs Algoritmos Geneticos 

Metodos Heuristicos vs Metaheuristicos

Best improvement
First improvement
