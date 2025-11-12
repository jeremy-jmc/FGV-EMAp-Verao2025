println("Hola todos")
println("Salut tout le monde")

miVariable = 50.0

typeof(miVariable)

ratio1 = 3//4
ratio2 = 1//2
ratio3 = ratio1 + ratio2
println("El ratio 3 es igual a $(ratio3)")



unaCadena = "Bonjour tout le monde"
typeof(unaCadena)

println(unaCadena)

tupla1 = (10, π, "Hola", 3//4)

tupla1[2]

println("Pi = $(pi)")

tupla1[3] = "Bonjour"

tupla1 = (10, π, "Bonjour", 3//4)

unaPersona = (nombre = "Pierre", edad = "25", estado_civil = 'S')

unaPersona[2]
unaPersona[:edad]

arreglo1 = [2,4,6]

typeof(arreglo1)

arreglo1[3] = 3.14

arreglo2 = Float64[1,2,3]

arreglo2[3] = 6

println(arreglo2)

arreglo3 = [10, 3.14, 3//4, "Hola"]

arreglo4 = [3*x for x in 1:10]

typeof(1:10)

rango1 = 1:0.5:10

arreglo5 = collect(rango1)

arregl6 = collect(200:-10:100)

arreglo7 = collect(1:5)

append!(arreglo7, [6,7,8,9])

println(arreglo7)

push!(arreglo7, 10)

println(arreglo7)

pushfirst!(arreglo7, 0)

ultimo_elemento = pop!(arreglo7)

println(ultimo_elemento)

println(arreglo7)

pop!(arreglo7)

println(arreglo7)

primer_elemento = popfirst!(arreglo7)

println(primer_elemento)

println(arreglo7)

sum(arreglo7)

sum([i^3 for i in 1:10])

length(arreglo7)

axes(arreglo7, 1)

arreglo8 = [3,1,7,4,8,6,2,8]

arreglo_ordenado = sort(arreglo8)

println(arreglo_ordenado)

orden_creciente(a, b) = (a < b)

orden_creciente(4, 7)

arreglo_ordenado = sort(arreglo8, lt = orden_creciente)

println(arreglo_ordenado)

orden_decreciente(a, b) = (a > b)

arreglo_orden_decreciente = sort(arreglo8, lt = orden_decreciente)

personas = []
persona1 = (nombre = "Pierre", edad = 25, sueldo = 4567.56)
persona2 = (nombre = "José", edad = 22, sueldo = 7654.56)
persona3 = (nombre = "Elsa", edad = 24, sueldo = 8765.56)
persona4 = (nombre = "Diana", edad = 30, sueldo = 9876.56)
push!(personas, persona1)
push!(personas, persona2)
push!(personas, persona3)
push!(personas, persona4)

# Ordenar el arreglo `personas` en orden decreciente de sueldo.

println("Hola mundo")

orden_creciente_edad(persona_a, persona_b) = (persona_a[:edad] < persona_b[:edad])

personas_orden = sort(personas, lt = orden_creciente_edad)

arreglo2D1 = [i - j for i in 1:10, j in 1:10]

arreglo2D1[5,:]

arreglo2D1[:,5]

arreglo2D1[3:8, 3:8]

personas[end]

arreglo2D1[end, end]

nb_filas, nb_columnas = size(arreglo2D1)

unaMatriz = zeros(5,5)

unaMatrizOnes = ones(5,5)

unaMatriz2 = fill("Hola", 5, 5, 5)

μ = 5

10μ

x = 6

x = [rand() for i in 1:5]

3x[1] + 2x[2] - x[3]

big(3)

factorial(big(1000))

f(x, y) = x^2 + (x * y) + y^2

f(1 + im, 3 + 4im)

f2(x, y) = x^2 * y^2
f2("Hola", "Mundo")

unDiccionario = Dict()
unDiccionario[1] = "José"
unDiccionario[5] = "Diana"

unDiccionario

# tablas hash -> Hash tables o arboles binarios

function crea_diccionario(numero, c)
  diccionario = Dict(i => i^2 + c for i in 1:numero)
  return diccionario
end

crea_diccionario2(numero, c) = Dict(i => i^2 + c for i in 1:numero)

miDiccionario = crea_diccionario2(10, 2)

miDiccionario[5]

miDiccionario[11] = 15

unDiccionario["Albert"] = "Einstein"

unDiccionario

valor = pop!(unDiccionario, "Albert")

println(valor)

unDiccionario

A = Set([1,3,7,9, 6, 4])
B = Set([2,5,7,3,4,8,10])

union(A, B)

mutable struct Persona
  id::Int64
  nombre::String
  edad::Int64
  sueldo::Float64
  telefono::String
end

persona1 = Persona(1, "Pierre", 25, 4567.56, "123456789")
persona2 = Persona(2, "José", 22, 7654.56, "987654321")
persona3 = Persona(3, "Elsa", 24, 8765.56, "456789123")
persona4 = Persona(4, "Diana", 30, 9876.56, "321654987")

personas = [persona1, persona2, persona3, persona4]

persona1.edad

sort(personas, lt = (a, b) -> (a.edad < b.edad))

persona1.telefono = "4567845554"

if persona1.sueldo > persona2.sueldo
  println("$(persona1.nombre) gana más que $(persona2.nombre)")
elseif persona1.sueldo < persona2.sueldo
  println("$(persona2.nombre) gana más que $(persona1.nombre)")
else
  println("$(persona1.nombre) y $(persona2.nombre) ganan lo mismo")
end


function comparar_personas(persona_a, persona_b)
  return persona_a.sueldo < persona_b.sueldo || persona_a.edad < persona_b.edad
end

for persona in personas
  if comparar_personas(persona, persona1)
    println("$(persona.nombre) gana menos o es más joven que $(persona1.nombre)")
  else
    println("$(persona.nombre) no gana menos y no es más joven que $(persona1.nombre)")
  end
end

!(4 < 5)

x = 6

if x ∈ [3,6,8,5, 4]
  println("$(x) está en el arreglo")
else
  println("$(x) no está en el arreglo")
end

'b' ∈ "Ahora"

x = [1,2,3]
!isempty(x)

for i in 1:5, j in 1:4
  println("Turno $i - Paso $j")
  if i + j == 5
    break
  end
end

for i in 1:5, j in 1:4
  if i + j == 5
    continue
  end
  println("Turno $i - Paso $j")
end

count = 1
while count <= 10
  println("Counter = $count")
  count = count + 1
end

