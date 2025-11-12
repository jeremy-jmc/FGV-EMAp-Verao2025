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