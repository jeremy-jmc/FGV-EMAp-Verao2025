import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from typing import List, Dict, Tuple, Optional
import copy
import math
import random

from models import ProblemInstance, Ruta, Cisterna, Cliente


# -----------------------------------------------------------------------------
# Utility Functions
# -----------------------------------------------------------------------------

def contar_vehiculos(rutas: List[Ruta]) -> Dict[int, int]:
    """Cuenta el número de vehículos usados por tipo."""
    contador = {1: 0, 2: 0}
    for ruta in rutas:
        contador[ruta.cisterna.tipo] += 1
    return contador


def build_product_map(clientes_list: List[int], route_k: Ruta, route_k_plus_1: Ruta) -> Dict[int, List[str]]:
    """
    Build a mapping of customer IDs to their delivered products from two routes.
    """
    m: Dict[int, List[str]] = {}
    for cid in clientes_list:
        if cid in route_k.productos_entregados:
            m[cid] = route_k.productos_entregados[cid]
        elif cid in route_k_plus_1.productos_entregados:
            m[cid] = route_k_plus_1.productos_entregados[cid]
    return m


# -----------------------------------------------------------------------------
# Route Evaluation and Utilities
# -----------------------------------------------------------------------------

class RouteEvaluator:
    """Evalúa y valida rutas."""
    
    def __init__(self, instance: ProblemInstance):
        self.instance = instance
    
    def calcular_tiempo_servicio(self, productos: List[str]) -> float:
        """
        Calcula el tiempo de servicio en un cliente según los productos entregados.
        
        Args:
            productos: Lista de productos entregados ['G'] o ['D'] o ['G', 'D']
        
        Returns:
            Tiempo total de servicio en minutos
        """
        return len(productos) * self.instance.tiempo_descarga
    
    def verificar_factibilidad_ruta(self, ruta: List[int], cisterna: Cisterna,
                                   productos_por_cliente: Dict[int, List[str]]) -> Tuple[bool, float, Dict]:
        """
        Verifica si una ruta es factible respecto a capacidad y ventanas de tiempo.
        
        Args:
            ruta: Lista de IDs de clientes en orden de visita
            cisterna: Cisterna asignada a la ruta
            productos_por_cliente: Dict {cliente_id: ['G', 'D', ...]}
        
        Returns:
            (factible, tiempo_total, info_detallada)
        """
        # Verificar capacidad
        carga_gasohol = 0
        carga_diesel = 0
        
        for cliente_id in ruta:
            cliente = self.instance.cliente_por_id(cliente_id)
            if 'G' in productos_por_cliente[cliente_id]:
                carga_gasohol += cliente.demanda_gasohol
            if 'D' in productos_por_cliente[cliente_id]:
                carga_diesel += cliente.demanda_diesel
        
        if carga_gasohol > cisterna.cap_gasohol or carga_diesel > cisterna.cap_diesel:
            return False, 0, {'razon': 'capacidad_excedida'}
        
        # Verificar ventanas de tiempo
        tiempo_actual = self.instance.depot.ventana_inicio
        nodo_actual = 0  # Depot
        
        tiempos_llegada = {}
        
        for cliente_id in ruta:
            cliente = self.instance.cliente_por_id(cliente_id)
            
            # Tiempo de viaje al siguiente cliente
            tiempo_viaje = self.instance.tiempo_viaje(nodo_actual, cliente_id)
            tiempo_llegada = tiempo_actual + tiempo_viaje
            
            # Si llegamos antes de la ventana, esperamos
            if tiempo_llegada < cliente.ventana_inicio:
                tiempo_llegada = cliente.ventana_inicio
            
            # Si llegamos después de la ventana, no es factible
            if tiempo_llegada > cliente.ventana_fin:
                return False, 0, {'razon': 'ventana_tiempo_violada', 'cliente': cliente_id}
            
            tiempos_llegada[cliente_id] = tiempo_llegada
            
            # Tiempo de servicio
            tiempo_servicio = self.calcular_tiempo_servicio(productos_por_cliente[cliente_id])
            
            tiempo_actual = tiempo_llegada + tiempo_servicio
            nodo_actual = cliente_id
        
        # Retorno al depot
        tiempo_viaje_retorno = self.instance.tiempo_viaje(nodo_actual, 0)
        tiempo_retorno = tiempo_actual + tiempo_viaje_retorno
        
        if tiempo_retorno > self.instance.depot.ventana_fin:
            return False, 0, {'razon': 'retorno_tardio'}
        
        return True, tiempo_retorno, {
            'carga_gasohol': carga_gasohol,
            'carga_diesel': carga_diesel,
            'tiempos_llegada': tiempos_llegada,
            'tiempo_retorno': tiempo_retorno
        }
    
    def calcular_distancia_ruta(self, ruta: List[int]) -> float:
        """Calcula la distancia total de una ruta incluyendo ida y vuelta al depot."""
        if len(ruta) == 0:
            return 0
        
        distancia = self.instance.distancia(0, ruta[0])  # Depot al primer cliente
        
        for i in range(len(ruta) - 1):
            distancia += self.instance.distancia(ruta[i], ruta[i + 1])
        
        distancia += self.instance.distancia(ruta[-1], 0)  # Último cliente al depot
        
        return distancia
    
    def seleccionar_mejor_cisterna(self, ruta: List[int], 
                                   productos_por_cliente: Dict[int, List[str]],
                                   vehiculos_usados: Dict[int, int]) -> Optional[Cisterna]:
        """
        Selecciona la cisterna de menor costo que puede satisfacer la ruta.
        
        Returns:
            Cisterna seleccionada o None si ninguna es factible
        """
        cisternas_factibles = []
        
        for cisterna in self.instance.cisternas_disponibles:
            # Verificar si hay vehículos disponibles de este tipo
            if vehiculos_usados.get(cisterna.tipo, 0) >= self.instance.num_vehiculos_por_tipo:
                continue
            
            factible, _, _ = self.verificar_factibilidad_ruta(ruta, cisterna, productos_por_cliente)
            if factible:
                distancia = self.calcular_distancia_ruta(ruta)
                costo = cisterna.costo_fijo + cisterna.costo_km * distancia
                cisternas_factibles.append((cisterna, costo))
        
        if not cisternas_factibles:
            return None
        
        # Ordenar por costo y retornar la más económica
        cisternas_factibles.sort(key=lambda x: x[1])
        return cisternas_factibles[0][0]
    
    def crear_ruta_objeto(self, clientes: List[int], cisterna: Cisterna, 
                         productos_por_cliente: Dict[int, List[str]]) -> Ruta:
        """Crea un objeto Ruta completo con todos sus atributos calculados."""
        factible, tiempo_total, info = self.verificar_factibilidad_ruta(
            clientes, cisterna, productos_por_cliente
        )
        distancia = self.calcular_distancia_ruta(clientes)
        costo = cisterna.costo_fijo + cisterna.costo_km * distancia
        
        tiempos_llegada = [info['tiempos_llegada'][cid] for cid in clientes]
        
        return Ruta(
            cisterna=cisterna,
            clientes=clientes,
            carga_gasohol=info['carga_gasohol'],
            carga_diesel=info['carga_diesel'],
            distancia_total=distancia,
            tiempo_total=tiempo_total,
            costo_total=costo,
            factible=factible,
            productos_entregados=productos_por_cliente,
            tiempos_llegada=tiempos_llegada
        )


# -----------------------------------------------------------------------------
# Sweep Algorithm for Initial Solution Construction
# -----------------------------------------------------------------------------

class SweepAlgorithm:
    """
    Implements the Angular Sweep Algorithm for initial solution construction.
    Based on Gillett & Miller (1974) - 'A Heuristic Algorithm for the Vehicle-Dispatch Problem'
    """
    
    def __init__(self, instance: ProblemInstance):
        """
        Inicializa el algoritmo con una instancia del problema.
        
        Args:
            instance: Instancia del problema con todos los datos necesarios
        """
        self.instance = instance
        self.evaluator = RouteEvaluator(instance)
        self.best_solution = None
    
    def forward_sweep(self) -> List[Ruta]:
        """
        Algoritmo Forward Sweep.
        Particiona los clientes en rutas comenzando desde el cliente con menor ángulo.
        
        Implementación del algoritmo Angular Sweep de Gillett & Miller (1974) adaptado para VRP con múltiples compartimentos y ventanas de tiempo.
            'A Heuristic Algorithm for the Vehicle-Dispatch Problem'

                The problem is to determine the number of routes and the paths in each rout that will minimize the total distance traveled by all vehicles in supplying all demands, subject to the lead and distance constraints on each vehicle.

                The distance, constraint could be replaced by a time constraint without changing the problem.

                The sweep algorithm divides the locations into a number of routes and then operates on the individual routes unitl an optimum or near-optimum solution is obtained.

                The vehicle-dispatch problem is to minimize the total distance traveled supplying all demands while satisfying all constraints

                The sweep algorithm consists of two parts: a forward sweep and a backward sweep.

                    In the forward-sweep algorithm, the locations are partitioned into routes beginning with the location that has the smallest angle, namely, location 1. 
                    Recall that the locations were renumbered according to the size of their polar-coordinate angle and the depot is location 0. 
                    The first route consists of locations 1, 2, ..., I, where I is the last location that can be added without exceeding the vehicle capacity or distance constraint. 
                    The second route contains locations J+1, J+2, ..., L, where L is the last location that can be added to the second route without exceeding the vehicle capacity or distance constraint. 
                    The remaining routes are formed in the same manner. 
                    The total distance traveled then is just the sum of distances for each route.

                Then, a 2-opt type of improvement procedure is applied to the set of routes obtained from the forward sweep. 
                The procedure to modify consider replacing one location in route K with one or more locations in route K + 1
                This improvement process is continued in the clockwise and counterclockwise directions until no further improvement is possible.

                The backward-sweep algorithm is similar to the forward-sweep algorithm except that the locations are considered in the reverse order.

        Returns:
            Lista de rutas generadas
        """
        rutas = []
        clientes_no_asignados = set(range(1, self.instance.n + 1))
        demandas_pendientes = {
            c.id: {'G': c.demanda_gasohol > 0, 'D': c.demanda_diesel > 0} 
            for c in self.instance.clientes
        }
        vehiculos_usados = {1: 0, 2: 0}
        
        while clientes_no_asignados:
            ruta_actual = []
            productos_ruta = {}
            
            for cliente_id in sorted(clientes_no_asignados, 
                                    key=lambda c: self.instance.cliente_por_id(c).angulo):
                
                cliente = self.instance.cliente_por_id(cliente_id)
                
                # Determinar qué productos entregar
                productos = []
                if demandas_pendientes[cliente_id]['G']:
                    productos.append('G')
                if demandas_pendientes[cliente_id]['D']:
                    productos.append('D')
                
                if not productos:
                    continue
                
                # Intentar agregar cliente a la ruta
                ruta_tentativa = ruta_actual + [cliente_id]
                productos_tentativa = productos_ruta.copy()
                productos_tentativa[cliente_id] = productos
                
                cisterna_factible = self.evaluator.seleccionar_mejor_cisterna(
                    ruta_tentativa, productos_tentativa, vehiculos_usados
                )
                
                if cisterna_factible:
                    ruta_actual = ruta_tentativa
                    productos_ruta = productos_tentativa
                else:
                    # Intentar solo con un producto si es posible
                    if len(productos) == 2:
                        for prod in ['G', 'D']:
                            productos_tentativa[cliente_id] = [prod]
                            cisterna = self.evaluator.seleccionar_mejor_cisterna(
                                ruta_tentativa, productos_tentativa, vehiculos_usados
                            )
                            if cisterna:
                                ruta_actual = ruta_tentativa
                                productos_ruta = productos_tentativa
                                break
                        else:
                            break  # No cabe ni con un solo producto
                    else:
                        break  # No se puede agregar más
            
            # Crear objeto Ruta
            if ruta_actual:
                cisterna = self.evaluator.seleccionar_mejor_cisterna(
                    ruta_actual, productos_ruta, vehiculos_usados
                )
                
                if cisterna is None:
                    raise ValueError(
                        f"No hay vehículos disponibles. Uso: Tipo 1: {vehiculos_usados[1]}/{self.instance.num_vehiculos_por_tipo}, "
                        f"Tipo 2: {vehiculos_usados[2]}/{self.instance.num_vehiculos_por_tipo}"
                    )
                
                ruta_obj = self.evaluator.crear_ruta_objeto(ruta_actual, cisterna, productos_ruta)
                rutas.append(ruta_obj)
                vehiculos_usados[cisterna.tipo] += 1
                
                # Actualizar estado
                for cliente_id, productos in productos_ruta.items():
                    if 'G' in productos:
                        demandas_pendientes[cliente_id]['G'] = False
                    if 'D' in productos:
                        demandas_pendientes[cliente_id]['D'] = False
                    
                    if not any(demandas_pendientes[cliente_id].values()):
                        clientes_no_asignados.discard(cliente_id)
            else:
                raise ValueError(f"No se pudo asignar clientes restantes: {clientes_no_asignados}")
        
        print(f"\n>>> Vehículos utilizados en Forward Sweep:")
        print(f"  * Tipo 1: {vehiculos_usados[1]}/{self.instance.num_vehiculos_por_tipo}")
        print(f"  * Tipo 2: {vehiculos_usados[2]}/{self.instance.num_vehiculos_por_tipo}")
        
        self.best_solution = copy.deepcopy(rutas)
        return rutas
    
    def _calcular_score_eliminacion(self, cliente_id: int, avg_radius: float) -> float:
        """Score para seleccionar cliente a eliminar: R(I) + An(I) * AVR."""
        cliente = self.instance.cliente_por_id(cliente_id)
        return cliente.radio + cliente.angulo * avg_radius
    
    def _seleccionar_cliente_a_eliminar(self, ruta: Ruta, avg_radius: float) -> Optional[int]:
        """Selecciona el cliente de la ruta que minimiza el score de eliminación."""
        if not ruta.clientes:
            return None
        
        return min(ruta.clientes, 
                  key=lambda cid: self._calcular_score_eliminacion(cid, avg_radius))
    
    def _obtener_candidatos_ordenados(self, ruta_k: Ruta, ruta_k_plus_1: Ruta) -> List[int]:
        """Obtiene clientes de K+1 ordenados por cercanía al último cliente de K."""
        if not ruta_k.clientes or not ruta_k_plus_1.clientes:
            return []
        
        ultimo_cliente_k = ruta_k.clientes[-1]
        return sorted(ruta_k_plus_1.clientes,
                     key=lambda c: self.instance.distancia(ultimo_cliente_k, c))
    
    def _intentar_insercion_greedy(self, base_clientes: List[int], 
                                  clientes_a_insertar: List[int], 
                                  ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                  vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[List[int], Cisterna, Dict, float]]:
        """Inserta clientes en la ruta base usando estrategia greedy."""
        ruta_actual = base_clientes[:]
        cisterna_actual = None
        info_actual = None
        costo_actual = None
        
        for cliente_id in clientes_a_insertar:
            mejor_insercion = self._encontrar_mejor_posicion_insercion(
                ruta_actual, cliente_id, ruta_k, ruta_k_plus_1, vehiculos_disponibles
            )
            
            if mejor_insercion is None:
                return None
            
            pos, cisterna, info, costo = mejor_insercion
            ruta_actual = ruta_actual[:pos] + [cliente_id] + ruta_actual[pos:]
            cisterna_actual = cisterna
            info_actual = info
            costo_actual = costo
        
        return (ruta_actual, cisterna_actual, info_actual, costo_actual)
    
    def _encontrar_mejor_posicion_insercion(self, ruta_actual: List[int], 
                                           cliente_id: int, 
                                           ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                           vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[int, Cisterna, Dict, float]]:
        """Encuentra la mejor posición para insertar un cliente en una ruta."""
        mejor_pos, mejor_costo = None, float('inf')
        mejor_cisterna, mejor_info = None, None
        
        for pos in range(len(ruta_actual) + 1):
            tentativa = ruta_actual[:pos] + [cliente_id] + ruta_actual[pos:]
            productos = build_product_map(tentativa, ruta_k, ruta_k_plus_1)
            
            cisterna = self.evaluator.seleccionar_mejor_cisterna(
                tentativa, productos, vehiculos_disponibles
            )
            if cisterna is None:
                continue
            
            factible, _, info = self.evaluator.verificar_factibilidad_ruta(
                tentativa, cisterna, productos
            )
            if not factible:
                continue
            
            distancia = self.evaluator.calcular_distancia_ruta(tentativa)
            costo = cisterna.costo_fijo + cisterna.costo_km * distancia
            
            if costo < mejor_costo:
                mejor_pos = pos
                mejor_costo = costo
                mejor_cisterna = cisterna
                mejor_info = info
        
        return (mejor_pos, mejor_cisterna, mejor_info, mejor_costo) if mejor_pos is not None else None
    
    def _reconstruir_ruta_k_plus_1(self, base_clientes: List[int], 
                                   cliente_a_insertar: int, 
                                   ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                   vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[List[int], Cisterna, Dict, float]]:
        """Reconstruye la ruta K+1 insertando un cliente eliminado de K."""
        mejor_pos, mejor_costo = None, float('inf')
        mejor_cisterna, mejor_info, mejor_ruta = None, None, None
        
        for pos in range(len(base_clientes) + 1):
            tentativa = base_clientes[:pos] + [cliente_a_insertar] + base_clientes[pos:]
            productos = build_product_map(tentativa, ruta_k, ruta_k_plus_1)
            
            cisterna = self.evaluator.seleccionar_mejor_cisterna(
                tentativa, productos, vehiculos_disponibles
            )
            if cisterna is None:
                continue
            
            factible, _, info = self.evaluator.verificar_factibilidad_ruta(
                tentativa, cisterna, productos
            )
            if not factible:
                continue
            
            distancia = self.evaluator.calcular_distancia_ruta(tentativa)
            costo = cisterna.costo_fijo + cisterna.costo_km * distancia
            
            if costo < mejor_costo:
                mejor_pos = pos
                mejor_costo = costo
                mejor_cisterna = cisterna
                mejor_info = info
                mejor_ruta = tentativa
        
        return (mejor_ruta, mejor_cisterna, mejor_info, mejor_costo) if mejor_pos is not None else None
    
    def _buscar_mejor_intercambio(self, ruta_k: Ruta, ruta_k_plus_1: Ruta, 
                                  cliente_eliminar: int, candidatos: List[int], 
                                  vehiculos_disponibles: Dict[int, int]) -> Optional[Tuple[List[int], Cisterna, Dict, List[int], Cisterna, Dict, float]]:
        """Busca el mejor intercambio de clientes entre dos rutas consecutivas."""
        base_k = [c for c in ruta_k.clientes if c != cliente_eliminar]
        costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total
        
        # Liberar temporalmente vehículos de ambas rutas
        veh_temp = vehiculos_disponibles.copy()
        veh_temp[ruta_k.cisterna.tipo] -= 1
        veh_temp[ruta_k_plus_1.cisterna.tipo] -= 1
        
        # Intentar con prefijos crecientes de candidatos
        for m in range(1, len(candidatos) + 1):
            prefijo = candidatos[:m]
            
            resultado_k = self._intentar_insercion_greedy(
                base_k, prefijo, ruta_k, ruta_k_plus_1, veh_temp
            )
            
            if resultado_k is None:
                continue
            
            nueva_k, cisterna_k, info_k, costo_k = resultado_k
            
            veh_temp2 = veh_temp.copy()
            veh_temp2[cisterna_k.tipo] += 1
            
            base_k1 = [c for c in ruta_k_plus_1.clientes if c not in prefijo]
            resultado_k1 = self._reconstruir_ruta_k_plus_1(
                base_k1, cliente_eliminar, ruta_k, ruta_k_plus_1, veh_temp2
            )
            
            if resultado_k1 is None:
                continue
            
            nueva_k1, cisterna_k1, info_k1, costo_k1 = resultado_k1
            nuevo_costo = costo_k + costo_k1
            
            if nuevo_costo < costo_actual:
                return (nueva_k, cisterna_k, info_k, nueva_k1, cisterna_k1, info_k1, nuevo_costo)
        
        return None
    
    def _crear_ruta_desde_swap(self, clientes: List[int], cisterna: Cisterna, 
                               info: Dict, ruta_original_k: Ruta, 
                               ruta_original_k_plus_1: Ruta) -> Ruta:
        """Crea un objeto Ruta a partir del resultado de un intercambio."""
        productos = build_product_map(clientes, ruta_original_k, ruta_original_k_plus_1)
        return self.evaluator.crear_ruta_objeto(clientes, cisterna, productos)

    def improving_sweep(self, rutas_candidatas: List[Ruta], 
                       clockwise: bool = False) -> Tuple[List[Ruta], bool]:
        """
        Implementa el algoritmo de mejora del Forward Angular Sweep.

        Considera reemplazar un cliente en la ruta K con uno o más clientes de la ruta K+1.
        Un reemplazo se realiza solo si reduce el costo y ambas rutas permanecen factibles.

            The procedure to modify consider replacing one location in route K with one or more locations in route K + 1 for K = 1, 2, ..., m - 1, where m is the number of routes formed.
            A replacement is made only if the cost of the two routes after the replacement is less than the cost before the replacement and both routes remain feasible after the replacement.

            The location to be deleted from route K is obtained by minimizing a function of the radius R(I) and the angle An(I) of each location in route K.
            This provides a location that is close to the depot and also close to the next route. A function that works very well is R(I) + An(I) * AVR (Average Radius among all locations).

            The first location, say location p, that is considered for inclusion in route K is the location in route K + 1 that is nearest to the last location that was added to route K. 
            The second location considered for inclusion in route K is the location in route K + 1 that is nearest to location p.
            If one or more locations are added to route K by this scheme, then the next location in route K + 1 is also checked to see if it can be included in route K.
            
            The process of adding one or more locations to route K and deleting another location continues until no further improvement is found.

        Args:
            rutas_iniciales: Lista de rutas iniciales
            clockwise: Si True, procesa las rutas en orden inverso (sentido horario)
        
        Returns:
            (rutas_mejoradas, hubo_mejora): Tupla con las rutas mejoradas y un flag indicando si hubo mejora
        """
        avg_radius = np.mean([c.radio for c in self.instance.clientes])
        
        rutas_mejoradas = copy.deepcopy(rutas_candidatas)
        if clockwise:
            rutas_mejoradas = list(reversed(rutas_mejoradas))
        
        vehiculos_usados = contar_vehiculos(rutas_mejoradas)
        iteracion = 0
        hubo_alguna_mejora = False
        seguir_mejorando = True
        
        while seguir_mejorando:
            seguir_mejorando = False
            iteracion += 1

            for k in range(len(rutas_mejoradas) - 1):
                mejora_local = True
                
                while mejora_local:
                    mejora_local = False
                    
                    ruta_k = rutas_mejoradas[k]
                    ruta_k_plus_1 = rutas_mejoradas[k + 1]
                    costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total

                    cliente_eliminar = self._seleccionar_cliente_a_eliminar(ruta_k, avg_radius)
                    if cliente_eliminar is None:
                        break

                    candidatos = self._obtener_candidatos_ordenados(ruta_k, ruta_k_plus_1)
                    if not candidatos:
                        break

                    mejor_swap = self._buscar_mejor_intercambio(
                        ruta_k, ruta_k_plus_1, cliente_eliminar, candidatos, vehiculos_usados
                    )

                    if mejor_swap is None:
                        break

                    nueva_k, cisterna_k, info_k, nueva_k1, cisterna_k1, info_k1, nuevo_costo = mejor_swap

                    nueva_ruta_k = self._crear_ruta_desde_swap(nueva_k, cisterna_k, info_k, ruta_k, ruta_k_plus_1)
                    nueva_ruta_k_plus_1 = self._crear_ruta_desde_swap(nueva_k1, cisterna_k1, info_k1, ruta_k, ruta_k_plus_1)

                    # Actualizar contador de vehículos
                    vehiculos_usados[ruta_k.cisterna.tipo] -= 1
                    vehiculos_usados[ruta_k_plus_1.cisterna.tipo] -= 1
                    vehiculos_usados[cisterna_k.tipo] += 1
                    vehiculos_usados[cisterna_k1.tipo] += 1

                    rutas_mejoradas[k] = nueva_ruta_k
                    rutas_mejoradas[k + 1] = nueva_ruta_k_plus_1

                    mejora_local = True
                    seguir_mejorando = True
                    hubo_alguna_mejora = True
                    
                    direccion_str = "horario" if clockwise else "antihorario"
                    ahorro = costo_actual - nuevo_costo
                    print(f"  [Iteración {iteracion} - {direccion_str}] Mejora rutas {k}-{k+1}: "
                          f"${costo_actual:.2f} → ${nuevo_costo:.2f} (ahorro: ${ahorro:.2f})")

        direccion_str = "horario" if clockwise else "antihorario"
        print(f"\n>>> Proceso de mejora ({direccion_str}) completado en {iteracion} iteraciones.")
        print(f">>> Vehículos utilizados:")
        print(f"  * Tipo 1: {vehiculos_usados[1]}/{self.instance.num_vehiculos_por_tipo}")
        print(f"  * Tipo 2: {vehiculos_usados[2]}/{self.instance.num_vehiculos_por_tipo}")

        if clockwise:
            rutas_mejoradas = list(reversed(rutas_mejoradas))
        if hubo_alguna_mejora:
            self.best_solution = copy.deepcopy(rutas_mejoradas)

        return rutas_mejoradas, hubo_alguna_mejora

    def iterative_improving_sweep(self, rutas_candidatas: List[Ruta]) -> List[Ruta]:
        """
        Ejecuta improving_sweep alternando entre sentido antihorario y horario hasta que ambas direcciones no produzcan mejoras.
        
        Según Gillett & Miller (1974): "The X and Y axes are then rotated counterclockwise or in the first location (counterclockwise is to the left). The procedure is then  repeated. The process of rotating the X and Y axes is continued until all possibilities have been exhausted."
        
        Args:
            rutas_iniciales: Lista de rutas iniciales del forward_sweep
        
        Returns:
            Lista de rutas mejoradas después de agotar ambas direcciones
        """

        rutas_actuales = rutas_candidatas
        mejora_global = True
        iteracion_global = 0

        print("\n" + "="*80)
        print("INICIANDO MEJORA ITERATIVA (CLOCKWISE Y COUNTERCLOCKWISE)")
        print("="*80)
        
        while mejora_global:
            iteracion_global += 1
            mejora_global = False
            
            print(f"\n>>> Iteración global {iteracion_global}")
            
            # Sentido antihorario
            print("\n  >>> Intentando mejora en sentido ANTIHORARIO...")
            rutas_ccw, mejora_ccw = self.improving_sweep(rutas_actuales, clockwise=False)
            
            if mejora_ccw:
                rutas_actuales = rutas_ccw
                mejora_global = True
                costo_actual = sum(r.costo_total for r in rutas_actuales)
                print(f"  >>> Mejora encontrada. Costo actual: ${costo_actual:,.2f}")
            else:
                print(f"  >>> Sin mejoras")
            
            # Sentido horario
            print("\n  >>> Intentando mejora en sentido HORARIO...")
            rutas_cw, mejora_cw = self.improving_sweep(rutas_actuales, clockwise=True)
            
            if mejora_cw:
                rutas_actuales = rutas_cw
                mejora_global = True
                costo_actual = sum(r.costo_total for r in rutas_actuales)
                print(f"  >>> Mejora encontrada. Costo actual: ${costo_actual:,.2f}")
            else:
                print(f"  >>> Sin mejoras")
            
            if not mejora_ccw and not mejora_cw:
                print(f"\n>>> No se encontraron más mejoras en ninguna dirección.")
                mejora_global = False
        
        print("\n" + "="*80)
        print(f"MEJORA ITERATIVA COMPLETADA EN {iteracion_global} ITERACIONES GLOBALES")
        print("="*80)
        
        self.best_solution = copy.deepcopy(rutas_actuales)
        return rutas_actuales


# -----------------------------------------------------------------------------
# Tabu Search Solver
# -----------------------------------------------------------------------------

class SolverTabuSearchMCVRPTW:
    """
    Solver for Multi-Compartment Vehicle Routing Problem with Time Windows using Tabu Search.
    Based on Silvestrin (2017) - 'An Iterated Tabu Search for Multi-Compartment Vehicle Routing Problem'
    """
    
    def __init__(self, sweep_solver: SweepAlgorithm):
        """
        Inicializa el solver de Tabu Search.
        
        Args:
            sweep_solver: Instancia de SweepAlgorithm para reutilizar funciones auxiliares
        """
        self.sweep = sweep_solver
        self.best_solution = copy.deepcopy(sweep_solver.best_solution)
    
    def _build_p_neighborhoods(self, clientes: List[int], p: int) -> Dict[int, List[int]]:
        """Construye p-vecindarios para cada cliente basado en distancias."""
        neighborhoods = {}
        for cliente_id in clientes:
            distances = [
                (other_id, self.sweep.instance.distancia(cliente_id, other_id)) 
                for other_id in clientes if other_id != cliente_id
            ]
            distances.sort(key=lambda x: x[1])
            neighborhoods[cliente_id] = [cid for cid, _ in distances[:p]]
        return neighborhoods
    
    def _calculate_insertion_cost(self, route_clients: List[int], insert_pos: int, 
                                  vertex: int, cisterna: Cisterna, 
                                  productos_map: Dict[int, List[str]]) -> Optional[float]:
        """Calcula el costo de insertar un vértice en una posición específica."""
        new_route = route_clients[:insert_pos] + [vertex] + route_clients[insert_pos:]
        
        factible, _, _ = self.sweep.evaluator.verificar_factibilidad_ruta(
            new_route, cisterna, productos_map
        )
        if not factible:
            return None
        
        distancia = self.sweep.evaluator.calcular_distancia_ruta(new_route)
        return cisterna.costo_fijo + cisterna.costo_km * distancia
    
    def _try_type_i_insertion(self, route_clients: List[int], v: int, v_l: int, 
                             v_j: int, v_k: int, cisterna: Cisterna,
                             productos_map: Dict[int, List[str]]) -> Optional[Tuple[List[int], float]]:
        """Intenta inserción Tipo I: v entre v_l y v_j con v_k en el camino."""
        try:
            idx_l = route_clients.index(v_l)
            idx_j = route_clients.index(v_j)
            idx_k = route_clients.index(v_k)
            
            # v_k debe estar en el camino de v_l a v_j
            if not (idx_l < idx_k < idx_j):
                return None
            
            # Construir nueva ruta: v_l -> v -> v_j, revertir caminos
            new_route = route_clients[:idx_l+1]  # hasta v_l
            new_route.append(v)  # insertar v
            new_route.extend(reversed(route_clients[idx_l+1:idx_j+1]))  # revertir v_l+1...v_j
            new_route.extend(route_clients[idx_j+1:idx_k])  # mantener hasta v_k-1
            new_route.extend(reversed(route_clients[idx_k:]))  # revertir desde v_k
            
            # Actualizar productos
            new_productos = productos_map.copy()
            cliente_v = self.sweep.instance.cliente_por_id(v)
            new_productos[v] = []
            if cliente_v.demanda_gasohol > 0:
                new_productos[v].append('G')
            if cliente_v.demanda_diesel > 0:
                new_productos[v].append('D')
            
            costo = self._calculate_insertion_cost(new_route, 0, v, cisterna, new_productos)
            if costo is not None:
                return (new_route, costo)
        except (ValueError, IndexError):
            pass
        return None
    
    def _try_type_ii_insertion(self, route_clients: List[int], v: int, v_i: int, 
                               v_j: int, v_k: int, v_l: int, cisterna: Cisterna,
                               productos_map: Dict[int, List[str]]) -> Optional[Tuple[List[int], float]]:
        """
        Intenta inserción Tipo II con path reversals más complejos.
        
        Constraints: v_k != v_j and v_k != v_j+1; v_l != v_i and v_l != v_i+1
        """
        try:
            idx_i = route_clients.index(v_i)
            idx_j = route_clients.index(v_j)
            idx_k = route_clients.index(v_k)
            idx_l = route_clients.index(v_l)
            
            # Validar constraints de posición para Type II
            if not (idx_i < idx_l - 1):  # v_l not adjacent to v_i
                return None
            if not (idx_l <= idx_j):  # v_l before or at v_j
                return None
            if not (idx_j < idx_k - 1):  # v_k not adjacent to v_j
                return None
            
            # Construir nueva ruta según Type II
            # [... v_i] + [v] + [v_j ... v_l reversed] + [v_j+1 ... v_k-1] + [v_i+1 ... v_l-1 reversed] + [v_k ...]
            new_route = route_clients[:idx_i+1]  # hasta v_i (inclusive)
            new_route.append(v)  # insertar v
            new_route.extend(reversed(route_clients[idx_l:idx_j+1]))  # revertir v_l...v_j
            new_route.extend(route_clients[idx_j+1:idx_k])  # mantener v_j+1...v_k-1
            new_route.extend(reversed(route_clients[idx_i+1:idx_l]))  # revertir v_i+1...v_l-1
            new_route.extend(route_clients[idx_k:])  # resto desde v_k
            
            # Actualizar productos
            new_productos = productos_map.copy()
            cliente_v = self.sweep.instance.cliente_por_id(v)
            new_productos[v] = []
            if cliente_v.demanda_gasohol > 0:
                new_productos[v].append('G')
            if cliente_v.demanda_diesel > 0:
                new_productos[v].append('D')
            
            # Validar factibilidad y calcular costo
            factible, _, _ = self.sweep.evaluator.verificar_factibilidad_ruta(
                new_route, cisterna, new_productos
            )
            if not factible:
                return None
            
            distancia = self.sweep.evaluator.calcular_distancia_ruta(new_route)
            costo = cisterna.costo_fijo + cisterna.costo_km * distancia
            
            return (new_route, costo)
        except (ValueError, IndexError):
            pass
        return None
    
    def _try_simple_insertion(self, route_clients: List[int], v: int, 
                             cisterna: Cisterna, productos_map: Dict[int, List[str]]) -> Optional[Tuple[List[int], int, float]]:
        """Inserción simple entre dos vértices consecutivos."""
        cliente_v = self.sweep.instance.cliente_por_id(v)
        new_productos = productos_map.copy()
        new_productos[v] = []
        if cliente_v.demanda_gasohol > 0:
            new_productos[v].append('G')
        if cliente_v.demanda_diesel > 0:
            new_productos[v].append('D')
        
        best_pos, best_cost = None, float('inf')
        for pos in range(len(route_clients) + 1):
            costo = self._calculate_insertion_cost(route_clients, pos, v, cisterna, new_productos)
            if costo is not None and costo < best_cost:
                best_pos = pos
                best_cost = costo
        
        if best_pos is not None:
            new_route = route_clients[:best_pos] + [v] + route_clients[best_pos:]
            return (new_route, best_pos, best_cost)
        return None

    def geni_insertion(self, rutas: List[Ruta], vertex_to_be_inserted: int, p: int = 5) -> List[Ruta]:
        """
        Implementing ideas of Gendreau (1992) - 'New insertion and postoptimization procedures for TSP'

        GENI (Generalized Insertion Procedure)

        The algorithm attempts fewer insertions (than predecesors), but executes eah one more carefully bu performing a limited number of local transformations of the tour, simultaneously with the insertion itself.

        The main feature of GENI is that insertion of a vertex `v` in a tour does not necessarily take place between two vertices which are consecutive when they are first considered. 
        However, after insertion, these two vertices become adjacent to `v` in the new tour. 
        Suppose that we wish to insert `v` between any vertices `v_l` and `v_j`. Let `v_k` be a vertex on the path from `v_l` to `v_j`. 
        For any vertex `v_h` on the tour, let `v_h-1` be its predecessor and `v_h+1` its successor.
        Insertion of `v` between `v_l` and `v_j` can be done in one of two ways:

        Type I insertion:

            Here `v_k` != `v_l`, and `v_k` != `v_j`.
            Inserting `v` in the tour results in the deletion of arcs (`v_l`, `v_l+1`), (`v_j`, `v_j+1`), and (`v_k`, `v_k+1`), and in their replacement by (`v_l`, `v`), (`v`, `v_j`), (`v_l+1`, `v_k`), and (`v_j+1`, `v_k+1`). This implies that the two paths (`v_l+1` ... `v_j`) and (`v_j+1` ... `v_k`) are reversed.

        Type II insertion:
            Here `v_k` != `v_j`, and `v_k` != `v_j+1`; `v_l` != `v_i` and `v_l` != `v_i+1`.
            Inserting `v` in the tour results in the deletion of (`v_i`, `v_i+1`), (`v_l-1`, `v_l`), (`v_j`, `v_j+1`), and (`v_k-1`, `v_k`). 
            These arcs are replaced by (`v_i`, `v`), (`v`, `v_j`), (`v_l`, `v_j+1`), (`v_k-1`, `v_l-1`) and (`v_i+1`, `v_k`). 
            As before, the paths (`v_i+1` ... `v_l-1`) and (`v_l` ... `v_j`) are reversed.

        The GENI algorithm considers the two possible orientations of the tour for each possible insertion.
        Since the potential number of choices for `v_i`, `v_j`, `v_k`, `v_l` is on the order of `n^4`, we limit the search as follows:
        For any vertex `v` in `V`, define its `p`-neighborhood `N_p(v)` as the set of the `p` vertices on the tour closes to `v` (with respect to the distance matrix).
        If `v` has fewer than `p` neighbors, they all belong to `N_p(v)`.
        Then, for a given parameter `p`, we first select `v_i` and `v_j` in `N_p(v)`, `v_k` in `N_p(v_i+1)`, and `v_l` in `N_p(v_j+1).
        We also consider all insertions of `v` between two consecutive vertices `v_i` and `v_i+1`, as long as `v_i` belongs to `N_p(v)`.
        In practice `p` is a relatively small number.

            GENI Algorithm:
                Implement the least cost insertion (having into account constraints) of a vertex `v` considering the two insertion types described above.
                Update the `p`-neighborhoods of all vertices affected by the insertion.
                If all vertice are now part of the tour, stop. Otherwise repeat the procedure for another vertex `v` not yet in the tour.

        The complexity of GENI is `O(n p^4 + n^2)` because `p^4` choices of `v_i`, `v_j`, `v_k`, and `v_l`
        """
        all_clients = []
        for ruta in rutas:
            all_clients.extend(ruta.clientes)
        
        # Construir p-vecindarios
        p_neighborhoods = self._build_p_neighborhoods(all_clients, p)
        
        vehiculos_usados = contar_vehiculos(rutas)
        best_insertion = None
        best_cost = float('inf')
        best_route_idx = None
        
        # Probar inserción en cada ruta
        for idx, ruta in enumerate(rutas):
            productos_map = ruta.productos_entregados.copy()
            
            # Inserción simple (más común y eficiente)
            simple = self._try_simple_insertion(ruta.clientes, vertex_to_be_inserted, 
                                               ruta.cisterna, productos_map)
            if simple and simple[2] < best_cost:
                best_insertion = ('simple', simple[0], ruta.cisterna)
                best_cost = simple[2]
                best_route_idx = idx
            
            # Intentar inserciones Tipo I y Tipo II solo con vecinos cercanos
            neighbors_v = p_neighborhoods.get(vertex_to_be_inserted, [])
            
            for v_i in neighbors_v:
                if v_i not in ruta.clientes:
                    continue
                    
                neighbors_vi = p_neighborhoods.get(v_i, [])
                
                for v_j in neighbors_v:
                    if v_j not in ruta.clientes or v_i == v_j:
                        continue
                    
                    neighbors_vj = p_neighborhoods.get(v_j, [])
                    
                    # Tipo I: v_k en vecindario de v_i
                    for v_k in neighbors_vi:
                        if v_k in ruta.clientes and v_k != v_i and v_k != v_j:
                            tipo1 = self._try_type_i_insertion(
                                ruta.clientes, vertex_to_be_inserted, 
                                v_i, v_j, v_k, ruta.cisterna, productos_map
                            )
                            if tipo1 and tipo1[1] < best_cost:
                                best_insertion = ('type1', tipo1[0], ruta.cisterna)
                                best_cost = tipo1[1]
                                best_route_idx = idx
                    
                    # Tipo II: v_k en vecindario de v_i, v_l en vecindario de v_j
                    for v_k in neighbors_vi:
                        if v_k not in ruta.clientes:
                            continue
                        for v_l in neighbors_vj:
                            if v_l not in ruta.clientes:
                                continue
                            if v_k == v_j or v_l == v_i:
                                continue
                            
                            tipo2 = self._try_type_ii_insertion(
                                ruta.clientes, vertex_to_be_inserted,
                                v_i, v_j, v_k, v_l, ruta.cisterna, productos_map
                            )
                            if tipo2 and tipo2[1] < best_cost:
                                best_insertion = ('type2', tipo2[0], ruta.cisterna)
                                best_cost = tipo2[1]
                                best_route_idx = idx
        
        # Aplicar mejor inserción encontrada
        if best_insertion:
            tipo, new_route_clients, cisterna = best_insertion
            
            # Reconstruir productos_map
            productos_map = {}
            for cid in new_route_clients:
                cliente = self.sweep.instance.cliente_por_id(cid)
                productos_map[cid] = []
                if cliente.demanda_gasohol > 0:
                    productos_map[cid].append('G')
                if cliente.demanda_diesel > 0:
                    productos_map[cid].append('D')
            
            nueva_ruta = self.sweep.evaluator.crear_ruta_objeto(
                new_route_clients, cisterna, productos_map
            )
            
            rutas_modificadas = copy.deepcopy(rutas)
            rutas_modificadas[best_route_idx] = nueva_ruta
            return rutas_modificadas
        
        # Si no se pudo insertar, crear nueva ruta
        cliente = self.sweep.instance.cliente_por_id(vertex_to_be_inserted)
        productos_map = {vertex_to_be_inserted: []}
        if cliente.demanda_gasohol > 0:
            productos_map[vertex_to_be_inserted].append('G')
        if cliente.demanda_diesel > 0:
            productos_map[vertex_to_be_inserted].append('D')
        
        cisterna = self.sweep.evaluator.seleccionar_mejor_cisterna(
            [vertex_to_be_inserted], productos_map, vehiculos_usados
        )
        
        if cisterna:
            nueva_ruta = self.sweep.evaluator.crear_ruta_objeto(
                [vertex_to_be_inserted], cisterna, productos_map
            )
            return rutas + [nueva_ruta]
        
        return rutas
    
    def perturbation(self, rutas: List[Ruta]) -> List[Ruta]:
        """
        Perturba una solución removiendo un cliente aleatorio y sus pi vecinos más cercanos, luego los reinserta usando GENI.

        To perturb a solution a random client is chosen and removed from its route, together its the `pi` nearest neighbors clientes
        `pi` is randomly chosen in [0, sqrt(n)], where n is the number of clients in the solution.

        The removed clients are then reinserted in the solution using a greedy insertion heuristic.
        Each clients is inserted into the route which minimizes the increase in the total routing cost (having into account the schedule, vehicle, and capacity constraints). [Parallelize operations for speedup]

        We use the Generalized Insertion Procedure (GENI) to insert visits into routes or remove visits from routes. 
        Together with the insertion or removal of a vertex, GENI applies a subset of 3-opt and 4-opt moves to the route.
        """
        # Recolectar todos los clientes
        all_clients = []
        for ruta in rutas:
            all_clients.extend(ruta.clientes)
        
        if len(all_clients) < 2:
            return rutas
        
        # Seleccionar cliente aleatorio
        selected_client = random.choice(all_clients)
        
        # Calcular pi
        n = len(all_clients)
        pi = random.randint(0, int(math.sqrt(n)))
        
        # Encontrar pi vecinos más cercanos
        distances = [(cid, self.sweep.instance.distancia(selected_client, cid)) 
                    for cid in all_clients if cid != selected_client]
        distances.sort(key=lambda x: x[1])
        neighbors_to_remove = [cid for cid, _ in distances[:pi]]
        
        clients_to_remove = [selected_client] + neighbors_to_remove
        
        # Remover clientes de las rutas
        rutas_sin_clientes = []
        for ruta in rutas:
            remaining = [c for c in ruta.clientes if c not in clients_to_remove]
            if remaining:
                productos_map = {c: ruta.productos_entregados[c] for c in remaining}
                vehiculos_usados = contar_vehiculos(rutas_sin_clientes)
                cisterna = self.sweep.evaluator.seleccionar_mejor_cisterna(
                    remaining, productos_map, vehiculos_usados
                )
                if cisterna:
                    nueva_ruta = self.sweep.evaluator.crear_ruta_objeto(
                        remaining, cisterna, productos_map
                    )
                    rutas_sin_clientes.append(nueva_ruta)
        
        # Reinsertar clientes usando GENI
        rutas_perturbadas = rutas_sin_clientes
        random.shuffle(clients_to_remove)  # Aleatorizar orden de inserción
        
        for cliente_id in clients_to_remove:
            rutas_perturbadas = self.geni_insertion(rutas_perturbadas, cliente_id, p=5)
        
        return rutas_perturbadas
    
    def _best_shift_move(self, rutas: List[Ruta], zeta: float):
        return rutas

    def tabu_search(self, rutas: List[Ruta], iteration: int, alpha: float = 1.0, beta: float = 1.0, rho: float = 1.0) -> List[Ruta]:
        """
        The performance of the tabu search depends on the neighborhood structure, the handling of infeasible solutions, and the design of the short-term memory.

        The proposed implementation startas from some initial solution and repeatedly moves to the best non-tabu neighbor. 
        The current solution may exceed, the capacity, lenght, or time window constraints.

        
        For a set of routes $s = {R_1, ldots, R_tau}$ we define its time (or distance) excess as:

        $$D^+(s) = sum_{R in s} max{d(R) - D, 0}$$

        and its capacity excess as:

        $$C^+ = sum_{R in s} max{max_{i in [m]} Delta c_i, 0}$$

        for $Delta c = (Delta c_1, ldots, Delta c_m) = c(R) - C$.

        The objective value of solution $s$ is then:

        $$F(s) = d(s) + alpha C^+(s) + beta D^+(s)$$

        where $alpha$ and $beta$ are penalties for each unit of capacity and time excess respectively. Initially, $alpha = beta = 0$.
        We will set $rho$ as the penalty for each unit of time window violation.

        The penalties are then updated to allow strategic oscillation between feasible and infeasible solutions.
        Every time the curren solution exceeds the capacity, lenght, or time window constraints, the corresponding penalty is increased by a factor of $(1 + delta)$, with $delta > 0$; otherwise, it is decreased by a factor of $(1 + delta)$.

        Neighbourhood and Tabu List:
        Diversification:
        """
        gamma, zeta = random.random(), random.random()

        rutas = self._best_shift_move(rutas, zeta)

        return rutas


# -----------------------------------------------------------------------------
# Solution Visualization
# -----------------------------------------------------------------------------

class SolutionVisualizer:
    """Visualiza y reporta soluciones."""
    
    def __init__(self, instance: ProblemInstance):
        self.instance = instance
    
    def imprimir_solucion(self, rutas: List[Ruta], verbosity: int = 1):
        """Imprime la solución de forma legible."""
        print("\n" + "=" * 80)
        print("SOLUCIÓN")
        print("=" * 80)
        
        costo_total = sum(r.costo_total for r in rutas)
        distancia_total = sum(r.distancia_total for r in rutas)
        
        vehiculos_tipo_1 = sum(1 for r in rutas if r.cisterna.tipo == 1)
        vehiculos_tipo_2 = sum(1 for r in rutas if r.cisterna.tipo == 2)
        
        if verbosity >= 1:
            print(f"\n>>> RESUMEN GENERAL:")
            print(f"  * Número de rutas: {len(rutas)}")
            print(f"  * Costo total: ${costo_total:,.2f}")
            print(f"  * Distancia total: {distancia_total:.2f} km")
            print(f"  * Cisternas Tipo 1: {vehiculos_tipo_1}/{self.instance.num_vehiculos_por_tipo}", end="")
            if vehiculos_tipo_1 > self.instance.num_vehiculos_por_tipo:
                print(" !!! EXCEDE LÍMITE", end="")
            print()
            print(f"  * Cisternas Tipo 2: {vehiculos_tipo_2}/{self.instance.num_vehiculos_por_tipo}", end="")
            if vehiculos_tipo_2 > self.instance.num_vehiculos_por_tipo:
                print(" !!! EXCEDE LÍMITE", end="")
            print()

        if verbosity >= 2:    
            print(f"\n>>> DETALLE DE RUTAS:")
            for idx, ruta in enumerate(rutas, 1):
                print(f"\n  Ruta #{idx}:")
                print(f"    Cisterna: Tipo {ruta.cisterna.tipo}")
                print(f"    Secuencia: Depot → {' → '.join(map(str, ruta.clientes))} → Depot")
                print(f"    Carga: Gasohol={ruta.carga_gasohol:.0f}gal ({ruta.carga_gasohol/ruta.cisterna.cap_gasohol*100:.1f}%), "
                    f"Diésel={ruta.carga_diesel:.0f}gal ({ruta.carga_diesel/ruta.cisterna.cap_diesel*100:.1f}%)")
                print(f"    Distancia: {ruta.distancia_total:.2f} km")
                print(f"    Tiempo: {ruta.tiempo_total:.1f} min")
                print(f"    Costo: ${ruta.costo_total:,.2f}")
                print(f"    Tiempos de llegada (min desde 04:00):")
                for cliente_id, tiempo_llegada in zip(ruta.clientes, ruta.tiempos_llegada):
                    hora = 4 + tiempo_llegada // 60
                    minuto = tiempo_llegada % 60
                    cliente = self.instance.cliente_por_id(cliente_id)
                    ventana_str = f"[{4 + cliente.ventana_inicio//60:02.0f}:{cliente.ventana_inicio%60:02.0f} - {4 + cliente.ventana_fin//60:02.0f}:{cliente.ventana_fin%60:02.0f}]"
                    print(f"      - Cliente {cliente_id}: {tiempo_llegada:.1f} min ({hora:02.0f}:{minuto:02.0f}) | Ventana: {ventana_str}")
                print(f"    Entregas por cliente:")
                for cliente_id, productos in ruta.productos_entregados.items():
                    prods_str = ", ".join(productos)
                    print(f"      - Cliente {cliente_id}: {prods_str}")
            
        print("\n" + "="*80)

    def visualizar_rutas(self, rutas: List[Ruta], caption: str = ""):
        """Visualiza las rutas en un mapa 2D."""
        colores_base = [
            '#FF6B6B', '#FFA07A', '#98D8C8', '#F7DC6F', 
            '#45B7D1', '#BB8FCE', '#F8B739', '#85C1E2',
            '#E63946', '#06FFA5', '#A8DADC', '#FF006E', 
            '#FB5607', '#3A86FF', '#FFBE0B', '#06D6A0', 
            '#EF476F', '#8338EC', '#4ECDC4', '#52B788',
            '#118AB2', '#FFD166', '#D62828', '#F77F00',
            '#2EC4B6', '#E71D36', '#011627', '#C9ADA7'
        ]
        
        costo_total = sum(r.costo_total for r in rutas)
        fig, ax = plt.subplots(figsize=(12, 9))
        
        # Dibujar depot
        ax.scatter(self.instance.depot.x, self.instance.depot.y, s=300, c='red', marker='s', 
                  label='Depósito', zorder=5, edgecolors='black', linewidth=2)
        ax.text(self.instance.depot.x + 0.75, self.instance.depot.y + 0.75, 'DEPOT', 
               fontsize=10, fontweight='bold')
        
        # Dibujar clientes
        clientes_x = [c.x for c in self.instance.clientes]
        clientes_y = [c.y for c in self.instance.clientes]
        ax.scatter(clientes_x, clientes_y, s=150, c='lightblue', 
                  label='Clientes', zorder=4, edgecolors='black', linewidth=1)
        
        for cliente in self.instance.clientes:
            ax.text(cliente.x + 0.75, cliente.y + 0.75, str(cliente.id), fontsize=9)
        
        # Dibujar rutas
        legend_elements = []
        
        for idx, ruta in enumerate(rutas):
            color = colores_base[idx % len(colores_base)]
            secuencia = [0] + ruta.clientes + [0]
            
            x_coords = []
            y_coords = []
            for nodo in secuencia:
                cliente = self.instance.cliente_por_id(nodo)
                x_coords.append(cliente.x)
                y_coords.append(cliente.y)
            
            ax.plot(x_coords, y_coords, color=color, linewidth=2, alpha=0.7, zorder=3)
            
            # Flechas de dirección
            for i in range(len(x_coords) - 1):
                dx = x_coords[i+1] - x_coords[i]
                dy = y_coords[i+1] - y_coords[i]
                mid_x = x_coords[i] + dx * 0.5
                mid_y = y_coords[i] + dy * 0.5
                ax.annotate('', xy=(mid_x + dx*0.1, mid_y + dy*0.1), 
                          xytext=(mid_x - dx*0.1, mid_y - dy*0.1),
                          arrowprops=dict(arrowstyle='->', color=color, lw=1.5, alpha=0.8))
            
            tipo_str = f"Tipo {ruta.cisterna.tipo}"
            costo_str = f"${ruta.costo_total:.0f}"
            dist_str = f"{ruta.distancia_total:.1f}km"
            legend_elements.append(
                mpatches.Patch(color=color, 
                              label=f"Ruta {idx+1}: {tipo_str} | {dist_str} | {costo_str}")
            )

        if caption:
            caption = " - " + caption      
        ax.set_xlabel('Coordenada X (km)', fontsize=11)
        ax.set_ylabel('Coordenada Y (km)', fontsize=11)
        ax.set_title('Visualización de Rutas' + caption + f"| CT: ${costo_total:,.2f}", 
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal', adjustable='box')
        ax.legend(handles=legend_elements, loc='upper left', 
                 bbox_to_anchor=(1.02, 1), fontsize=9, framealpha=0.9)
        
        plt.tight_layout()
        plt.show()

