#include "solvers.h"
#include <iostream>
#include <numeric>
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <random>
#include <set>

SweepAlgorithm::SweepAlgorithm(ProblemInstance& instance)
    : instance(instance), evaluator(instance) {}

std::vector<Ruta> SweepAlgorithm::forward_sweep() {
    std::vector<Ruta> rutas;
    std::vector<int> clientes_no_asignados_vec;
    for(const auto& c : instance.clientes) {
        clientes_no_asignados_vec.push_back(c.id);
    }

    std::map<int, std::map<std::string, bool>> demandas_pendientes;
    for (const auto& c : instance.clientes) {
        demandas_pendientes[c.id] = {{"G", c.demanda_gasohol > 0}, {"D", c.demanda_diesel > 0}};
    }

    std::map<int, int> vehiculos_usados = {{1, 0}, {2, 0}};

    while (!clientes_no_asignados_vec.empty()) {
        std::vector<int> cluster_actual;
        std::map<int, std::vector<std::string>> productos_cluster;

        // Create a copy for iteration that is sorted by angle
        auto clientes_ordenados = clientes_no_asignados_vec;
        std::sort(clientes_ordenados.begin(), clientes_ordenados.end(), [&](int a, int b){
            return instance.cliente_por_id(a).angulo < instance.cliente_por_id(b).angulo;
        });

        for (int cliente_id : clientes_ordenados) {
            // Skip if no pending demands
            if (!demandas_pendientes[cliente_id]["G"] && !demandas_pendientes[cliente_id]["D"]) {
                continue;
            }

            std::vector<int> cluster_test = cluster_actual;
            cluster_test.push_back(cliente_id);

            std::map<int, std::vector<std::string>> productos_test = productos_cluster;
            
            std::vector<std::string> prods;
            if (demandas_pendientes[cliente_id]["G"]) prods.push_back("G");
            if (demandas_pendientes[cliente_id]["D"]) prods.push_back("D");
            productos_test[cliente_id] = prods;

            auto mejor_cisterna = evaluator.seleccionar_mejor_cisterna(cluster_test, productos_test, vehiculos_usados);

            if (mejor_cisterna) {
                cluster_actual = cluster_test;
                productos_cluster = productos_test;
            } else {
                // Try with just one product if there were two
                if (prods.size() == 2) {
                    productos_test[cliente_id] = {"G"};
                    auto cisterna_g = evaluator.seleccionar_mejor_cisterna(cluster_test, productos_test, vehiculos_usados);
                    if (cisterna_g) {
                        cluster_actual = cluster_test;
                        productos_cluster = productos_test;
                        continue; // Continue to next client
                    }

                    productos_test[cliente_id] = {"D"};
                    auto cisterna_d = evaluator.seleccionar_mejor_cisterna(cluster_test, productos_test, vehiculos_usados);
                    if (cisterna_d) {
                        cluster_actual = cluster_test;
                        productos_cluster = productos_test;
                        continue; // Continue to next client
                    }
                }
                // If it doesn't fit even with one product, or it only had one product, we can't add more clients to this route.
                break; 
            }
        }

        if (cluster_actual.empty()) {
             if (!clientes_no_asignados_vec.empty()) {
                 // This can happen if remaining clients can't form a feasible route.
                 // We should check if any client has been left unassigned.
                 bool all_assigned = true;
                 for(int cid : clientes_no_asignados_vec) {
                     if(demandas_pendientes[cid]["G"] || demandas_pendientes[cid]["D"]) {
                         all_assigned = false;
                         break;
                     }
                 }
                 if(!all_assigned) {
                    throw std::runtime_error("Error: No se pudo asignar clientes restantes.");
                 }
             }
             break; // Exit if no cluster could be formed
        }
        
        auto cisterna_final = evaluator.seleccionar_mejor_cisterna(cluster_actual, productos_cluster, vehiculos_usados);
        
        if(cisterna_final) {
            auto ruta_obj_opt = evaluator.crear_ruta_objeto(cluster_actual, *cisterna_final, productos_cluster);
            if (ruta_obj_opt) {
                rutas.push_back(*ruta_obj_opt);
                vehiculos_usados[cisterna_final->tipo]++;

                std::vector<int> clientes_servidos_en_cluster;
                for (const auto& [cliente_id, productos] : productos_cluster) {
                    for (const auto& prod : productos) {
                        if (prod == "G") demandas_pendientes[cliente_id]["G"] = false;
                        if (prod == "D") demandas_pendientes[cliente_id]["D"] = false;
                    }
                    if (!demandas_pendientes[cliente_id]["G"] && !demandas_pendientes[cliente_id]["D"]) {
                        clientes_servidos_en_cluster.push_back(cliente_id);
                    }
                }
                
                // Remove serviced clients using erase-remove idiom
                clientes_no_asignados_vec.erase(
                    std::remove_if(clientes_no_asignados_vec.begin(), clientes_no_asignados_vec.end(),
                        [&](int id) {
                            return std::find(clientes_servidos_en_cluster.begin(), clientes_servidos_en_cluster.end(), id) != clientes_servidos_en_cluster.end();
                        }),
                    clientes_no_asignados_vec.end()
                );

            } else {
                 throw std::runtime_error("Error: No se pudo crear la ruta a pesar de encontrar cisterna.");
            }
        } else {
            if (!cluster_actual.empty()) {
                 throw std::runtime_error("Error: No se pudo asignar un vehiculo a un cluster no vacio.");
            }
        }
    }
    return rutas;
}

// -----------------------------------------------------------------------------
// Improving Sweep Implementation
// -----------------------------------------------------------------------------

std::map<int, int> contar_vehiculos_cpp(const std::vector<Ruta>& rutas) {
    std::map<int, int> contador = {{1, 0}, {2, 0}};
    for (const auto& ruta : rutas) {
        contador[ruta.cisterna.tipo]++;
    }
    return contador;
}

std::map<int, std::vector<std::string>> build_product_map_cpp(
    const std::vector<int>& clientes_list,
    const Ruta& route_k,
    const Ruta& route_k_plus_1)
{
    std::map<int, std::vector<std::string>> m;
    for (int cid : clientes_list) {
        auto it_k = route_k.productos_entregados.find(cid);
        if (it_k != route_k.productos_entregados.end()) {
            m[cid] = it_k->second;
        } else {
            auto it_k1 = route_k_plus_1.productos_entregados.find(cid);
            if (it_k1 != route_k_plus_1.productos_entregados.end()) {
                m[cid] = it_k1->second;
            }
        }
    }
    return m;
}

double SweepAlgorithm::calcular_score_eliminacion(int cliente_id, double avg_radius) {
    const auto& cliente = instance.cliente_por_id(cliente_id);
    return cliente.radio + cliente.angulo * avg_radius;
}

std::optional<int> SweepAlgorithm::seleccionar_cliente_a_eliminar(const Ruta& ruta, double avg_radius) {
    if (ruta.clientes.empty()) {
        return std::nullopt;
    }

    return *std::min_element(ruta.clientes.begin(), ruta.clientes.end(),
        [&](int a, int b) {
            return calcular_score_eliminacion(a, avg_radius) < calcular_score_eliminacion(b, avg_radius);
        });
}

std::vector<int> SweepAlgorithm::obtener_candidatos_ordenados(const Ruta& ruta_k, const Ruta& ruta_k_plus_1) {
    if (ruta_k.clientes.empty() || ruta_k_plus_1.clientes.empty()) {
        return {};
    }

    int ultimo_cliente_k = ruta_k.clientes.back();
    std::vector<int> candidatos = ruta_k_plus_1.clientes;

    std::sort(candidatos.begin(), candidatos.end(),
        [&](int a, int b) {
            return instance.distancia(ultimo_cliente_k, a) < instance.distancia(ultimo_cliente_k, b);
        });

    return candidatos;
}

std::optional<SweepAlgorithm::InsercionInfo> SweepAlgorithm::encontrar_mejor_posicion_insercion(
    const std::vector<int>& ruta_actual, int cliente_id, const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
    std::map<int, int> vehiculos_disponibles)
{
    std::optional<InsercionInfo> mejor_insercion = std::nullopt;
    double mejor_costo = std::numeric_limits<double>::infinity();

    for (size_t pos = 0; pos <= ruta_actual.size(); ++pos) {
        std::vector<int> tentativa = ruta_actual;
        tentativa.insert(tentativa.begin() + pos, cliente_id);

        auto productos = build_product_map_cpp(tentativa, ruta_k, ruta_k_plus_1);
        auto cisterna_opt = evaluator.seleccionar_mejor_cisterna(tentativa, productos, vehiculos_disponibles);

        if (!cisterna_opt) {
            continue;
        }
        auto& cisterna = *cisterna_opt;

        auto [factible, tiempo_total, info] = evaluator.verificar_factibilidad_ruta(tentativa, cisterna, productos);

        if (!factible) {
            continue;
        }

        double distancia = evaluator.calcular_distancia_ruta(tentativa);
        double costo = cisterna.costo_fijo + cisterna.costo_km * distancia;

        if (costo < mejor_costo) {
            mejor_costo = costo;
            mejor_insercion = InsercionInfo{static_cast<int>(pos), cisterna, info, costo};
        }
    }
    return mejor_insercion;
}


std::optional<SweepAlgorithm::GreedyInsercionResult> SweepAlgorithm::intentar_insercion_greedy(
    const std::vector<int>& base_clientes, const std::vector<int>& clientes_a_insertar,
    const Ruta& ruta_k, const Ruta& ruta_k_plus_1, std::map<int, int> vehiculos_disponibles)
{
    std::vector<int> ruta_actual = base_clientes;
    std::optional<Cisterna> cisterna_actual = std::nullopt;
    std::optional<std::map<std::string, double>> info_actual = std::nullopt;
    double costo_actual = 0.0;

    for (int cliente_id : clientes_a_insertar) {
        auto mejor_insercion = encontrar_mejor_posicion_insercion(ruta_actual, cliente_id, ruta_k, ruta_k_plus_1, vehiculos_disponibles);

        if (!mejor_insercion) {
            return std::nullopt;
        }

        ruta_actual.insert(ruta_actual.begin() + mejor_insercion->pos, cliente_id);
        cisterna_actual = mejor_insercion->cisterna;
        info_actual = mejor_insercion->info;
        costo_actual = mejor_insercion->costo;
    }

    if (cisterna_actual && info_actual) {
        return GreedyInsercionResult{ruta_actual, *cisterna_actual, *info_actual, costo_actual};
    }
    return std::nullopt;
}

std::optional<SweepAlgorithm::ReconstruccionResult> SweepAlgorithm::reconstruir_ruta_k_plus_1(
    const std::vector<int>& base_clientes, int cliente_a_insertar, const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
    std::map<int, int> vehiculos_disponibles)
{
    std::optional<ReconstruccionResult> mejor_resultado = std::nullopt;
    double mejor_costo = std::numeric_limits<double>::infinity();

    for (size_t pos = 0; pos <= base_clientes.size(); ++pos) {
        std::vector<int> tentativa = base_clientes;
        tentativa.insert(tentativa.begin() + pos, cliente_a_insertar);

        auto productos = build_product_map_cpp(tentativa, ruta_k, ruta_k_plus_1);
        auto cisterna_opt = evaluator.seleccionar_mejor_cisterna(tentativa, productos, vehiculos_disponibles);

        if (!cisterna_opt) {
            continue;
        }
        auto& cisterna = *cisterna_opt;

        auto [factible, tiempo_total, info] = evaluator.verificar_factibilidad_ruta(tentativa, cisterna, productos);

        if (!factible) {
            continue;
        }

        double distancia = evaluator.calcular_distancia_ruta(tentativa);
        double costo = cisterna.costo_fijo + cisterna.costo_km * distancia;

        if (costo < mejor_costo) {
            mejor_costo = costo;
            mejor_resultado = ReconstruccionResult{tentativa, cisterna, info, costo};
        }
    }
    return mejor_resultado;
}

Ruta SweepAlgorithm::crear_ruta_desde_swap(const std::vector<int>& clientes, const Cisterna& cisterna, const Ruta& ruta_original_k, const Ruta& ruta_original_k_plus_1) {
    auto productos = build_product_map_cpp(clientes, ruta_original_k, ruta_original_k_plus_1);
    return *evaluator.crear_ruta_objeto(clientes, cisterna, productos);
}


std::optional<SweepAlgorithm::SwapResult> SweepAlgorithm::buscar_mejor_intercambio(
    const Ruta& ruta_k, const Ruta& ruta_k_plus_1, int cliente_eliminar, const std::vector<int>& candidatos,
    std::map<int, int> vehiculos_disponibles)
{
    std::vector<int> base_k;
    std::copy_if(ruta_k.clientes.begin(), ruta_k.clientes.end(), std::back_inserter(base_k),
                 [cliente_eliminar](int c) { return c != cliente_eliminar; });

    double costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total;

    auto veh_temp = vehiculos_disponibles;
    veh_temp[ruta_k.cisterna.tipo]--;
    veh_temp[ruta_k_plus_1.cisterna.tipo]--;

    for (size_t m = 1; m <= candidatos.size(); ++m) {
        std::vector<int> prefijo(candidatos.begin(), candidatos.begin() + m);

        auto resultado_k = intentar_insercion_greedy(base_k, prefijo, ruta_k, ruta_k_plus_1, veh_temp);
        if (!resultado_k) {
            continue;
        }

        auto veh_temp2 = veh_temp;
        veh_temp2[resultado_k->cisterna.tipo]++;

        std::vector<int> base_k1;
        std::copy_if(ruta_k_plus_1.clientes.begin(), ruta_k_plus_1.clientes.end(), std::back_inserter(base_k1),
                     [&](int c) { return std::find(prefijo.begin(), prefijo.end(), c) == prefijo.end(); });

        auto resultado_k1 = reconstruir_ruta_k_plus_1(base_k1, cliente_eliminar, ruta_k, ruta_k_plus_1, veh_temp2);
        if (!resultado_k1) {
            continue;
        }

        double nuevo_costo = resultado_k->costo + resultado_k1->costo;

        if (nuevo_costo < costo_actual) {
            return SwapResult{
                resultado_k->ruta, resultado_k->cisterna, resultado_k->info,
                resultado_k1->ruta, resultado_k1->cisterna, resultado_k1->info,
                nuevo_costo
            };
        }
    }

    return std::nullopt;
}


std::pair<std::vector<Ruta>, bool> SweepAlgorithm::improving_sweep(const std::vector<Ruta>& rutas_candidatas, bool clockwise) {
    double avg_radius = 0;
    for (const auto& c : instance.clientes) {
        avg_radius += c.radio;
    }
    avg_radius /= instance.clientes.size();

    auto rutas_mejoradas = rutas_candidatas;
    if (clockwise) {
        std::reverse(rutas_mejoradas.begin(), rutas_mejoradas.end());
    }

    auto vehiculos_usados = contar_vehiculos_cpp(rutas_mejoradas);
    int iteracion = 0;
    bool hubo_alguna_mejora = false;
    bool seguir_mejorando = true;

    while (seguir_mejorando) {
        seguir_mejorando = false;
        iteracion++;

        for (size_t k = 0; k < rutas_mejoradas.size() - 1; ++k) {
            bool mejora_local = true;
            while (mejora_local) {
                mejora_local = false;

                Ruta& ruta_k = rutas_mejoradas[k];
                Ruta& ruta_k_plus_1 = rutas_mejoradas[k + 1];
                double costo_actual = ruta_k.costo_total + ruta_k_plus_1.costo_total;

                auto cliente_eliminar_opt = seleccionar_cliente_a_eliminar(ruta_k, avg_radius);
                if (!cliente_eliminar_opt) break;
                int cliente_eliminar = *cliente_eliminar_opt;

                auto candidatos = obtener_candidatos_ordenados(ruta_k, ruta_k_plus_1);
                if (candidatos.empty()) break;

                auto mejor_swap = buscar_mejor_intercambio(ruta_k, ruta_k_plus_1, cliente_eliminar, candidatos, vehiculos_usados);

                if (!mejor_swap) break;

                auto& swap_res = *mejor_swap;
                auto nueva_ruta_k = crear_ruta_desde_swap(swap_res.nueva_k, swap_res.cisterna_k, ruta_k, ruta_k_plus_1);
                auto nueva_ruta_k_plus_1 = crear_ruta_desde_swap(swap_res.nueva_k1, swap_res.cisterna_k1, ruta_k, ruta_k_plus_1);

                vehiculos_usados[ruta_k.cisterna.tipo]--;
                vehiculos_usados[ruta_k_plus_1.cisterna.tipo]--;
                vehiculos_usados[swap_res.cisterna_k.tipo]++;
                vehiculos_usados[swap_res.cisterna_k1.tipo]++;

                rutas_mejoradas[k] = nueva_ruta_k;
                rutas_mejoradas[k + 1] = nueva_ruta_k_plus_1;

                mejora_local = true;
                seguir_mejorando = true;
                hubo_alguna_mejora = true;

                std::string direccion_str = clockwise ? "horario" : "antihorario";
                double ahorro = costo_actual - swap_res.nuevo_costo;
                std::cout << "  [Iteracion " << iteracion << " - " << direccion_str << "] Mejora rutas " << k << "-" << k + 1
                          << ": $" << std::fixed << std::setprecision(2) << costo_actual
                          << " -> $" << swap_res.nuevo_costo << " (ahorro: $" << ahorro << ")" << std::endl;
            }
        }
    }

    std::string direccion_str = clockwise ? "horario" : "antihorario";
    std::cout << "\n>>> Proceso de mejora (" << direccion_str << ") completado en " << iteracion << " iteraciones." << std::endl;
    std::cout << ">>> Vehiculos utilizados:" << std::endl;
    std::cout << "  * Tipo 1: " << vehiculos_usados[1] << "/" << instance.num_vehiculos_por_tipo << std::endl;
    std::cout << "  * Tipo 2: " << vehiculos_usados[2] << "/" << instance.num_vehiculos_por_tipo << std::endl;

    if (clockwise) {
        std::reverse(rutas_mejoradas.begin(), rutas_mejoradas.end());
    }
    if (hubo_alguna_mejora) {
        best_solution = rutas_mejoradas;
    }

    return {rutas_mejoradas, hubo_alguna_mejora};
}

std::vector<Ruta> SweepAlgorithm::iterative_improving_sweep(const std::vector<Ruta>& rutas_candidatas) {
    auto rutas_actuales = rutas_candidatas;
    bool mejora_global = true;
    int iteracion_global = 0;

    std::cout << "\n" << std::string(80, '=') << std::endl;
    std::cout << "INICIANDO MEJORA ITERATIVA (CLOCKWISE Y COUNTERCLOCKWISE)" << std::endl;
    std::cout << std::string(80, '=') << std::endl;

    while (mejora_global) {
        iteracion_global++;
        mejora_global = false;

        std::cout << "\n>>> Iteracion global " << iteracion_global << std::endl;

        // Sentido antihorario
        std::cout << "\n  >>> Intentando mejora en sentido ANTIHORARIO..." << std::endl;
        auto [rutas_ccw, mejora_ccw] = improving_sweep(rutas_actuales, false);

        if (mejora_ccw) {
            rutas_actuales = rutas_ccw;
            mejora_global = true;
            double costo_actual = 0;
            for(const auto& r : rutas_actuales) costo_actual += r.costo_total;
            std::cout << "  >>> Mejora encontrada. Costo actual: $" << std::fixed << std::setprecision(2) << costo_actual << std::endl;
        } else {
            std::cout << "  >>> Sin mejoras" << std::endl;
        }

        // Sentido horario
        std::cout << "\n  >>> Intentando mejora en sentido HORARIO..." << std::endl;
        auto [rutas_cw, mejora_cw] = improving_sweep(rutas_actuales, true);

        if (mejora_cw) {
            rutas_actuales = rutas_cw;
            mejora_global = true;
            double costo_actual = 0;
            for(const auto& r : rutas_actuales) costo_actual += r.costo_total;
            std::cout << "  >>> Mejora encontrada. Costo actual: $" << std::fixed << std::setprecision(2) << costo_actual << std::endl;
        } else {
            std::cout << "  >>> Sin mejoras" << std::endl;
        }

        if (!mejora_ccw && !mejora_cw) {
            std::cout << "\n>>> No se encontraron mas mejoras en ninguna direccion." << std::endl;
            mejora_global = false;
        }
    }

    std::cout << "\n" << std::string(80, '=') << std::endl;
    std::cout << "MEJORA ITERATIVA COMPLETADA EN " << iteracion_global << " ITERACIONES GLOBALES" << std::endl;
    std::cout << std::string(80, '=') << std::endl;

    best_solution = rutas_actuales;
    return rutas_actuales;
}

// -----------------------------------------------------------------------------
// Tabu Search Solver Implementation
// -----------------------------------------------------------------------------

SolverTabuSearchMCVRPTW::SolverTabuSearchMCVRPTW(SweepAlgorithm& sweep_solver)
    : sweep(sweep_solver), 
      evaluator(sweep_solver.get_evaluator()),
      instance(sweep_solver.get_instance())
{
    best_solution = sweep.get_best_solution();
    incumbent_cost = 0;
    for(const auto& r : best_solution) {
        incumbent_cost += r.costo_total;
    }
    r_prime = best_solution.size();
}

std::map<int, std::vector<int>> SolverTabuSearchMCVRPTW::_build_p_neighborhoods(const std::vector<int>& clientes, int p) {
    std::map<int, std::vector<int>> neighborhoods;
    for (int cliente_id : clientes) {
        std::vector<std::pair<int, double>> distances;
        for (int other_id : clientes) {
            if (cliente_id != other_id) {
                distances.push_back({other_id, instance.distancia(cliente_id, other_id)});
            }
        }
        std::sort(distances.begin(), distances.end(), [](const auto& a, const auto& b) {
            return a.second < b.second;
        });
        
        std::vector<int> neighbors;
        for (int i = 0; i < std::min((int)distances.size(), p); ++i) {
            neighbors.push_back(distances[i].first);
        }
        neighborhoods[cliente_id] = neighbors;
    }
    return neighborhoods;
}

std::optional<double> SolverTabuSearchMCVRPTW::_calculate_insertion_cost(
    const std::vector<int>& route_clients, 
    int insert_pos, 
    int vertex, 
    const Cisterna& cisterna, 
    const std::map<int, std::vector<std::string>>& productos_map) 
{
    std::vector<int> new_route = route_clients;
    new_route.insert(new_route.begin() + insert_pos, vertex);

    auto [factible, tiempo_total, info] = evaluator.verificar_factibilidad_ruta(new_route, cisterna, productos_map);
    if (!factible) {
        return std::nullopt;
    }

    double distancia = evaluator.calcular_distancia_ruta(new_route);
    return cisterna.costo_fijo + cisterna.costo_km * distancia;
}

std::optional<std::pair<std::vector<int>, double>> SolverTabuSearchMCVRPTW::_try_simple_insertion(
    const std::vector<int>& route_clients, 
    int v, 
    const Cisterna& cisterna, 
    std::map<int, std::vector<std::string>> productos_map)
{
    const auto& cliente_v = instance.cliente_por_id(v);
    std::vector<std::string> prods;
    if (cliente_v.demanda_gasohol > 0) prods.push_back("G");
    if (cliente_v.demanda_diesel > 0) prods.push_back("D");
    productos_map[v] = prods;

    std::optional<int> best_pos;
    double best_cost = std::numeric_limits<double>::infinity();

    for (size_t pos = 0; pos <= route_clients.size(); ++pos) {
        auto cost_opt = _calculate_insertion_cost(route_clients, pos, v, cisterna, productos_map);
        if (cost_opt && *cost_opt < best_cost) {
            best_pos = pos;
            best_cost = *cost_opt;
        }
    }

    if (best_pos) {
        std::vector<int> new_route = route_clients;
        new_route.insert(new_route.begin() + *best_pos, v);
        return std::make_pair(new_route, best_cost);
    }
    return std::nullopt;
}

std::vector<Ruta> SolverTabuSearchMCVRPTW::geni_insertion(std::vector<Ruta> rutas, int vertex_to_be_inserted, int p) {
    std::optional<std::pair<std::vector<int>, const Cisterna*>> best_insertion;
    double best_cost = std::numeric_limits<double>::infinity();
    int best_route_idx = -1;

    for (size_t i = 0; i < rutas.size(); ++i) {
        auto& ruta = rutas[i];
        auto simple_insertion_opt = _try_simple_insertion(ruta.clientes, vertex_to_be_inserted, ruta.cisterna, ruta.productos_entregados);
        
        if (simple_insertion_opt && simple_insertion_opt->second < best_cost) {
            best_cost = simple_insertion_opt->second;
            best_insertion = std::make_pair(simple_insertion_opt->first, &ruta.cisterna);
            best_route_idx = i;
        }
    }

    if (best_insertion && best_route_idx != -1) {
        auto& [new_route_clients, cisterna_ptr] = *best_insertion;
        
        std::map<int, std::vector<std::string>> new_productos_map;
        for (int cid : new_route_clients) {
            const auto& cliente = instance.cliente_por_id(cid);
            std::vector<std::string> prods;
            if (cliente.demanda_gasohol > 0) prods.push_back("G");
            if (cliente.demanda_diesel > 0) prods.push_back("D");
            if (!prods.empty()) new_productos_map[cid] = prods;
        }

        auto new_ruta_opt = evaluator.crear_ruta_objeto(new_route_clients, *cisterna_ptr, new_productos_map);
        if (new_ruta_opt) {
            rutas[best_route_idx] = *new_ruta_opt;
        }
    } else {
        // If no insertion is possible, create a new route
        const auto& cliente = instance.cliente_por_id(vertex_to_be_inserted);
        std::map<int, std::vector<std::string>> productos_map;
        std::vector<std::string> prods;
        if (cliente.demanda_gasohol > 0) prods.push_back("G");
        if (cliente.demanda_diesel > 0) prods.push_back("D");
        productos_map[vertex_to_be_inserted] = prods;

        auto vehiculos_usados = contar_vehiculos_cpp(rutas);
        auto cisterna_opt = evaluator.seleccionar_mejor_cisterna({vertex_to_be_inserted}, productos_map, vehiculos_usados);

        if (cisterna_opt) {
            auto new_ruta_opt = evaluator.crear_ruta_objeto({vertex_to_be_inserted}, *cisterna_opt, productos_map);
            if (new_ruta_opt) {
                rutas.push_back(*new_ruta_opt);
            }
        } else {
            std::cerr << "WARN: Could not insert client " << vertex_to_be_inserted << " and could not create a new route." << std::endl;
        }
    }
    return rutas;
}

std::vector<Ruta> SolverTabuSearchMCVRPTW::perturbation(const std::vector<Ruta>& rutas, int k_max) {
    auto rutas_perturbadas = rutas;
    
    std::vector<int> all_clients;
    for (const auto& r : rutas_perturbadas) {
        all_clients.insert(all_clients.end(), r.clientes.begin(), r.clientes.end());
    }

    if (all_clients.empty()) return rutas;

    std::random_device rd;
    std::mt19937 gen(rd());
    int k = std::uniform_int_distribution<>(1, std::min((int)all_clients.size(), k_max))(gen);

    std::vector<int> clientes_a_remover;
    std::sample(all_clients.begin(), all_clients.end(), std::back_inserter(clientes_a_remover), k, gen);

    for (auto& r : rutas_perturbadas) {
        r.clientes.erase(std::remove_if(r.clientes.begin(), r.clientes.end(), [&](int c) {
            return std::find(clientes_a_remover.begin(), clientes_a_remover.end(), c) != clientes_a_remover.end();
        }), r.clientes.end());
    }

    // Remove empty routes
    rutas_perturbadas.erase(std::remove_if(rutas_perturbadas.begin(), rutas_perturbadas.end(), 
        [](const Ruta& r){ return r.clientes.empty(); }), rutas_perturbadas.end());

    for (int cliente_id : clientes_a_remover) {
        rutas_perturbadas = geni_insertion(rutas_perturbadas, cliente_id);
    }

    std::vector<Ruta> rutas_finales;
    for (auto& r : rutas_perturbadas) {
        if (r.clientes.empty()) continue;

        auto vehiculos_usados = contar_vehiculos_cpp(rutas_finales);
        auto cisterna_opt = evaluator.seleccionar_mejor_cisterna(r.clientes, r.productos_entregados, vehiculos_usados);
        if (cisterna_opt) {
            auto nueva_ruta_opt = evaluator.crear_ruta_objeto(r.clientes, *cisterna_opt, r.productos_entregados);
            if (nueva_ruta_opt) {
                rutas_finales.push_back(*nueva_ruta_opt);
            }
        } else {
            std::cerr << "WARN: Could not find a cistern for a perturbed route. Route discarded." << std::endl;
        }
    }

    return rutas_finales;
}

std::vector<Ruta> SolverTabuSearchMCVRPTW::tabu_search(const std::vector<Ruta>& rutas, int current_iteration) {
    // Calculate tabu tenure
    int n = instance.clientes.size();
    int tau = 1;
    if (n > 0 && r_prime > 0) {
        std::uniform_int_distribution<> dist(1, static_cast<int>(sqrt(n * r_prime)));
        std::random_device rd;
        std::mt19937 gen(rd());
        tau = dist(gen);
    }

    // Find and apply best shift move
    auto new_rutas = _best_shift_move(rutas, current_iteration, tau);

    // Calculate violations and update penalties
    auto violations = _calculate_violations(new_rutas);
    std::uniform_real_distribution<> dist_delta(0.1, 0.3);
    std::random_device rd_delta;
    std::mt19937 gen_delta(rd_delta());
    double delta = dist_delta(gen_delta);
    _update_penalties(violations, delta);

    return new_rutas;
}

SolverTabuSearchMCVRPTW::ViolationInfo SolverTabuSearchMCVRPTW::_calculate_violations(const std::vector<Ruta>& rutas) {
    ViolationInfo violations;
    for (const auto& ruta : rutas) {
        // Capacity violations
        double delta_gasohol = std::max(0.0, ruta.carga_gasohol - ruta.cisterna.cap_gasohol);
        double delta_diesel = std::max(0.0, ruta.carga_diesel - ruta.cisterna.cap_diesel);
        violations.capacity_excess += std::max(delta_gasohol, delta_diesel);

        // Distance/time violations
        double max_tiempo = instance.depot.ventana_fin;
        violations.distance_excess += std::max(0.0, ruta.tiempo_total - max_tiempo);

        // Time window violations
        double tiempo_actual = instance.depot.ventana_inicio;
        int nodo_actual = 0;
        for (int cliente_id : ruta.clientes) {
            const auto& cliente = instance.cliente_por_id(cliente_id);
            double tiempo_viaje = instance.tiempo_viaje(nodo_actual, cliente_id);
            double tiempo_llegada = tiempo_actual + tiempo_viaje;

            if (tiempo_llegada > cliente.ventana_fin) {
                violations.tw_excess += tiempo_llegada - cliente.ventana_fin;
            }
            if (tiempo_llegada < cliente.ventana_inicio) {
                tiempo_llegada = cliente.ventana_inicio;
            }

            auto it = ruta.productos_entregados.find(cliente_id);
            if (it != ruta.productos_entregados.end()) {
                 tiempo_actual = tiempo_llegada + evaluator.calcular_tiempo_servicio(it->second);
            }
           
            nodo_actual = cliente_id;
        }
    }
    violations.has_violations = violations.capacity_excess > 0 || violations.distance_excess > 0 || violations.tw_excess > 0;
    return violations;
}

double SolverTabuSearchMCVRPTW::_calculate_penalized_objective(const std::vector<Ruta>& rutas) {
    auto violations = _calculate_violations(rutas);
    double base_cost = 0.0;
    for (const auto& r : rutas) {
        base_cost += r.costo_total;
    }
    return base_cost + alpha * violations.capacity_excess + beta * violations.distance_excess + rho * violations.tw_excess;
}

std::map<int, std::vector<int>> SolverTabuSearchMCVRPTW::_build_nearest_routes(const std::vector<Ruta>& rutas) {
    if (rutas.empty()) {
        return {};
    }

    double total_distance = 0;
    int total_visits = 0;
    std::vector<int> all_clients;
    for (const auto& r : rutas) {
        total_distance += r.distancia_total;
        total_visits += r.clientes.size();
        all_clients.insert(all_clients.end(), r.clientes.begin(), r.clientes.end());
    }
    double d_hat = (total_visits > 0) ? total_distance / total_visits : 1.0;

    std::map<int, std::vector<int>> nearest_routes;
    for (int cliente_id : all_clients) {
        double threshold = 2 * d_hat;
        std::vector<int> found_routes;
        while (found_routes.empty() && threshold < 10000) { // Safety break
            for (size_t i = 0; i < rutas.size(); ++i) {
                for (int other_client : rutas[i].clientes) {
                    if (instance.distancia(cliente_id, other_client) <= threshold) {
                        found_routes.push_back(i);
                        break;
                    }
                }
            }
            if (found_routes.empty()) {
                threshold *= 1.2;
            }
        }
        std::sort(found_routes.begin(), found_routes.end());
        found_routes.erase(std::unique(found_routes.begin(), found_routes.end()), found_routes.end());
        nearest_routes[cliente_id] = found_routes;
    }
    return nearest_routes;
}

std::optional<std::vector<Ruta>> SolverTabuSearchMCVRPTW::_apply_shift_move(
    std::vector<Ruta> rutas, int source_idx, int dest_idx, int vertex, const std::vector<std::string>& products)
{
    if (source_idx >= rutas.size() || dest_idx >= rutas.size()) return std::nullopt;

    Ruta& source_route = rutas[source_idx];
    
    auto it_v = std::find(source_route.clientes.begin(), source_route.clientes.end(), vertex);
    if (it_v == source_route.clientes.end()) return std::nullopt;

    auto it_p = source_route.productos_entregados.find(vertex);
    if (it_p == source_route.productos_entregados.end()) return std::nullopt;

    std::vector<std::string> original_products = it_p->second;
    std::vector<std::string> remaining_products;
    std::set<std::string> p_set(products.begin(), products.end());

    for(const auto& op : original_products) {
        if(p_set.find(op) == p_set.end()) {
            remaining_products.push_back(op);
        }
    }

    if (remaining_products.empty()) { // Case 1: Move all products
        source_route.clientes.erase(it_v);
        source_route.productos_entregados.erase(it_p);

        if (source_route.clientes.empty()) {
            rutas.erase(rutas.begin() + source_idx);
            if (dest_idx > source_idx) dest_idx--;
        } else {
            auto vehiculos_usados = contar_vehiculos_cpp(rutas);
            if(vehiculos_usados.count(source_route.cisterna.tipo))
                vehiculos_usados[source_route.cisterna.tipo]--;
            auto new_cisterna_opt = evaluator.seleccionar_mejor_cisterna(source_route.clientes, source_route.productos_entregados, vehiculos_usados);
            if (!new_cisterna_opt) return std::nullopt;
            auto new_ruta_opt = evaluator.crear_ruta_objeto(source_route.clientes, *new_cisterna_opt, source_route.productos_entregados);
            if (!new_ruta_opt) return std::nullopt;
            rutas[source_idx] = *new_ruta_opt;
        }
    } else { // Case 2: Split demands
        source_route.productos_entregados[vertex] = remaining_products;
        auto new_ruta_opt = evaluator.crear_ruta_objeto(source_route.clientes, source_route.cisterna, source_route.productos_entregados);
        if (!new_ruta_opt) return std::nullopt;
        rutas[source_idx] = *new_ruta_opt;
    }

    if (dest_idx >= rutas.size()) { // Destination route might have been removed
        // This case needs careful handling. For now, we might fail the move.
        // A better approach would be to re-validate dest_idx or handle creating a new route.
        return std::nullopt;
    }
    Ruta& dest_route = rutas[dest_idx];

    auto it_dest_v = std::find(dest_route.clientes.begin(), dest_route.clientes.end(), vertex);
    if (it_dest_v != dest_route.clientes.end()) { // Vertex already in dest route
        auto& dest_prods = dest_route.productos_entregados[vertex];
        dest_prods.insert(dest_prods.end(), products.begin(), products.end());
        std::sort(dest_prods.begin(), dest_prods.end());
        dest_prods.erase(std::unique(dest_prods.begin(), dest_prods.end()), dest_prods.end());
        
        auto new_ruta_opt = evaluator.crear_ruta_objeto(dest_route.clientes, dest_route.cisterna, dest_route.productos_entregados);
        if (!new_ruta_opt) return std::nullopt; // Could fail if new total demand is infeasible
        rutas[dest_idx] = *new_ruta_opt;
    } else { // Insert new vertex into dest route
        std::vector<int> new_dest_clients = dest_route.clientes;
        new_dest_clients.push_back(vertex);
        
        auto new_dest_prods = dest_route.productos_entregados;
        new_dest_prods[vertex] = products;

        // We need to find the best insertion position
        auto temp_rutas = rutas;
        temp_rutas.erase(temp_rutas.begin() + dest_idx);
        auto vehiculos_usados = contar_vehiculos_cpp(temp_rutas);

        auto result = geni_insertion(std::vector<Ruta>{dest_route}, vertex);
        if (result.size() != 1) return std::nullopt; // Insertion failed
        rutas[dest_idx] = result[0];
    }
    return rutas;
}

bool SolverTabuSearchMCVRPTW::_is_tabu(int vertex, const std::vector<std::string>& products, int source_idx, int iteration) {
    for (const auto& move : tabu_list) {
        if (move.vertex == vertex && move.products == products && move.source_route_idx == source_idx && iteration < move.expiration_iteration) {
            return true;
        }
    }
    return false;
}

void SolverTabuSearchMCVRPTW::_update_tabu_list(int vertex, const std::vector<std::string>& products, int source_idx, int tenure, int iteration) {
    tabu_list.push_back({vertex, products, source_idx, iteration + tenure});
    // Clean up expired moves
    tabu_list.erase(std::remove_if(tabu_list.begin(), tabu_list.end(), 
        [iteration](const TabuMove& move) {
            return move.expiration_iteration <= iteration;
        }), tabu_list.end());
}

std::vector<Ruta> SolverTabuSearchMCVRPTW::_best_shift_move(std::vector<Ruta> rutas, int iteration, int tenure) {
    if (rutas.empty()) return rutas;

    auto nearest_routes = _build_nearest_routes(rutas);
    
    std::optional<std::vector<Ruta>> best_solution_candidate;
    double best_objective = std::numeric_limits<double>::infinity();
    
    struct BestMoveInfo {
        int vertex;
        std::vector<std::string> products;
        int source_idx;
        int dest_idx;
    };
    std::optional<BestMoveInfo> best_move;

    for (size_t source_idx = 0; source_idx < rutas.size(); ++source_idx) {
        // Create a copy of clients to avoid iterator invalidation issues
        std::vector<int> source_clients = rutas[source_idx].clientes;
        for (int vertex : source_clients) {
            if (rutas[source_idx].productos_entregados.find(vertex) == rutas[source_idx].productos_entregados.end()) continue;
            const auto& products_in_source = rutas[source_idx].productos_entregados.at(vertex);
            
            std::vector<std::vector<std::string>> product_subsets;
            if (std::find(products_in_source.begin(), products_in_source.end(), "G") != products_in_source.end()) {
                product_subsets.push_back({"G"});
            }
            if (std::find(products_in_source.begin(), products_in_source.end(), "D") != products_in_source.end()) {
                product_subsets.push_back({"D"});
            }
            if (products_in_source.size() == 2) {
                product_subsets.push_back({"G", "D"});
            }

            for (const auto& products_tuple : product_subsets) {
                auto it_routes = nearest_routes.find(vertex);
                if (it_routes == nearest_routes.end()) continue;

                for (int dest_idx : it_routes->second) {
                    if (dest_idx >= rutas.size() || dest_idx == source_idx) continue;

                    bool is_tabu = _is_tabu(vertex, products_tuple, source_idx, iteration);
                    
                    auto new_solution_opt = _apply_shift_move(rutas, source_idx, dest_idx, vertex, products_tuple);
                    if (!new_solution_opt) continue;

                    double objective = _calculate_penalized_objective(*new_solution_opt);
                    double current_cost = 0;
                    for(const auto& r : *new_solution_opt) current_cost += r.costo_total;
                    
                    bool aspiration = (current_cost < incumbent_cost);

                    if ((!is_tabu || aspiration) && (objective < best_objective)) {
                        best_objective = objective;
                        best_solution_candidate = new_solution_opt;
                        best_move = {vertex, products_tuple, (int)source_idx, dest_idx};
                    }
                }
            }
        }
    }

    if (best_solution_candidate && best_move) {
        _update_tabu_list(best_move->vertex, best_move->products, best_move->source_idx, tenure, iteration);
        
        auto violations = _calculate_violations(*best_solution_candidate);
        if (!violations.has_violations) {
            double actual_cost = 0;
            for(const auto& r : *best_solution_candidate) actual_cost += r.costo_total;
            if (actual_cost < incumbent_cost) {
                incumbent_cost = actual_cost;
                this->best_solution = *best_solution_candidate;
                r_prime = this->best_solution.size();
            }
        }
        return *best_solution_candidate;
    }

    return rutas;
}

void SolverTabuSearchMCVRPTW::_update_penalties(const ViolationInfo& violations, double delta) {
    if (violations.has_violations) {
        if (violations.capacity_excess > 0) {
            alpha = alpha * (1 + delta);
        }
        if (violations.distance_excess > 0) {
            beta = beta * (1 + delta);
        }
    } else {
        alpha = alpha / (1 + delta);
        beta = beta / (1 + delta);
    }
}
