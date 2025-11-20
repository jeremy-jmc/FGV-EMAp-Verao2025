#include "solvers.h"
#include <iostream>
#include <numeric>
#include <algorithm>
#include <cmath>
#include <iomanip>

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
