#include "solvers.h"
#include <numeric>
#include <iostream>
#include <algorithm>

SweepAlgorithm::SweepAlgorithm(const ProblemInstance& instance)
    : instance(instance), evaluator(instance) {}

std::vector<Ruta> SweepAlgorithm::forward_sweep() {
    std::vector<Ruta> rutas;
    std::vector<int> clientes_no_asignados;
    for(const auto& c : instance.clientes) {
        clientes_no_asignados.push_back(c.id);
    }

    std::map<int, int> vehiculos_usados = {{1, 0}, {2, 0}};

    while (!clientes_no_asignados.empty()) {
        std::vector<int> cluster_actual;
        std::map<int, std::vector<std::string>> productos_cluster;

        for (int cliente_id : clientes_no_asignados) {
            std::vector<int> cluster_test = cluster_actual;
            cluster_test.push_back(cliente_id);

            std::map<int, std::vector<std::string>> productos_test = productos_cluster;
            const auto& cliente = instance.cliente_por_id(cliente_id);
            
            std::vector<std::string> prods;
            if (cliente.demanda_gasohol > 0) prods.push_back("G");
            if (cliente.demanda_diesel > 0) prods.push_back("D");
            productos_test[cliente_id] = prods;

            auto mejor_cisterna = evaluator.seleccionar_mejor_cisterna(cluster_test, productos_test, vehiculos_usados);

            if (mejor_cisterna) {
                cluster_actual = cluster_test;
                productos_cluster = productos_test;
            } else {
                break; 
            }
        }

        if (cluster_actual.empty()) {
            std::cerr << "Error: No se pudo asignar un cliente. Posiblemente no hay cisternas factibles." << std::endl;
            // Handle error, maybe by breaking the loop or trying a different strategy
            if (!clientes_no_asignados.empty()) {
                 std::cerr << "Cliente no asignado: " << clientes_no_asignados[0] << std::endl;
            }
            break;
        }
        
        auto cisterna_final = evaluator.seleccionar_mejor_cisterna(cluster_actual, productos_cluster, vehiculos_usados);
        
        if(cisterna_final.has_value()){
            Ruta nueva_ruta = evaluator.crear_ruta_objeto(cluster_actual, cisterna_final.value(), productos_cluster);
            rutas.push_back(nueva_ruta);
            vehiculos_usados[cisterna_final->tipo]++;

            std::vector<int> temp_no_asignados;
            for(int c_id : clientes_no_asignados){
                bool found = false;
                for(int c_cluster_id : cluster_actual){
                    if(c_id == c_cluster_id){
                        found = true;
                        break;
                    }
                }
                if(!found){
                    temp_no_asignados.push_back(c_id);
                }
            }
            clientes_no_asignados = temp_no_asignados;
        } else {
             std::cerr << "Error: No se encontró cisterna para el cluster final." << std::endl;
             break;
        }
    }
    return rutas;
}

std::map<int, std::vector<std::string>> build_product_map(const std::vector<int>& clientes_list, const Ruta& route_k, const Ruta& route_k_plus_1) {
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

double SweepAlgorithm::_calcular_score_eliminacion(int cliente_id, double avg_radius) {
    const auto& cliente = instance.cliente_por_id(cliente_id);
    if (avg_radius == 0) return cliente.radio;
    return cliente.radio / avg_radius;
}

std::optional<int> SweepAlgorithm::_seleccionar_cliente_a_eliminar(const Ruta& ruta, double avg_radius) {
    if (ruta.clientes.empty()) {
        return std::nullopt;
    }

    std::map<int, double> scores;
    for (int cid : ruta.clientes) {
        scores[cid] = _calcular_score_eliminacion(cid, avg_radius);
    }

    int best_client = -1;
    double min_score = std::numeric_limits<double>::max();
    for(auto const& [cid, score] : scores){
        if(score < min_score){
            min_score = score;
            best_client = cid;
        }
    }
    return best_client;
}

std::vector<int> SweepAlgorithm::_obtener_candidatos_ordenados(const Ruta& ruta_k, const Ruta& ruta_k_plus_1) {
    std::vector<int> candidatos;
    for(auto const& [cid, prods] : ruta_k_plus_1.productos_entregados){
        candidatos.push_back(cid);
    }
    // This sorting is implicit in python as it iterates over a dictionary that was created from a sorted list
    std::sort(candidatos.begin(), candidatos.end());
    return candidatos;
}

std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>>
SweepAlgorithm::_intentar_insercion_greedy(const std::vector<int>& base_clientes,
                                           const std::vector<int>& clientes_a_insertar,
                                           const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                                           std::map<int, int>& vehiculos_disponibles) {
    std::vector<int> ruta_actual = base_clientes;
    for (int cliente_id : clientes_a_insertar) {
        auto mejor_pos = _encontrar_mejor_posicion_insercion(ruta_actual, cliente_id, ruta_k, ruta_k_plus_1, vehiculos_disponibles);
        if (mejor_pos) {
            auto [pos, cisterna, prods, costo] = *mejor_pos;
            ruta_actual.insert(ruta_actual.begin() + pos, cliente_id);
        } else {
            return std::nullopt;
        }
    }

    auto product_map = build_product_map(ruta_actual, ruta_k, ruta_k_plus_1);
    auto cisterna_final = evaluator.seleccionar_mejor_cisterna(ruta_actual, product_map, vehiculos_disponibles);

    if (cisterna_final) {
        double costo_final = cisterna_final->costo_fijo + cisterna_final->costo_km * evaluator.calcular_distancia_ruta(ruta_actual);
        return std::make_tuple(ruta_actual, *cisterna_final, product_map, costo_final);
    }
    return std::nullopt;
}

std::optional<std::tuple<int, Cisterna, std::map<int, std::vector<std::string>>, double>>
SweepAlgorithm::_encontrar_mejor_posicion_insercion(const std::vector<int>& ruta_actual, int cliente_id,
                                                    const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                                                    std::map<int, int>& vehiculos_disponibles) {
    double mejor_costo_incremental = std::numeric_limits<double>::max();
    std::optional<std::tuple<int, Cisterna, std::map<int, std::vector<std::string>>, double>> mejor_resultado = std::nullopt;

    for (size_t i = 0; i <= ruta_actual.size(); ++i) {
        std::vector<int> ruta_test = ruta_actual;
        ruta_test.insert(ruta_test.begin() + i, cliente_id);

        auto product_map = build_product_map(ruta_test, ruta_k, ruta_k_plus_1);
        auto cisterna = evaluator.seleccionar_mejor_cisterna(ruta_test, product_map, vehiculos_disponibles);

        if (cisterna) {
            double costo_actual = cisterna->costo_fijo + cisterna->costo_km * evaluator.calcular_distancia_ruta(ruta_test);
            if (costo_actual < mejor_costo_incremental) {
                mejor_costo_incremental = costo_actual;
                mejor_resultado = std::make_tuple(i, *cisterna, product_map, costo_actual);
            }
        }
    }
    return mejor_resultado;
}

std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>>
SweepAlgorithm::_reconstruir_ruta_k_plus_1(const std::vector<int>& base_clientes, int cliente_a_insertar,
                                           const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                                           std::map<int, int>& vehiculos_disponibles) {
    if (base_clientes.empty()) {
        return std::nullopt;
    }
    return _intentar_insercion_greedy(base_clientes, {cliente_a_insertar}, ruta_k, ruta_k_plus_1, vehiculos_disponibles);
}

std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>>
SweepAlgorithm::_buscar_mejor_intercambio(const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                                          int cliente_eliminar, const std::vector<int>& candidatos,
                                          std::map<int, int>& vehiculos_disponibles) {
    double mejor_ahorro = -std::numeric_limits<double>::max();
    std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>> mejor_swap = std::nullopt;

    std::vector<int> ruta_k_base;
    for(int c : ruta_k.clientes) {
        if (c != cliente_eliminar) {
            ruta_k_base.push_back(c);
        }
    }

    for (int candidato_id : candidatos) {
        auto res_k = _intentar_insercion_greedy(ruta_k_base, {candidato_id}, ruta_k, ruta_k_plus_1, vehiculos_disponibles);
        if (!res_k) continue;
        
        auto [nueva_ruta_k_clientes, cisterna_k, prods_k, costo_k] = *res_k;

        std::vector<int> ruta_k_plus_1_base;
        for(int c : ruta_k_plus_1.clientes) {
            if (c != candidato_id) {
                ruta_k_plus_1_base.push_back(c);
            }
        }

        auto res_k1 = _reconstruir_ruta_k_plus_1(ruta_k_plus_1_base, cliente_eliminar, ruta_k, ruta_k_plus_1, vehiculos_disponibles);
        if (!res_k1) continue;

        auto [nueva_ruta_k1_clientes, cisterna_k1, prods_k1, costo_k1] = *res_k1;

        double ahorro = (ruta_k.costo_total + ruta_k_plus_1.costo_total) - (costo_k + costo_k1);
        if (ahorro > mejor_ahorro) {
            mejor_ahorro = ahorro;
            mejor_swap = std::make_tuple(nueva_ruta_k_clientes, cisterna_k, prods_k, nueva_ruta_k1_clientes, cisterna_k1, prods_k1, ahorro);
        }
    }
    return mejor_swap;
}

Ruta SweepAlgorithm::_crear_ruta_desde_swap(const std::vector<int>& clientes, const Cisterna& cisterna,
                                            const std::map<int, std::vector<std::string>>& info,
                                            const Ruta& ruta_original_k, const Ruta& ruta_original_k_plus_1) {
    auto product_map = build_product_map(clientes, ruta_original_k, ruta_original_k_plus_1);
    return evaluator.crear_ruta_objeto(clientes, cisterna, product_map);
}


std::tuple<std::vector<Ruta>, bool> SweepAlgorithm::improving_sweep(std::vector<Ruta> rutas_candidatas, bool clockwise) {
    bool improved = false;
    int n_rutas = rutas_candidatas.size();
    if (n_rutas < 2) {
        return {rutas_candidatas, false};
    }

    for (int i = 0; i < n_rutas; ++i) {
        int k_idx = clockwise ? (n_rutas - 1 - i) : i;
        int k_plus_1_idx = (k_idx + 1) % n_rutas;

        Ruta& ruta_k = rutas_candidatas[k_idx];
        Ruta& ruta_k_plus_1 = rutas_candidatas[k_plus_1_idx];

        double avg_radius_k = 0;
        for(int cid : ruta_k.clientes) avg_radius_k += instance.cliente_por_id(cid).radio;
        if(!ruta_k.clientes.empty()) avg_radius_k /= ruta_k.clientes.size();

        auto cliente_a_eliminar_opt = _seleccionar_cliente_a_eliminar(ruta_k, avg_radius_k);
        if (!cliente_a_eliminar_opt) continue;
        int cliente_a_eliminar = *cliente_a_eliminar_opt;

        std::map<int, int> vehiculos_disponibles = {{1, 0}, {2, 0}};
        for(const auto& r : rutas_candidatas) vehiculos_disponibles[r.cisterna.tipo]++;
        vehiculos_disponibles[ruta_k.cisterna.tipo]--;
        vehiculos_disponibles[ruta_k_plus_1.cisterna.tipo]--;

        auto candidatos = _obtener_candidatos_ordenados(ruta_k, ruta_k_plus_1);
        auto mejor_swap = _buscar_mejor_intercambio(ruta_k, ruta_k_plus_1, cliente_a_eliminar, candidatos, vehiculos_disponibles);

        if (mejor_swap && std::get<6>(*mejor_swap) > 0) {
            auto [clientes_k, cis_k, prods_k, clientes_k1, cis_k1, prods_k1, ahorro] = *mejor_swap;
            
            rutas_candidatas[k_idx] = _crear_ruta_desde_swap(clientes_k, cis_k, prods_k, ruta_k, ruta_k_plus_1);
            rutas_candidatas[k_plus_1_idx] = _crear_ruta_desde_swap(clientes_k1, cis_k1, prods_k1, ruta_k, ruta_k_plus_1);
            improved = true;
        }
    }
    return {rutas_candidatas, improved};
}

std::vector<Ruta> SweepAlgorithm::iterative_improving_sweep(std::vector<Ruta> rutas_candidatas) {
    int max_iter = 10;
    for (int i = 0; i < max_iter; ++i) {
        bool improved_ccw = false;
        std::vector<Ruta> rutas_despues_ccw;
        std::tie(rutas_despues_ccw, improved_ccw) = improving_sweep(rutas_candidatas, false);
        if (improved_ccw) {
            rutas_candidatas = rutas_despues_ccw;
        }

        bool improved_cw = false;
        std::vector<Ruta> rutas_despues_cw;
        std::tie(rutas_despues_cw, improved_cw) = improving_sweep(rutas_candidatas, true);
        if (improved_cw) {
            rutas_candidatas = rutas_despues_cw;
        }

        if (!improved_ccw && !improved_cw) {
            break;
        }
    }
    return rutas_candidatas;
}
