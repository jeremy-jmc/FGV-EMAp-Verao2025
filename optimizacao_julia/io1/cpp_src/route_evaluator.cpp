#include "route_evaluator.h"
#include <numeric>
#include <algorithm>
#include <iostream>

RouteEvaluator::RouteEvaluator(ProblemInstance& instance) : instance(instance) {}

double RouteEvaluator::calcular_tiempo_servicio(const std::vector<std::string>& productos) const {
    return productos.size() * instance.tiempo_descarga;
}

std::tuple<bool, double, std::map<std::string, double>>
RouteEvaluator::verificar_factibilidad_ruta(const std::vector<int>& ruta, const Cisterna& cisterna,
                                           const std::map<int, std::vector<std::string>>& productos_por_cliente) {
    double carga_gasohol = 0;
    double carga_diesel = 0;

    for (int cliente_id : ruta) {
        const auto& cliente = instance.cliente_por_id(cliente_id);
        auto it = productos_por_cliente.find(cliente_id);
        if (it != productos_por_cliente.end()) {
            for (const auto& prod : it->second) {
                if (prod == "G") carga_gasohol += cliente.demanda_gasohol;
                if (prod == "D") carga_diesel += cliente.demanda_diesel;
            }
        }
    }

    if (carga_gasohol > cisterna.cap_gasohol || carga_diesel > cisterna.cap_diesel) {
        return {false, 0.0, {{"razon", 1}}}; // 1: capacidad_excedida
    }

    double tiempo_actual = instance.depot.ventana_inicio;
    int nodo_actual = 0;
    std::map<std::string, double> info;
    std::map<int, double> tiempos_llegada_map;

    for (int cliente_id : ruta) {
        const auto& cliente = instance.cliente_por_id(cliente_id);
        double tiempo_viaje = instance.tiempo_viaje(nodo_actual, cliente_id);
        double tiempo_llegada = tiempo_actual + tiempo_viaje;

        if (tiempo_llegada < cliente.ventana_inicio) {
            tiempo_llegada = cliente.ventana_inicio;
        }

        if (tiempo_llegada > cliente.ventana_fin) {
            return {false, 0.0, {{"razon", 2}, {"cliente", (double)cliente_id}}}; // 2: ventana_tiempo_violada
        }
        
        tiempos_llegada_map[cliente_id] = tiempo_llegada;

        auto it = productos_por_cliente.find(cliente_id);
        if (it != productos_por_cliente.end()) {
            tiempo_actual = tiempo_llegada + calcular_tiempo_servicio(it->second);
        } else {
            tiempo_actual = tiempo_llegada;
        }

        nodo_actual = cliente_id;
    }

    double tiempo_viaje_retorno = instance.tiempo_viaje(nodo_actual, 0);
    double tiempo_retorno = tiempo_actual + tiempo_viaje_retorno;

    if (tiempo_retorno > instance.depot.ventana_fin) {
        return {false, 0.0, {{"razon", 3}}}; // 3: retorno_tardio
    }
    
    info["carga_gasohol"] = carga_gasohol;
    info["carga_diesel"] = carga_diesel;
    // Note: We can't easily return the map of arrival times here with the current structure.
    // The calling function will need to recalculate if needed, or we refactor.
    // For now, we return an empty map for times.
    info["tiempo_retorno"] = tiempo_retorno;


    return {true, tiempo_retorno, info};
}

double RouteEvaluator::calcular_distancia_ruta(const std::vector<int>& ruta) const {
    if (ruta.empty()) {
        return 0.0;
    }

    double distancia = instance.distancia(0, ruta[0]);
    for (size_t i = 0; i < ruta.size() - 1; ++i) {
        distancia += instance.distancia(ruta[i], ruta[i + 1]);
    }
    distancia += instance.distancia(ruta.back(), 0);

    return distancia;
}

std::optional<Cisterna> RouteEvaluator::seleccionar_mejor_cisterna(const std::vector<int>& ruta,
                                                                   const std::map<int, std::vector<std::string>>& productos_por_cliente,
                                                                   std::map<int, int>& vehiculos_usados) {
    std::vector<std::pair<Cisterna, double>> cisternas_factibles;

    for (const auto& cisterna : instance.cisternas_disponibles) {
        auto it = vehiculos_usados.find(cisterna.tipo);
        if (it != vehiculos_usados.end() && it->second >= instance.num_vehiculos_por_tipo) {
            continue;
        }

        auto [factible, tiempo_total, info] = verificar_factibilidad_ruta(ruta, cisterna, productos_por_cliente);
        if (factible) {
            double distancia = calcular_distancia_ruta(ruta);
            double costo = cisterna.costo_fijo + cisterna.costo_km * distancia;
            cisternas_factibles.push_back({cisterna, costo});
        }
    }

    if (cisternas_factibles.empty()) {
        return std::nullopt;
    }

    std::sort(cisternas_factibles.begin(), cisternas_factibles.end(),
              [](const auto& a, const auto& b) {
                  return a.second < b.second;
              });

    return cisternas_factibles[0].first;
}

std::optional<Ruta> RouteEvaluator::crear_ruta_objeto(const std::vector<int>& clientes, const Cisterna& cisterna,
                                       const std::map<int, std::vector<std::string>>& productos_por_cliente) {
    auto [factible, tiempo_total, info] = verificar_factibilidad_ruta(clientes, cisterna, productos_por_cliente);
    if (!factible) {
        // Even if not feasible, we might want to create the object to inspect it.
        // Let's calculate basic info.
        double distancia = calcular_distancia_ruta(clientes);
        double costo = cisterna.costo_fijo + cisterna.costo_km * distancia;
        double carga_gasohol = 0;
        double carga_diesel = 0;
        for (int cid : clientes) {
            auto it = productos_por_cliente.find(cid);
            if (it != productos_por_cliente.end()) {
                const auto& cliente = instance.cliente_por_id(cid);
                for (const auto& prod : it->second) {
                    if (prod == "G") carga_gasohol += cliente.demanda_gasohol;
                    if (prod == "D") carga_diesel += cliente.demanda_diesel;
                }
            }
        }
        return Ruta{cisterna, clientes, carga_gasohol, carga_diesel, distancia, 0.0, costo, false, productos_por_cliente, {}};
    }

    double distancia = calcular_distancia_ruta(clientes);
    double costo = cisterna.costo_fijo + cisterna.costo_km * distancia;

    // Recalculate arrival times to store them
    std::vector<double> tiempos_llegada;
    double tiempo_actual = instance.depot.ventana_inicio;
    int nodo_actual = 0;
    for (int cliente_id : clientes) {
        const auto& cliente = instance.cliente_por_id(cliente_id);
        double tiempo_viaje = instance.tiempo_viaje(nodo_actual, cliente_id);
        double tiempo_llegada = tiempo_actual + tiempo_viaje;
        if (tiempo_llegada < cliente.ventana_inicio) {
            tiempo_llegada = cliente.ventana_inicio;
        }
        tiempos_llegada.push_back(tiempo_llegada);
        tiempo_actual = tiempo_llegada + calcular_tiempo_servicio(productos_por_cliente.at(cliente_id));
        nodo_actual = cliente_id;
    }


    return Ruta{
        cisterna,
        clientes,
        info["carga_gasohol"],
        info["carga_diesel"],
        distancia,
        tiempo_total,
        costo,
        factible,
        productos_por_cliente,
        tiempos_llegada
    };
}
