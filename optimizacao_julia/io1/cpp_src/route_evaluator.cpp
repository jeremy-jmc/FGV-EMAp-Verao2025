#include "route_evaluator.h"
#include <numeric>
#include <algorithm>
#include <iostream>

RouteEvaluator::RouteEvaluator(const ProblemInstance& instance) : instance(instance) {}

double RouteEvaluator::calcular_tiempo_servicio(const std::vector<std::string>& productos) const {
    return productos.size() * instance.tiempo_descarga;
}

std::tuple<bool, double, std::map<std::string, std::vector<double>>>
RouteEvaluator::verificar_factibilidad_ruta(const std::vector<int>& ruta, const Cisterna& cisterna,
                                           const std::map<int, std::vector<std::string>>& productos_por_cliente) const {
    double carga_gasohol = 0;
    double carga_diesel = 0;

    for (int cliente_id : ruta) {
        const auto& cliente = instance.cliente_por_id(cliente_id);
        carga_gasohol += cliente.demanda_gasohol;
        carga_diesel += cliente.demanda_diesel;
    }

    if (carga_gasohol > cisterna.cap_gasohol || carga_diesel > cisterna.cap_diesel) {
        return {false, -1.0, {}};
    }

    double tiempo_actual = instance.depot.ventana_inicio;
    int nodo_actual = 0;
    std::map<std::string, std::vector<double>> info;
    std::vector<double> tiempos_llegada;

    for (int cliente_id : ruta) {
        double tiempo_viaje = instance.tiempo_viaje(nodo_actual, cliente_id);
        tiempo_actual += tiempo_viaje;
        const auto& cliente = instance.cliente_por_id(cliente_id);

        if (tiempo_actual < cliente.ventana_inicio) {
            tiempo_actual = cliente.ventana_inicio;
        }

        if (tiempo_actual > cliente.ventana_fin) {
            return {false, -1.0, {}};
        }
        
        tiempos_llegada.push_back(tiempo_actual);

        auto it = productos_por_cliente.find(cliente_id);
        if (it != productos_por_cliente.end()) {
            tiempo_actual += calcular_tiempo_servicio(it->second);
        }

        nodo_actual = cliente_id;
    }

    double tiempo_viaje_retorno = instance.tiempo_viaje(nodo_actual, 0);
    double tiempo_retorno = tiempo_actual + tiempo_viaje_retorno;

    if (tiempo_retorno > instance.depot.ventana_fin) {
        return {false, -1.0, {}};
    }
    
    info["tiempos_llegada"] = tiempos_llegada;

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
                                                                   const std::map<int, int>& vehiculos_usados) const {
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

Ruta RouteEvaluator::crear_ruta_objeto(const std::vector<int>& clientes, const Cisterna& cisterna,
                                       const std::map<int, std::vector<std::string>>& productos_por_cliente) {
    auto [factible, tiempo_total, info] = verificar_factibilidad_ruta(clientes, cisterna, productos_por_cliente);
    double distancia = calcular_distancia_ruta(clientes);
    double costo = cisterna.costo_fijo + cisterna.costo_km * distancia;

    double carga_gasohol = 0;
    double carga_diesel = 0;
    for (int cliente_id : clientes) {
        const auto& cliente = instance.cliente_por_id(cliente_id);
        carga_gasohol += cliente.demanda_gasohol;
        carga_diesel += cliente.demanda_diesel;
    }

    std::vector<double> tiempos_llegada;
    if (factible && info.count("tiempos_llegada")) {
        tiempos_llegada = info["tiempos_llegada"];
    }

    return Ruta{
        cisterna,
        clientes,
        carga_gasohol,
        carga_diesel,
        distancia,
        tiempo_total,
        costo,
        factible,
        productos_por_cliente,
        tiempos_llegada
    };
}
