#ifndef ROUTE_EVALUATOR_H
#define ROUTE_EVALUATOR_H

#include "models.h"
#include <tuple>
#include <optional>

class RouteEvaluator {
public:
    RouteEvaluator(ProblemInstance& instance);

    double calcular_tiempo_servicio(const std::vector<std::string>& productos) const;

    std::tuple<bool, double, std::map<std::string, double>>
    verificar_factibilidad_ruta(const std::vector<int>& ruta, const Cisterna& cisterna,
                               const std::map<int, std::vector<std::string>>& productos_por_cliente);

    double calcular_distancia_ruta(const std::vector<int>& ruta) const;

    std::optional<Cisterna> seleccionar_mejor_cisterna(const std::vector<int>& ruta,
                                                       const std::map<int, std::vector<std::string>>& productos_por_cliente,
                                                       std::map<int, int>& vehiculos_usados);
    
    std::optional<Ruta> crear_ruta_objeto(const std::vector<int>& clientes, const Cisterna& cisterna,
                           const std::map<int, std::vector<std::string>>& productos_por_cliente);


private:
    ProblemInstance& instance;
};

#endif // ROUTE_EVALUATOR_H
