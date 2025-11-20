#ifndef SOLVERS_H
#define SOLVERS_H

#include "models.h"
#include "route_evaluator.h"
#include <vector>
#include <optional>

class SweepAlgorithm {
public:
    SweepAlgorithm(const ProblemInstance& instance);
    std::vector<Ruta> forward_sweep();
    std::vector<Ruta> iterative_improving_sweep(std::vector<Ruta> rutas_candidatas);

private:
    const ProblemInstance& instance;
    RouteEvaluator evaluator;
    std::optional<std::vector<Ruta>> best_solution;

    std::tuple<std::vector<Ruta>, bool> improving_sweep(std::vector<Ruta> rutas_candidatas, bool clockwise = false);
    double _calcular_score_eliminacion(int cliente_id, double avg_radius);
    std::optional<int> _seleccionar_cliente_a_eliminar(const Ruta& ruta, double avg_radius);
    std::vector<int> _obtener_candidatos_ordenados(const Ruta& ruta_k, const Ruta& ruta_k_plus_1);
    
    std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>>
    _intentar_insercion_greedy(const std::vector<int>& base_clientes,
                               const std::vector<int>& clientes_a_insertar,
                               const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                               std::map<int, int>& vehiculos_disponibles);

    std::optional<std::tuple<int, Cisterna, std::map<int, std::vector<std::string>>, double>>
    _encontrar_mejor_posicion_insercion(const std::vector<int>& ruta_actual, int cliente_id,
                                        const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                                        std::map<int, int>& vehiculos_disponibles);

    std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>>
    _reconstruir_ruta_k_plus_1(const std::vector<int>& base_clientes, int cliente_a_insertar,
                               const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                               std::map<int, int>& vehiculos_disponibles);

    std::optional<std::tuple<std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, std::vector<int>, Cisterna, std::map<int, std::vector<std::string>>, double>>
    _buscar_mejor_intercambio(const Ruta& ruta_k, const Ruta& ruta_k_plus_1,
                              int cliente_eliminar, const std::vector<int>& candidatos,
                              std::map<int, int>& vehiculos_disponibles);

    Ruta _crear_ruta_desde_swap(const std::vector<int>& clientes, const Cisterna& cisterna,
                                const std::map<int, std::vector<std::string>>& info,
                                const Ruta& ruta_original_k, const Ruta& ruta_original_k_plus_1);
};

std::map<int, std::vector<std::string>> build_product_map(const std::vector<int>& clientes_list, const Ruta& route_k, const Ruta& route_k_plus_1);

#endif // SOLVERS_H
