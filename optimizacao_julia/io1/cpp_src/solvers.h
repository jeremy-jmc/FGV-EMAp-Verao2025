#ifndef SOLVERS_H
#define SOLVERS_H

#include "models.h"
#include "route_evaluator.h"
#include <vector>
#include <optional>
#include <string>
#include <map>

class SweepAlgorithm {
public:
    SweepAlgorithm(ProblemInstance& instance);
    std::vector<Ruta> forward_sweep();
    std::pair<std::vector<Ruta>, bool> improving_sweep(const std::vector<Ruta>& rutas_candidatas, bool clockwise = false);
    std::vector<Ruta> iterative_improving_sweep(const std::vector<Ruta>& rutas_candidatas);

    struct InsercionInfo {
        int pos;
        Cisterna cisterna;
        std::map<std::string, double> info;
        double costo;
    };

    struct GreedyInsercionResult {
        std::vector<int> ruta;
        Cisterna cisterna;
        std::map<std::string, double> info;
        double costo;
    };

    struct ReconstruccionResult {
        std::vector<int> ruta;
        Cisterna cisterna;
        std::map<std::string, double> info;
        double costo;
    };

    struct SwapResult {
        std::vector<int> nueva_k;
        Cisterna cisterna_k;
        std::map<std::string, double> info_k;
        std::vector<int> nueva_k1;
        Cisterna cisterna_k1;
        std::map<std::string, double> info_k1;
        double nuevo_costo;
    };

private:
    ProblemInstance& instance;
    RouteEvaluator evaluator;
    std::vector<Ruta> best_solution;

    double calcular_score_eliminacion(int cliente_id, double avg_radius);
    std::optional<int> seleccionar_cliente_a_eliminar(const Ruta& ruta, double avg_radius);
    std::vector<int> obtener_candidatos_ordenados(const Ruta& ruta_k, const Ruta& ruta_k_plus_1);

    std::optional<InsercionInfo> encontrar_mejor_posicion_insercion(
        const std::vector<int>& ruta_actual,
        int cliente_id,
        const Ruta& ruta_k,
        const Ruta& ruta_k_plus_1,
        std::map<int, int> vehiculos_disponibles
    );

    std::optional<GreedyInsercionResult> intentar_insercion_greedy(
        const std::vector<int>& base_clientes,
        const std::vector<int>& clientes_a_insertar,
        const Ruta& ruta_k,
        const Ruta& ruta_k_plus_1,
        std::map<int, int> vehiculos_disponibles
    );

    std::optional<ReconstruccionResult> reconstruir_ruta_k_plus_1(
        const std::vector<int>& base_clientes,
        int cliente_a_insertar,
        const Ruta& ruta_k,
        const Ruta& ruta_k_plus_1,
        std::map<int, int> vehiculos_disponibles
    );

    std::optional<SwapResult> buscar_mejor_intercambio(
        const Ruta& ruta_k,
        const Ruta& ruta_k_plus_1,
        int cliente_eliminar,
        const std::vector<int>& candidatos,
        std::map<int, int> vehiculos_disponibles
    );

    Ruta crear_ruta_desde_swap(
        const std::vector<int>& clientes,
        const Cisterna& cisterna,
        const Ruta& ruta_original_k,
        const Ruta& ruta_original_k_plus_1
    );
};

#endif // SOLVERS_H
