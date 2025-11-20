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
    // Constructor que inicializa el algoritmo con una instancia del problema.
    SweepAlgorithm(ProblemInstance& instance);
    // Construye una solución inicial usando el algoritmo de barrido angular (forward sweep).
    std::vector<Ruta> forward_sweep();
    // Intenta mejorar una solución intercambiando clientes entre rutas adyacentes.
    std::pair<std::vector<Ruta>, bool> improving_sweep(const std::vector<Ruta>& rutas_candidatas, bool clockwise = false);
    // Aplica el 'improving_sweep' iterativamente en ambas direcciones hasta que no haya más mejoras.
    std::vector<Ruta> iterative_improving_sweep(const std::vector<Ruta>& rutas_candidatas);

    ProblemInstance& get_instance() { return instance; }
    RouteEvaluator& get_evaluator() { return evaluator; }
    const std::vector<Ruta>& get_best_solution() const { return best_solution; }

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

class SolverTabuSearchMCVRPTW {
public:
    // Constructor que inicializa el solver de búsqueda tabú.
    SolverTabuSearchMCVRPTW(SweepAlgorithm& sweep_solver);

    // Aplica una perturbación a la solución actual para escapar de óptimos locales.
    std::vector<Ruta> perturbation(const std::vector<Ruta>& rutas, int k_max = 5);
    // Ejecuta el algoritmo de búsqueda tabú para mejorar una solución.
    std::vector<Ruta> tabu_search(const std::vector<Ruta>& rutas, int current_iteration);
    const std::vector<Ruta>& get_best_solution() const { return best_solution; }


private:
    SweepAlgorithm& sweep;
    RouteEvaluator& evaluator;
    ProblemInstance& instance;
    
    std::vector<Ruta> best_solution;
    double alpha = 1.0;
    double beta = 1.0;
    const double rho = 10.0;
    
    struct TabuMove {
        int vertex;
        std::vector<std::string> products;
        int source_route_idx;
        int expiration_iteration;

        bool operator==(const TabuMove& other) const {
            return vertex == other.vertex &&
                   products == other.products &&
                   source_route_idx == other.source_route_idx;
        }
    };

    std::vector<TabuMove> tabu_list;
    double incumbent_cost;
    int r_prime;

    std::map<int, std::vector<int>> _build_p_neighborhoods(const std::vector<int>& clientes, int p);
    
    std::optional<double> _calculate_insertion_cost(
        const std::vector<int>& route_clients, 
        int insert_pos, 
        int vertex, 
        const Cisterna& cisterna, 
        const std::map<int, std::vector<std::string>>& productos_map
    );

    std::optional<std::pair<std::vector<int>, double>> _try_simple_insertion(
        const std::vector<int>& route_clients, 
        int v, 
        const Cisterna& cisterna, 
        std::map<int, std::vector<std::string>> productos_map
    );

    std::vector<Ruta> geni_insertion(std::vector<Ruta> rutas, int vertex_to_be_inserted, int p = 5);

    struct ViolationInfo {
        double capacity_excess = 0.0;
        double distance_excess = 0.0;
        double tw_excess = 0.0;
        bool has_violations = false;
    };

    ViolationInfo _calculate_violations(const std::vector<Ruta>& rutas);
    double _calculate_penalized_objective(const std::vector<Ruta>& rutas);
    std::map<int, std::vector<int>> _build_nearest_routes(const std::vector<Ruta>& rutas);
    std::optional<std::vector<Ruta>> _apply_shift_move(
        std::vector<Ruta> rutas, 
        int source_idx, 
        int dest_idx,
        int vertex, 
        const std::vector<std::string>& products
    );
    bool _is_tabu(int vertex, const std::vector<std::string>& products, int source_idx, int iteration);
    void _update_tabu_list(int vertex, const std::vector<std::string>& products, int source_idx, int tenure, int iteration);
    std::vector<Ruta> _best_shift_move(std::vector<Ruta> rutas, int iteration, int tenure);
    void _update_penalties(const ViolationInfo& violations, double delta);
};

#endif // SOLVERS_H
