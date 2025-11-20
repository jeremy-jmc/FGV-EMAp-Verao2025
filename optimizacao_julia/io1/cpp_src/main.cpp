#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <iomanip> // Required for std::setprecision
#include "csv_reader.h"
#include "models.h"
#include "solvers.h"
#include "solution_visualizer.h"

std::string clockwise_angle_str(double angle_ccw) {
    double angle = fmod(-angle_ccw, 2 * M_PI);
    return std::to_string(angle);
}

int main() {
    std::string file_name = "../instancias/25_clientes_1.csv";
    std::string file_summary = "25_c_1";

    CSVReader reader(file_name);
    auto base_data = reader.getData();

    std::map<int, std::map<std::string, double>> tipos_cisternas = {
        {1, {{"cap_gasohol", 5800}, {"cap_diesel", 5200}, {"costo_fijo", 450}, {"costo_km", 2}}},
        {2, {{"cap_gasohol", 4000}, {"cap_diesel", 4000}, {"costo_fijo", 370}, {"costo_km", 2}}}
    };

    for (bool cc : {false, true}) {
        for (bool sd : {false, true}) {
            std::cout << "\n--- Clockwise: " << cc << " | Split Demands: " << sd << " ---\n";
            
            auto data = base_data;
            std::string prefix = file_summary;
            prefix += (cc ? "_cw" : "_ccw");
            prefix += (sd ? "_sd" : "_nsd");

            ProblemInstance instance(data, tipos_cisternas, cc, sd);
            SolutionVisualizer visualizer(instance);
            SweepAlgorithm sweep_solver(instance);

            auto rutas = sweep_solver.forward_sweep();
            visualizer.imprimir_solucion(rutas, 1, "forward_sweep", 0, prefix);
            
            auto improved_rutas = sweep_solver.iterative_improving_sweep(rutas);
            visualizer.imprimir_solucion(improved_rutas, 1, "improving_sweep", 0, prefix);

            // Iterated Tabu Search
            SolverTabuSearchMCVRPTW tabu_solver(sweep_solver);
            auto best_solution = improved_rutas;
            double best_cost = 0;
            for(const auto& r : best_solution) best_cost += r.costo_total;

            int I = 50; // Number of iterations for ITS
            for (int iteration = 1; iteration <= I; ++iteration) {
                auto rutas_perturbadas = tabu_solver.perturbation(best_solution);
                auto current_rutas = tabu_solver.tabu_search(rutas_perturbadas, iteration);

                double prob = pow((double)iteration / I, 2);
                if ((double)rand() / RAND_MAX < prob) {
                    current_rutas = rutas_perturbadas;
                }
                
                double current_cost = 0;
                for(const auto& r : current_rutas) current_cost += r.costo_total;
                std::cout << "  [Iteracion " << iteration << "] -> "  << "current_cost: " << current_cost << ", best_cost: " << best_cost << std::endl;
                if (current_cost < best_cost) {
                    best_solution = current_rutas;
                    best_cost = current_cost;
                    std::cout << "  [Iteracion " << iteration << "] Nueva mejor solucion: $" 
                              << std::fixed << std::setprecision(2) << best_cost << std::endl;
                }
            }
            
            visualizer.imprimir_solucion(best_solution, 2, "final_solution", I, prefix);

        }
    }

    return 0;
}
