#include <iostream>
#include <vector>
#include <string>
#include <map>
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
        }
    }

    return 0;
}
