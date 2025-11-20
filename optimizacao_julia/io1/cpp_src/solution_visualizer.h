#ifndef SOLUTION_VISUALIZER_H
#define SOLUTION_VISUALIZER_H

#include "models.h"
#include <string>
#include <vector>

class SolutionVisualizer {
public:
    SolutionVisualizer(const ProblemInstance& instance);
    void imprimir_solucion(const std::vector<Ruta>& rutas, int verbosity = 1, const std::string& phase_name = "", int iterations = -1, const std::string& prefix = "");

private:
    const ProblemInstance& instance;
};

#endif // SOLUTION_VISUALIZER_H
