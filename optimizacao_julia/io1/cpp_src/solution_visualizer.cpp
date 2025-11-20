#include "solution_visualizer.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <numeric>

SolutionVisualizer::SolutionVisualizer(const ProblemInstance& instance) : instance(instance) {}

void SolutionVisualizer::imprimir_solucion(const std::vector<Ruta>& rutas, int verbosity, const std::string& phase_name, int iterations, const std::string& prefix) {
    std::stringstream output;

    output << "\n" << std::string(80, '=') << "\n";
    output << "SOLUCIÓN\n";
    output << std::string(80, '=') << "\n";

    double costo_total = 0;
    double distancia_total = 0;
    int vehiculos_tipo_1 = 0;
    int vehiculos_tipo_2 = 0;

    for (const auto& r : rutas) {
        costo_total += r.costo_total;
        distancia_total += r.distancia_total;
        if (r.cisterna.tipo == 1) {
            vehiculos_tipo_1++;
        } else {
            vehiculos_tipo_2++;
        }
    }

    if (verbosity >= 1) {
        output << "\n>>> RESUMEN GENERAL:\n";
        output << "  * Número de rutas: " << rutas.size() << "\n";
        output << "  * Costo total: $" << std::fixed << std::setprecision(2) << costo_total << "\n";
        output << "  * Distancia total: " << distancia_total << " km\n";

        std::string excede_t1 = (vehiculos_tipo_1 > instance.num_vehiculos_por_tipo) ? " !!! EXCEDE LÍMITE" : "";
        output << "  * Cisternas Tipo 1: " << vehiculos_tipo_1 << "/" << instance.num_vehiculos_por_tipo << excede_t1 << "\n";

        std::string excede_t2 = (vehiculos_tipo_2 > instance.num_vehiculos_por_tipo) ? " !!! EXCEDE LÍMITE" : "";
        output << "  * Cisternas Tipo 2: " << vehiculos_tipo_2 << "/" << instance.num_vehiculos_por_tipo << excede_t2 << "\n";
    }

    if (verbosity >= 2) {
        output << "\n>>> DETALLE DE RUTAS:\n";
        int idx = 1;
        for (const auto& ruta : rutas) {
            output << "\n  Ruta #" << idx++ << ":\n";
            output << "    Cisterna: Tipo " << ruta.cisterna.tipo << "\n";
            output << "    Secuencia: Depot -> ";
            for (size_t i = 0; i < ruta.clientes.size(); ++i) {
                output << ruta.clientes[i] << (i == ruta.clientes.size() - 1 ? "" : " -> ");
            }
            output << " -> Depot\n";
            output << "    Carga: Gasohol=" << std::fixed << std::setprecision(0) << ruta.carga_gasohol << "gal (" << std::setprecision(1) << (ruta.carga_gasohol / ruta.cisterna.cap_gasohol * 100) << "%), "
                   << "Diésel=" << std::fixed << std::setprecision(0) << ruta.carga_diesel << "gal (" << std::setprecision(1) << (ruta.carga_diesel / ruta.cisterna.cap_diesel * 100) << "%)\n";
            output << "    Distancia: " << std::fixed << std::setprecision(2) << ruta.distancia_total << " km\n";
            output << "    Tiempo: " << std::fixed << std::setprecision(1) << ruta.tiempo_total << " min\n";
            output << "    Costo: $" << std::fixed << std::setprecision(2) << ruta.costo_total << "\n";
            output << "    Tiempos de llegada (min desde 04:00):\n";
            for (size_t i = 0; i < ruta.clientes.size(); ++i) {
                int cliente_id = ruta.clientes[i];
                double tiempo_llegada = ruta.tiempos_llegada[i];
                int hora = 4 + static_cast<int>(tiempo_llegada) / 60;
                int minuto = static_cast<int>(tiempo_llegada) % 60;
                const auto& cliente = instance.cliente_por_id(cliente_id);
                std::stringstream ventana_ss;
                ventana_ss << "[" << std::setw(2) << std::setfill('0') << (4 + cliente.ventana_inicio / 60) << ":" << std::setw(2) << std::setfill('0') << (cliente.ventana_inicio % 60)
                           << " - " << std::setw(2) << std::setfill('0') << (4 + cliente.ventana_fin / 60) << ":" << std::setw(2) << std::setfill('0') << (cliente.ventana_fin % 60) << "]";
                output << "      - Cliente " << cliente_id << " (" << std::fixed << std::setprecision(1) << cliente.x << ", " << cliente.y << "): " << tiempo_llegada << " min ("
                       << std::setw(2) << std::setfill('0') << hora << ":" << std::setw(2) << std::setfill('0') << minuto << ") | Ventana: " << ventana_ss.str() << "\n";
            }
            output << "    Entregas por cliente:\n";
            for (const auto& pair : ruta.productos_entregados) {
                std::stringstream prods_ss;
                for (size_t i = 0; i < pair.second.size(); ++i) {
                    prods_ss << pair.second[i] << (i == pair.second.size() - 1 ? "" : ", ");
                }
                output << "      - Cliente " << pair.first << ": " << prods_ss.str() << "\n";
            }
        }
    }

    output << "\n" << std::string(80, '=') << "\n";

    std::cout << output.str();

    if (!phase_name.empty() && iterations != -1) {
        std::string results_dir = "../results";
        // The following is not standard C++11, but it is common in C++17
        // #include <filesystem>
        // if (!std::filesystem::exists(results_dir)) {
        //     std::filesystem::create_directory(results_dir);
        // }
        std::string filename = prefix + "_phase_" + phase_name + "_it" + std::to_string(iterations) + "_cpp.txt";
        std::ofstream file(results_dir + "/" + filename);
        if (file.is_open()) {
            file << output.str();
            file.close();
            std::cout << "[INFO] Solución guardada en: " << results_dir << "/" << filename << std::endl;
        } else {
            std::cerr << "Error al abrir el archivo para guardar la solución." << std::endl;
        }
    }
}
