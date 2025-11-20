#include "models.h"
#include <iostream>
#include <algorithm>
#include <cmath>

int time_to_minutes(const std::string& time_str) {
    int h = std::stoi(time_str.substr(0, 2));
    int m = std::stoi(time_str.substr(3, 2));
    return (h - 4) * 60 + m;
}

double polar_coordinate_angle(double x, double y, double depot_x, double depot_y) {
    double delta_x = x - depot_x;
    double delta_y = y - depot_y;
    double angle = atan2(delta_y, delta_x);
    return angle >= 0 ? angle : angle + 2 * M_PI;
}

double radius(double x, double y, double depot_x, double depot_y) {
    return std::sqrt(pow(x - depot_x, 2) + pow(y - depot_y, 2));
}

double clockwise_angle(double angle_ccw) {
    return fmod(-angle_ccw + 2 * M_PI, 2 * M_PI);
}

std::vector<Cliente> split_client_demands(const std::vector<Cliente>& original_clientes) {
    std::vector<Cliente> split_clientes;
    for (const auto& c : original_clientes) {
        bool has_gas = c.demanda_gasohol > 0;
        bool has_diesel = c.demanda_diesel > 0;

        if (has_gas) {
            Cliente gas_client = c;
            gas_client.demanda_diesel = 0;
            split_clientes.push_back(gas_client);
        }
        if (has_diesel) {
            Cliente diesel_client = c;
            diesel_client.demanda_gasohol = 0;
            split_clientes.push_back(diesel_client);
        }
    }
    
    // The python version sorts again after splitting
    std::sort(split_clientes.begin(), split_clientes.end(), [](const Cliente& a, const Cliente& b) {
        if (std::abs(a.angulo - b.angulo) > 1e-9) {
            return a.angulo < b.angulo;
        }
        return a.radio < b.radio;
    });

    return split_clientes;
}





ProblemInstance::ProblemInstance(const std::vector<std::vector<std::string>>& data,
                                 const std::map<int, std::map<std::string, double>>& tipos_cisternas,
                                 bool clockwise,
                                 bool split_demands,
                                 int num_vehiculos_por_tipo,
                                 double velocidad,
                                 double tiempo_descarga,
                                 double M)
    : num_vehiculos_por_tipo(num_vehiculos_por_tipo),
      velocidad(velocidad),
      tiempo_descarga(tiempo_descarga),
      M(M) {

    // Depot
    depot = {
        0,
        std::stod(data[0][1]),
        std::stod(data[0][2]),
        0, 0,
        time_to_minutes(data[0][5]),
        time_to_minutes(data[0][6]),
        0, 0
    };
    cliente_id_to_idx[0] = 0;

    std::vector<std::pair<double, double>> coords;
    coords.push_back({depot.x, depot.y});

    std::vector<Cliente> temp_clientes;
    for (size_t i = 1; i < data.size(); ++i) {
        const auto& row = data[i];
        Cliente c = {
            std::stoi(row[0]),
            std::stod(row[1]),
            std::stod(row[2]),
            std::stod(row[3]),
            std::stod(row[4]),
            time_to_minutes(row[5]),
            time_to_minutes(row[6]),
            0, 0
        };
        c.angulo = polar_coordinate_angle(c.x, c.y, depot.x, depot.y);
        c.radio = radius(c.x, c.y, depot.x, depot.y);
        temp_clientes.push_back(c);
    }

    // Apply clockwise transformation before sorting if needed
    if (clockwise) {
        for (auto& c : temp_clientes) {
            c.angulo = clockwise_angle(c.angulo);
        }
    }

    // Sort clients by angle and radius BEFORE splitting
    std::sort(temp_clientes.begin(), temp_clientes.end(), [](const Cliente& a, const Cliente& b) {
        if (std::abs(a.angulo - b.angulo) > 1e-9) {
            return a.angulo < b.angulo;
        }
        return a.radio < b.radio;
    });

    if (split_demands) {
        temp_clientes = split_client_demands(temp_clientes);
        // Re-assign IDs *after* splitting
        for(size_t i = 0; i < temp_clientes.size(); ++i) {
            temp_clientes[i].id = i + 1;
        }
    }

    for(const auto& c : temp_clientes) {
        clientes.push_back(c);
        coords.push_back({c.x, c.y});
        cliente_id_to_idx[c.id] = clientes.size();
    }

    this->n = clientes.size();

    // Distance matrix
    D.resize(coords.size(), std::vector<double>(coords.size()));
    for (size_t i = 0; i < coords.size(); ++i) {
        for (size_t j = 0; j < coords.size(); ++j) {
            D[i][j] = std::sqrt(pow(coords[i].first - coords[j].first, 2) + pow(coords[i].second - coords[j].second, 2));
        }
    }

    // Cisternas
    for (const auto& pair : tipos_cisternas) {
        cisternas_disponibles.push_back({
            pair.first,
            pair.second.at("cap_gasohol"),
            pair.second.at("cap_diesel"),
            pair.second.at("costo_fijo"),
            pair.second.at("costo_km")
        });
    }
}

double ProblemInstance::distancia(int i, int j) const {
    int idx_i = cliente_id_to_idx.at(i);
    int idx_j = cliente_id_to_idx.at(j);
    return D[idx_i][idx_j];
}

double ProblemInstance::tiempo_viaje(int i, int j) const {
    return (distancia(i, j) / velocidad) * 60.0;
}

const Cliente& ProblemInstance::cliente_por_id(int cliente_id) const {
    if (cliente_id == 0) {
        return depot;
    }
    auto it = std::find_if(clientes.begin(), clientes.end(), [cliente_id](const Cliente& c){
        return c.id == cliente_id;
    });
    return *it;
}
