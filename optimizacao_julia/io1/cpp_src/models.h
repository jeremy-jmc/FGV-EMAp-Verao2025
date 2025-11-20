#ifndef MODELS_H
#define MODELS_H

#include <vector>
#include <string>
#include <map>
#include <cmath>
#include <numeric>

struct Cliente {
    int id;
    double x;
    double y;
    double demanda_gasohol;
    double demanda_diesel;
    int ventana_inicio;
    int ventana_fin;
    double angulo;
    double radio;
};

struct Cisterna {
    int tipo;
    double cap_gasohol;
    double cap_diesel;
    double costo_fijo;
    double costo_km;
};

struct Ruta {
    Cisterna cisterna;
    std::vector<int> clientes;
    double carga_gasohol;
    double carga_diesel;
    double distancia_total;
    double tiempo_total;
    double costo_total;
    bool factible;
    std::map<int, std::vector<std::string>> productos_entregados;
    std::vector<double> tiempos_llegada;
};

class ProblemInstance {
public:
    ProblemInstance(const std::vector<std::vector<std::string>>& data,
                    const std::map<int, std::map<std::string, double>>& tipos_cisternas,
                    bool clockwise,
                    bool split_demands,
                    int num_vehiculos_por_tipo = 20,
                    double velocidad = 60.0,
                    double tiempo_descarga = 5.0,
                    double M = 10000.0);

    double distancia(int i, int j) const;
    double tiempo_viaje(int i, int j) const;
    const Cliente& cliente_por_id(int cliente_id) const;

    Cliente depot;
    std::vector<Cliente> clientes;
    std::vector<Cisterna> cisternas_disponibles;
    int n;
    int num_vehiculos_por_tipo;
    double velocidad;
    double tiempo_descarga;
    double M;

private:
    std::vector<std::vector<double>> D;
    std::map<int, int> cliente_id_to_idx;
};

std::vector<std::vector<std::string>> split_client_demands(const std::vector<std::vector<std::string>>& data);

#endif // MODELS_H
