# JuMP Verão

Este curso de introdução ao JuMP é baseado nos notebooks [do workshop SESO](https://github.com/odow/SESO2023).

Outras boas referências são os tutoriais _getting started_ de [julia](https://jump.dev/JuMP.jl/stable/tutorials/getting_started/getting_started_with_julia/) e [JuMP](https://jump.dev/JuMP.jl/stable/tutorials/getting_started/getting_started_with_JuMP/).

## Instalando Julia e os pacotes JuMP

- Baixe o Julia em [julialang.org](https://julialang.org/downloads/).
- O arquivo `Project.toml` deste repositório contém as dependências necessárias para o ambiente de Julia do curso.
    - Baixe para um diretório (por exemplo, fazendo o _checkout_ deste repositório);
    - use o comando `] activate .` (ou `using Pkg; Pkg.activate(".")`) para ativar o ambiente no diretório corrente;
    - use o comando `] instantiate` (ou `Pkg.instantiate()`) para baixar, instalar e pré-compilar as dependências.
