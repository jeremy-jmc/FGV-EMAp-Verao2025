CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2
LDFLAGS =

SRCS = main.cpp models.cpp csv_reader.cpp route_evaluator.cpp solvers.cpp solution_visualizer.cpp
OBJS = $(SRCS:.cpp=.o)
TARGET = mcvro_run

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET)
