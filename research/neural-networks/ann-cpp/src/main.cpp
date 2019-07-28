#include <iostream>
#include <vector>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include <fstream>
#include <sstream>

struct Connection
{
	double weight;
	double deltaWeight;
};

class Neuron;
typedef std::vector<Neuron> Layer;

/*********************** class Neuron **************************/

class Neuron
{
public:
	Neuron(unsigned numOutputs, unsigned myIndex);
	void   feedForward(const Layer& prevLayer);
	void   setOutputVal(double val);
	double getOutputVal(void) const;

	void   calcOutputGradients(double targetVal);
	void   calcHiddenGradients(const Layer& nextLayer);
	void   updateInputWeights(Layer& prevLayer);
	
private:
	static double eta;   // [0.0..1.0] overall net training
	                     // rate
	static double alpha; // [0.0..n] multiplier of the last
	                     // weight change (momentum)
	unsigned m_myIndex;
	double   m_outputVal;
	double   m_gradient;
	std::vector<Connection> m_outputWeights;
	static double randomWeight(void);
	static double transferFunction(double x);
	static double transferFunctionDerivative(double x);
	double sumDOW(const Layer& nextLayer) const;
};

double Neuron::eta   = 0.15; // overall net learning rate
double Neuron::alpha = 0.5;  // momentum, multiplier of last
                             // deltaWeight

Neuron::Neuron(unsigned numOutputs, unsigned myIndex)
{
	for(unsigned c = 0; c < numOutputs; c++) {
		m_outputWeights.push_back(Connection());
		m_outputWeights.back().weight = randomWeight();
	}
	m_myIndex = myIndex;
}

void Neuron::feedForward(const Layer& prevLayer)
{
	double sum = 0.0;

	// Sum the previous layer's outputs (which are our inputs)
	// Include the bias node from the previous layer.
	for(unsigned n = 0; n < prevLayer.size(); n++) {
		sum += prevLayer[n].m_outputVal *
			prevLayer[n].m_outputWeights[m_myIndex].weight;
	}

	m_outputVal = Neuron::transferFunction(sum);
}

void Neuron::setOutputVal(double val)
{
	m_outputVal = val;
}

double Neuron::getOutputVal(void) const
{
	return m_outputVal;
}


void Neuron::calcOutputGradients(double targetVal)
{
	double delta = targetVal - m_outputVal;
	m_gradient = delta
		* Neuron::transferFunctionDerivative(m_outputVal);
}

void Neuron::calcHiddenGradients(const Layer& nextLayer)
{
	double dow = sumDOW(nextLayer);
	m_gradient = dow
		* Neuron::transferFunctionDerivative(m_outputVal);
}

void Neuron::updateInputWeights(Layer& prevLayer)
{
	// The weights to be updated are in the Connection
	// container in the neurons in the preceding layer
	for(unsigned n = 0; n < prevLayer.size(); n++) {
		Neuron& neuron = prevLayer[n];
		double oldDeltaWeight =
			neuron.m_outputWeights[m_myIndex].deltaWeight;
		double newDeltaWeight =
			// Individual input, magnified by the gradient
			// and train rate;
			eta // = overall net learning rate.
			    // 0.0 - slow learner
			    // 0.2 - medium learner
			    // 1.0 - reckless learner
			* neuron.getOutputVal()
			* m_gradient
			// Also add momentum = a fraction of the previous
			// delta weight
			+ alpha // = momentum
			        // 0.0 - no momentum
			        // 0.5 - moderate momentum
			* oldDeltaWeight;

		neuron.m_outputWeights[m_myIndex].deltaWeight =
			newDeltaWeight;
		neuron.m_outputWeights[m_myIndex].weight +=
			newDeltaWeight;
	}
}



double Neuron::randomWeight(void) {
	return rand() / double(RAND_MAX);
}

double Neuron::transferFunction(double x)
{
	// tanh - output range [-1.0..1.0]
	return tanh(x);
	
	// Sigmoid - output range [0.0..1.0]
	//return x / (1.0 + abs(x));
}

double Neuron::transferFunctionDerivative(double x)
{
	// tanh derivative
	return 1.0 - (x * x);

	// Sigmoid derivative
	//return x * (1.0 - x);
}

double Neuron::sumDOW(const Layer& nextLayer) const
{
	double sum = 0.0;
	// Sum our contributions of the errors at the nodes we feed
	for(unsigned n = 0; n < nextLayer.size() - 1; n++) {
		sum += m_outputWeights[n].weight
			* nextLayer[n].m_gradient;
	}
	return sum;
}


/************************* class Net ***************************/

class Net
{
public:
	Net(const std::vector<unsigned>& topology);
	void feedForward(const std::vector<double>& inputVals);
	void backProp(const std::vector<double>& targetVals);
	void getResults(std::vector<double>& resultVals) const;

	double getRecentAverageError(void) const;

private:
	std::vector<Layer> m_layers;
	double             m_error;
	double             m_recentAverageError;
	double             m_recentAverageSmoothingFactor;
};

Net::Net(const std::vector<unsigned>& topology)
{
	unsigned numLayers = topology.size();
	for(unsigned layerNum = 0; layerNum < numLayers; layerNum++) {
		m_layers.push_back(Layer());
		unsigned numOutputs = (layerNum != topology.size() - 1)
			? topology[layerNum + 1]
			: 0;

		// Add neurons and bias neuron
		for(unsigned neuronNum = 0; 
		neuronNum <= topology[layerNum];
		neuronNum++) {
			m_layers.back().push_back(Neuron(numOutputs,
											 neuronNum));
			std::cout << "Made neuron #" << neuronNum
					  << " on layer #" << layerNum
					  << std::endl;
		}

		// Force the bias node's output value to 1.0.
		// It's the last neuron created above
		m_layers.back().back().setOutputVal(1.0);
	}
}

void Net::feedForward(const std::vector<double>& inputVals)
{
	assert(inputVals.size() == m_layers[0].size() - 1);

	// Assign (latch) the input values into the input neurons
	for(unsigned i = 0; i < inputVals.size(); i++) {
		m_layers[0][i].setOutputVal(inputVals[i]);
	}

	// Forward propagate
	for(unsigned layerNum = 1;
		layerNum < m_layers.size();
		layerNum++) {
		Layer& prevLayer = m_layers[layerNum - 1];
		for(unsigned n = 0;
			n < m_layers[layerNum].size() - 1;
			n++) {
			m_layers[layerNum][n].feedForward(prevLayer);
		}
	}
}

void Net::backProp(const std::vector<double>& targetVals)
{
	// Calculate overall net error (RMS of output neuron errors)
	// RMS = root mean square error
	Layer& outputLayer = m_layers.back();
	m_error = 0.0;

	for(unsigned n = 0; n < outputLayer.size() - 1; n++) {
		double delta = targetVals[n]
			- outputLayer[n].getOutputVal();
		m_error += delta * delta;
	}
	m_error /= outputLayer.size() - 1; // Get average error²
	m_error = sqrt(m_error); // RMS

	// Implement a recent average measurement:

	m_recentAverageError =
		(m_recentAverageError * m_recentAverageSmoothingFactor + m_error)
		/ (m_recentAverageSmoothingFactor + 1.0);
	
	// Calculate output layer gradients
	for(unsigned n = 0; n < outputLayer.size() - 1; n++) {
		outputLayer[n].calcOutputGradients(targetVals[n]);
	}

	// Calculate gradients on hidden layers
	for(unsigned layerNum = m_layers.size() - 2;
		layerNum > 0;
		layerNum--) {
		Layer &hiddenLayer = m_layers[layerNum];
		Layer &nextLayer   = m_layers[layerNum + 1];

		for(unsigned n = 0; n < hiddenLayer.size(); n++) {
			hiddenLayer[n].calcHiddenGradients(nextLayer);
		}
	}

	// For all layers from outputs to first hidden layer,
	// update connection weights
	for(unsigned layerNum = m_layers.size() - 1;
		layerNum > 0;
		layerNum--) {
		Layer& layer     = m_layers[layerNum];
		Layer& prevLayer = m_layers[layerNum - 1];

		for(unsigned n = 0; n < layer.size() - 1; n++) {
			layer[n].updateInputWeights(prevLayer);
		}
	}
}

void Net::getResults(std::vector<double>& resultVals) const
{
	resultVals.clear();
	for(unsigned n = 0; n < m_layers.back().size() - 1; n++)  {
		resultVals.push_back(m_layers.back()[n].getOutputVal());
	}
}

double Net::getRecentAverageError(void) const
{
	return m_recentAverageError;
}

/************************ Test class *************************/

typedef std::vector<double> TestData;

class NetTrainer
{
public:
	NetTrainer(const std::string inputFile);

	unsigned               getNumTests();
	std::vector<unsigned> getTopology();
	std::vector<TestData>& getInputs();
	std::vector<TestData>& getOutputs();
private:
	std::vector<unsigned>  m_topology;
	std::vector<TestData>  m_inputs;
	std::vector<TestData>  m_outputs;
	std::ifstream          m_file;
};

NetTrainer::NetTrainer(const std::string inputFile)
{
	m_topology.clear();
	m_inputs.clear();
	m_outputs.clear();

	m_file.open(inputFile.c_str());
	std::string line;
	std::string buffer;
	
	do {
		std::getline(m_file, line);
		std::stringstream ss(line);
	    if(line == "")
			break;
		
		while(!ss.eof()) {
			ss >> buffer;
			if(buffer == "topology:") {
				unsigned n;
				while(!ss.eof()) {
					ss >> n;
					m_topology.push_back(n);
				}
			}
			else if(buffer == "in:") {
				double n;
				m_inputs.push_back(TestData());
				while(!ss.eof()) {
					ss >> n;
					m_inputs.back().push_back(n);
				}
				assert(m_inputs.back().size() ==
					   m_topology[0]);
			} else if(buffer == "out:") {
				double n;
				m_outputs.push_back(TestData());
				while(!ss.eof()) {
					ss >> n;
					m_outputs.back().push_back(n);
				}
				assert(m_outputs.back().size() ==
					   m_topology.back());
			}
		}
	} while(!m_file.eof());

	m_file.close();

	assert(m_inputs.size() == m_outputs.size());
}

std::vector<unsigned> NetTrainer::getTopology()
{
	return m_topology;
}

std::vector<TestData>& NetTrainer::getInputs()
{
	return m_inputs;
}

std::vector<TestData>& NetTrainer::getOutputs()
{
	return m_outputs;
}

unsigned NetTrainer::getNumTests() {
	return m_inputs.size();
}

/*************************************************************/

int main(int argc, char** argv)
{
	auto parseVal = [](double val) -> std::string {
		if(val < 0.5) {
			if(val < 0.25) return "certainly false";
			else if(val > 0.25) return "maybe false";
			else return "probably false";
		} else if(val > 0.5) {
			if(val < 0.75) return "maybe true";
			else if(val > 0.75) return "certainly true";
			else return "probably false";
		}
		else return "uncertain";
	};
	
	std::cout << "Loading training data..\n";
	NetTrainer trainer("test.txt");
	std::cout << "Creating Neural Network...\n";
	Net myNetwork(trainer.getTopology());
	std::vector<double> results;
	std::cout << "Training the network..." << std::endl;
	for(unsigned i = 0; i < trainer.getNumTests(); i++) {
		std::cout << "Executing test #" << i << std::endl;
		std::cout << "Input          >> ";
		for(auto val : trainer.getInputs()[i])
			std::cout << val << " ";
		std::cout << std::endl
				  << "Target         >> ";
		for(auto val : trainer.getOutputs()[i])
			std::cout << val << " " << parseVal(val);
		std::cout << std::endl;
		myNetwork.feedForward(trainer.getInputs()[i]);
		myNetwork.getResults(results);
		myNetwork.backProp(trainer.getOutputs()[i]);
		
		std::cout << "Output         >> ";
	    for(auto val : results)
			std::cout << val << " " << parseVal(val);
	    std::cout << std::endl
				  << "Average Error  >> "
				  << myNetwork.getRecentAverageError();
		std::cout << std::endl << std::endl;
	}


	// Net usage tool
	std::cout << "==== Begin Neural Net test cases ===="
	          << std::endl;
	std::cout << "Input test case, or -1 to quit.\n\n";
	bool quit = false;
	do
	{
	    std::cout << "Input  << ";
	    std::vector<double> v;
	    v.reserve(trainer.getTopology()[0]);
	    for(unsigned i = 0; i < trainer.getTopology()[0]; i++) {
		double buffer;
		std::cin >> buffer;
		if(buffer != -1.0)
		    v.push_back(buffer);
		else {
		    quit = true;
		    break;
		}
	    }
	    if(!quit) {
		myNetwork.feedForward(v);
		myNetwork.getResults(results);
		
		std::cout << "Output >> ";
	    for(auto val : results)
			std::cout << val << " " << parseVal(val);
		std::cout << std::endl << std::endl;
	    }
	} while(!quit);
	
	return 0;
}
