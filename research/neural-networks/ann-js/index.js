// Simple neural network which solves for XOR
// Thanks for Per Harald Borgen for the awesome video  at
// https://scrimba.com/casts/cast-1980.

/* Building the network */

// Uses the synaptic library
const { Layer, Network } = window.synaptic;

// Create layers
var inputLayer  = new Layer(2); // New layer with 2 neurons.
var hiddenLayer = new Layer(3); // New layer with 3 neurons.
var outputLayer = new Layer(1); // New layer with 1 neuron.

inputLayer.project(hiddenLayer);  // input tosses data at hidden
hiddenLayer.project(outputLayer); // hidden tosses data at output

// Create the actual network
var myNetwork = new Network({
    input:  inputLayer,    // Pass input layer
    hidden: [hiddenLayer], // Pass hidden layers
    output: outputLayer    // Pass output layer
});



/* Training the network */

var learningRate = 0.3; // Sets a learning rate; experiment with this
// This rate dictates how big of a step the network should take in the
// correct direction when backpropagating.
// If it is too big, it might go wild...
// If it is too small, it'll take too long to learn or won't even
// learn at all.

// Now for the actual training.
// We train our network 20000 times to learn about the XOR operation.
// Experiment with this number.
var numTrainings = 20000;

function XorTraining() {
    for(var i = 0; i < numTrainings; i++)
    {
        // [0, 0] => 0
        // If both are inactive, then right answer is false
        myNetwork.activate([0, 0]); // Input data to both neurons
        myNetwork.propagate(learningRate, [0]); // Backpropagate right ans

        // [0, 1] => 1
        // If second is active, then right answer is true
        myNetwork.activate([0, 1]);
        myNetwork.propagate(learningRate, [1]);

        // [1, 0] => 1
        // If first is active, then right answer is true
        myNetwork.activate([1, 0]);
        myNetwork.propagate(learningRate, [1]);

        // [1, 1] => 0
        // If both are active, then right answer is false
        myNetwork.activate([1, 1]);
        myNetwork.propagate(learningRate, [0]);
    }
}

// Extra: I added a training for AND & OR operations.
function AndTraining() {
    for(var i = 0; i < numTrainings; i++)
    {
        // [0, 0] => 0
        myNetwork.activate([0, 0]);
        myNetwork.propagate(learningRate, [0]);
        // [1, 0] => 0
        myNetwork.activate([1, 0]);
        myNetwork.propagate(learningRate, [0]);
        // [0, 1] => 0
        myNetwork.activate([0, 1]);
        myNetwork.propagate(learningRate, [0]);
        // [1, 1] => 1
        myNetwork.activate([1, 1]);
        myNetwork.propagate(learningRate, [1]);
    }
}

function OrTraining() {
    for(var i = 0; i < numTrainings; i++)
    {
        // [0, 0] => 0
        myNetwork.activate([0, 0]);
        myNetwork.propagate(learningRate, [0]);
        // [1, 0] => 1
        myNetwork.activate([1, 0]);
        myNetwork.propagate(learningRate, [1]);
        // [0, 1] => 1
        myNetwork.activate([0, 1]);
        myNetwork.propagate(learningRate, [1]);
        // [1, 1] => 1
        myNetwork.activate([1, 1]);
        myNetwork.propagate(learningRate, [1]);
    }
}


// Train the network
//XorTraining();
//AndTraining();
OrTraining();


// This is a quick function I wrote myself to output
// the results...
function logTestRes(testIntro, testResult) {
    console.log(
        (testResult < 0.5
         ? (testIntro + " => Probably false; ans = " + testResult)
         : ((testResult > 0.5)
            ? (testIntro + " => Probably true; ans = " + testResult)
            : (testIntro + " => Undefined; ans = " + testResult))));
}


// Now to test the network
logTestRes("[0, 0]", myNetwork.activate([0, 0]));
logTestRes("[0, 1]", myNetwork.activate([0, 1]));
logTestRes("[1, 0]", myNetwork.activate([1, 0]));
logTestRes("[1, 1]", myNetwork.activate([1, 1]));
