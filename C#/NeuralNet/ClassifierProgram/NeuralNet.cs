using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace NeuralNet
{
    public class NeuralException : Exception
    {
        public string error_message;
        public NeuralException(string errorMessage)
        {
            this.error_message = errorMessage;
        }
    }

    /// <summary>
    /// Static library for the various activation functions that will be used
    /// </summary>
    public class ActivationFunction
    {

        public static double Sigmoid(double input)
        {
            return (1 / (1 + Math.Exp(-input)));
        }

        public static double BackError(double target, double sum_value)
        {
            return target * ActivationFunction.SigmoidPrime(sum_value) ;
        }

        public static double SigmoidPrime(double input)
        {
            return ActivationFunction.Sigmoid(input) * (1 - ActivationFunction.Sigmoid(input) );
        }

        public static double StepFunction(double input)
        {
            return input > 0.0 ? 1.0 : 0.0;
        }
    }

    /// <summary>
    /// A struct for storing its relevant values
    /// </summary>
    public class Neuron
    {
        public int Index;
        public double Sum;
        public double Value;
        public double Error;
        public double ErrorSum;
        public double ErrorDelta;
        public double Bias;
        public double Target;
        public Neuron(int index)
        {
            Sum = Value = Error = ErrorSum = Bias = Target = 0;
            this.Index = index;
        }
    }

    /// <summary>
    /// Layer contains Neurons and a layer index
    /// </summary>
    public class Layer
    {
        private int index;
        private List<Neuron> neurons;

        public Layer(int index)
        {
            this.index = index;
            this.neurons = new List<Neuron>();
        }

        public Layer(int index, List<Neuron> neurons)
        {
            this.index = index;
            this.neurons = neurons;
        }

        #region Properties
        public int Index
        {
            get { return index; }
        }

        public List<Neuron> Neurons
        {
            get { return neurons; }
        }
        #endregion Properties
    }

    /// <summary>
    /// Struct to hold an input vector and its target vector
    /// </summary>
    public class InputPair
    {
        public string ClassifiedID;
        public string ID;
        public Matrix InputMatrix;
        public Matrix TargetMatrix;

        public InputPair(string id, Matrix input, Matrix target)
        {
            ID = id;
            InputMatrix = input;
            TargetMatrix = target;
        }
    }

    public abstract class Network
    {
        #region Members

        protected Matrix weightsMatrix;
        protected Matrix weightChangesMatrix;
        protected Matrix confusionMatrix;
        protected double learningRate;
        protected List<Neuron> neurons;
        protected List<Layer> layers;
        protected Logger logger;
        protected List<InputPair> inputMatrices;
        protected int totalEpochs;

        #endregion Members

        public Network(Logger logger)
        {
            neurons = new List<Neuron>();
            layers = new List<Layer>();
            this.logger = logger;
            totalEpochs = 0;
        }

        #region Abstracts

        /// <summary>
        /// Uses an input from data to set the number of layers and the
        /// number of neurons in each layer
        /// </summary>
        /// <param name="pair"></param>
        /// <param name="layerStruct"></param>
        public abstract void InitializeNetwork(List<InputPair> inputMatrices, int[] layerStruct, double learningRate);

        /// <summary>
        /// Trains the network using the training data as input, for
        /// a certain amount of Epochs
        /// </summary>
        /// <param name="epochCount">Number of iterations to train the network</param>
        public abstract void TrainNetwork(int epochCount);

        public abstract void TestNetwork(List<InputPair> inputVectors);

        #endregion Abstracts

        #region Methods

        /// <summary>
        /// Maps the current input pair to the input and output layer values
        /// </summary>
        /// <param name="pair"></param>
        protected void MapInputAndTarget(InputPair pair)
        {
            for (int i = 0; i < pair.InputMatrix.RowCount; i++)
            {
                layers[0].Neurons[i].Value = pair.InputMatrix[i, 0];
            }
            for (int i = 0; i < pair.TargetMatrix.RowCount; i++)
            {
                layers[layers.Count - 1].Neurons[i].Target = pair.TargetMatrix[i, 0];
            }
        }

        /// <summary>
        /// Initializes all the weights in a neuron to be a certain number
        /// </summary>
        /// <param name="weight">The weight to set</param>
        public void InitializeWeightsAndBiases(double weights,
                double biases,
                bool random)
        {

            if (random)
                RandomizeWeightsAndBiases(weights, biases);
            else
                SetWeightsAndBiases(weights, biases);
        }

        /// <summary>
        /// Sets the weights and biases to random numbers bound by
        /// the supplied thresholds
        /// </summary>
        /// <param name="weights"></param>
        /// <param name="biases"></param>
        protected void RandomizeWeightsAndBiases(double weights,
                double biases)
        {
            Random random = new Random();
            for (int i = 0; i < neurons.Count; i++)
            {
                for (int j = 0; j < neurons.Count; j++)
                {
                    double negFactor = random.NextDouble() >= 0.5 ? 1 : -1;
                    weightsMatrix[i, j] = weights * random.NextDouble() * negFactor;
                }
            }
            foreach (Neuron neuron in neurons)
            {
                double negFactor = random.NextDouble() >= 0.5 ? 1 : -1;
                neuron.Bias = biases * random.NextDouble() * negFactor;
            }
        }

        /// <summary>
        /// Sets the weights and biases to the supplied number
        /// </summary>
        /// <param name="weights"></param>
        /// <param name="biases"></param>
        protected void SetWeightsAndBiases(double weights,
                double biases)
        {
            for (int i = 0; i < neurons.Count; i++)
            {
                for (int j = 0; j < neurons.Count; j++)
                {
                    weightsMatrix[i, j] = weights;
                }
            }
            foreach (Neuron neuron in neurons)
            {
                neuron.Bias = biases;
            }
        }

        /// <summary>
        /// Compares the actual target vector with the expect target, and 
        /// generates a confusion Matrix representing the error
        /// </summary>
        protected void ClassifyInputVector(InputPair inputPair)
        {
            string outputLetter = GetOutputLetter();
            inputPair.ClassifiedID = outputLetter;
        }

        /// <summary>
        /// Determines a matrix representing the error for each class value
        /// in the input vector set
        /// </summary>
        /// <param name="inputMatrices"></param>
        protected void GenerateConfusionMatrix(List<InputPair> inputMatrices)
        {
            confusionMatrix = new Matrix(inputMatrices[0].TargetMatrix.RowCount, inputMatrices[0].TargetMatrix.RowCount);
            // Temporary dict to map letters to numbers
            Dictionary<string, int> tempMap = new Dictionary<string, int>();
            tempMap["A"] = 0; tempMap["B"] = 1; tempMap["C"] = 2; tempMap["D"] = 3; tempMap["E"] = 4; tempMap["F"] = 5;
            tempMap["G"] = 6;
            foreach(var inputPair in inputMatrices)
            {
                var row = tempMap[inputPair.ID];
                var column = tempMap[inputPair.ClassifiedID];
                confusionMatrix[row, column] += 1; 
            }
        }

        /// <summary>
        /// Gets a matrix of all the weights coming into the given neuron
        /// </summary>
        /// <param name="neuron"></param>
        /// <param name="inputLayer"></param>
        /// <returns></returns>
        protected Matrix GetInputWeightMatrix(Neuron neuron, Layer inputLayer)
        {
            Matrix weights = new Matrix(inputLayer.Neurons.Count, 1);
            for (int i = 0; i < inputLayer.Neurons.Count; i++)
            {
                // Get the weight from the currentMatrix to the going matrix
                weights[i, 0] = weightsMatrix[inputLayer.Neurons[i].Index, neuron.Index ];
            }
            return weights;
        }

        protected Matrix GetOutputWeightMatrix(Neuron neuron, Layer outputLayer)
        {
            Matrix weights = new Matrix(outputLayer.Neurons.Count, 1);
            for (int i = 0; i < outputLayer.Neurons.Count; i++)
            {
                // Get the weight from the currentMatrix to the going matrix
                weights[i, 0] = weightsMatrix[neuron.Index, outputLayer.Neurons[i].Index];
            }
            return weights;
        }

        /// <summary>
        /// Returns a n x1 matrix of all the values from the neurons in the previous layer
        /// If the current layer is the first layer, returns the input vector
        /// </summary>
        /// <param name="neuron"></param>
        /// <returns></returns>
        protected Matrix GetInputMatrix(Layer layer)
        {
            Matrix inputs = new Matrix(layer.Neurons.Count, 1);
            for (int i = 0; i < layer.Neurons.Count; i++)
            {
                inputs[i, 0] = layer.Neurons[i].Value;
            }
            return inputs;
        }

        /// <summary>
        /// Determines how different two matrices are, represented by
        /// a number between 0 and 1, 0 being no difference
        /// </summary>
        /// <param name="matrix1"></param>
        /// <param name="matrix2"></param>
        /// <returns></returns>
        protected double GetDistanceMatrix(Matrix matrix1, Matrix matrix2)
        {
            if(matrix1.RowCount != matrix2.RowCount || matrix1.ColumnCount != matrix2.ColumnCount)
            {
                string error = String.Format("{0}-{1} does not match {2}-{3}",
                    matrix1.RowCount, matrix1.ColumnCount, matrix2.RowCount, matrix2.ColumnCount);

                throw new NeuralException(error);
            }
            Matrix distanceMatrix = ((matrix1 - matrix2).Transpose) * (matrix1 - matrix2);
            double distance = (double)(Math.Sqrt((double)distanceMatrix[0, 0]));
            return distance;
            double difference = 0;
            for (int i = 0; i < matrix1.RowCount; i++)
            {
                for (int j = 0; j < matrix1.ColumnCount; j++)
                {
                    difference += Math.Abs(matrix1[i, j] - matrix2[i, j]);
                }
            }
            // Return the percent different
            return difference / (matrix1.RowCount * matrix1.ColumnCount);
        }


        /// <summary>
        /// Adds the values of the weights change matrix to the current weights matrix
        /// </summary>
        protected void UpdateWeights()
        {
            for (int i = 0; i < weightsMatrix.RowCount; i++)
            {
                for (int j = 0; j < weightsMatrix.ColumnCount; j++)
                {
                    weightsMatrix[i, j] = weightChangesMatrix[i, j] + weightsMatrix[i, j];
                }
            }
        }

        /// <summary>
        /// Gets the neuron with the largest sum in the selected layer
        /// </summary>
        /// <param name="layer"></param>
        /// <returns></returns>
        protected Neuron GetLargestSum(Layer layer)
        {
            Neuron reneuron = layer.Neurons[0];
            double sum = 0;
            foreach (var neuron in layer.Neurons)
            {
                if (neuron.Sum > sum)
                {
                    sum = neuron.Sum;
                    reneuron = neuron;
                }
            }
            return reneuron;
        }

        /// <summary>
        /// Gets the neuron with the larges value in the selected layer
        /// </summary>
        /// <param name="layer"></param>
        /// <returns></returns>
        protected Neuron GetLargestValue(Layer layer)
        {
            Neuron reneuron = layer.Neurons[0];
            double sum = 0;
            foreach (var neuron in layer.Neurons)
            {
                if (neuron.Value > sum)
                {
                    sum = neuron.Value;
                    reneuron = neuron;
                }
            }
            return reneuron;
        }

        /// <summary>
        /// Returns the closest representation of the output vector with a confidence value
        /// </summary>
        /// <returns></returns>
        public string GetOutputLetter()
        {
            Layer outputLayer = layers[layers.Count - 1];
            double[] output = new double[outputLayer.Neurons.Count];
            int maxIndex = 0;
            double max = 0;
            for (int i = 0; i < outputLayer.Neurons.Count; i++)
            {
                output[i] = outputLayer.Neurons[i].Value;
                if (output[i] > max)
                {
                    maxIndex = i;
                    max = output[i];
                }
            }
            string letter = "-";
            switch (maxIndex)
            {
                case 0:
                    letter = "A";
                    break;
                case 1:
                    letter = "B";
                    break;
                case 2:
                    letter = "C";
                    break;
                case 3:
                    letter = "D";
                    break;
                case 4:
                    letter = "E";
                    break;
                case 5:
                    letter = "F";
                    break;
                case 6:
                    letter = "G";
                    break;
            }
            return letter;
        }

        #region ToStringMethods

        public string GetSumString(Matrix input_matrix, Matrix weight_matrix, double bias, double value)
        {
            string sum_string = bias.ToString();
            for (int i = 0; i < input_matrix.RowCount; i++)
            {
                sum_string += " + " + input_matrix[i, 0].ToString("N6") + " * " + weight_matrix[i, 0];
            }
            sum_string += " = " + value.ToString("N6");
            return sum_string;
        }

        /// <summary>
        /// Prints a list of numbers, each representing a layer, the number representing 
        /// the number of neurons in that layer
        /// </summary>
        /// <returns>layer0Count1, layer1Count2, etc</returns>
        public string LayersString()
        {
            string lString = "";
            foreach (var layer in layers)
            {
                lString += layer.Neurons.Count.ToString() + ",";
            }
            return lString;
        }

        #endregion ToSTringMethods

        #endregion Methods

        #region Properties

        public Logger Logger
        {
            get { return logger; }
        }
        public Matrix ConfusionMatrix
        {
            get { return confusionMatrix; }
        }

        public int Epochs
        {
            get { return totalEpochs; }
        }


        #endregion Properties
    }

    /// <summary>
    /// Network that institutes back propagation
    /// </summary>
    public class BackPropagationNetwork : Network
    {
        public BackPropagationNetwork(Logger logger) : base(logger)  {}

        #region Overrides

        /// <summary>
        /// Uses an input from data to set the number of layers and the
        /// number of neurons in each layer
        /// </summary>
        /// <param name="pair"></param>
        /// <param name="layerStruct"></param>
        public override void InitializeNetwork(List<InputPair> inputMatrices, int[] layerStruct, double learningRate)
        {
            this.learningRate = learningRate;
            this.inputMatrices = inputMatrices;
            this.layers.Clear();
            int neuronCount = 0;
            var pair = inputMatrices[0];
            // Initialize the input layer
            Layer inputLayer = new Layer(layers.Count);
            //for (int i = layers.Count; i < pair.InputMatrix.RowCount; i++)
            for (int i = 0; i < pair.InputMatrix.RowCount; i++)
            {
                Neuron neuron = new Neuron(neuronCount);
                neuronCount++;
                neurons.Add(neuron);
                inputLayer.Neurons.Add(neuron);
            }
            this.layers.Add(inputLayer);

            // Initialize the hidden layers
            for (int j = 0; j < layerStruct.Length; j++)
            {
                Layer hiddenLayer = new Layer(layers.Count);
                for (int i = layers.Count; i < layerStruct[j]; i++)
                {
                    Neuron neuron = new Neuron(neuronCount);
                    neuronCount++;
                    neurons.Add(neuron);
                    hiddenLayer.Neurons.Add(neuron);
                }
                this.layers.Add(hiddenLayer);
            }

            // Initialize the target layer
            Layer outputLayer = new Layer(layers.Count);
            for (int i = 0; i < pair.TargetMatrix.RowCount; i++)
            {
                Neuron neuron = new Neuron(neuronCount);
                neuronCount++;
                neurons.Add(neuron);
                outputLayer.Neurons.Add(neuron);
            }
            this.layers.Add(outputLayer);

            // Initialize the weights
            weightsMatrix = new Matrix(new double[neurons.Count, neurons.Count]);
            weightChangesMatrix = new Matrix(new double[neurons.Count, neurons.Count]);
        }

        public override void TrainNetwork(int epochCount)
        {
            for (int i = 0; i < epochCount; i++)
            {
                totalEpochs += 1;
                logger.Log("\r\nEPOCH " + i.ToString() + "\r\n");
                foreach (var inputPair in inputMatrices)
                {
                    MapInputAndTarget(inputPair);
                    FeedForward();
                    ClassifyInputVector(inputPair);
                    BackPropogate();
                    UpdateWeights();
                }
            }
            GenerateConfusionMatrix(inputMatrices);
        }

        public override void TestNetwork(List<InputPair> inputVectors)
        {
            foreach (var inputPair in inputVectors)
            {
                MapInputAndTarget(inputPair);
                FeedForward();
                ClassifyInputVector(inputPair);
            }
            GenerateConfusionMatrix(inputVectors);
        }

        #endregion Overrides

        /// <summary>
        /// Feeds the values of the input matrix through the network
        /// </summary>
        /// <param name="inputMatrix">Values of the input</param>
        public void FeedForward()
        {
            logger.Log("\r\nFEED FORWARD\r\n");
            //Go through each layer, starting with the second since the first in the input layer
            for (int i = 1; i < layers.Count; i++)
            {
                Layer currentLayer = layers[i];
                Layer previousLayer = layers[i - 1];
                // Go through each neuron in the current layer
                foreach (Neuron neuron in currentLayer.Neurons)
                {
                    // Calculate values
                    Matrix inputs = GetInputMatrix(previousLayer);
                    Matrix weights = GetInputWeightMatrix(neuron, previousLayer);
                    Matrix sum_matrix = weights.Transpose * inputs;

                    double sum_value = sum_matrix[0, 0];
                    double sum = sum_value + neuron.Bias;
                    double value = ActivationFunction.Sigmoid(sum);
                    neuron.Sum = sum;
                    neuron.Value = value;

                    // Generate logging info
                    logger.Log(String.Format("Neuron: {0} - Layer: {1}",
                        neuron.Index.ToString(), currentLayer.Index.ToString()));
                    //logger.Log(GetSumString(inputs, weights, neuron.Bias, sum));
                    logger.Log("sum:" + neuron.Sum.ToString("N6"));
                    logger.Log("value:" + neuron.Value.ToString("N6"));      
                }
            }
        }

        /// <summary>
        /// Propogates the errors of the mapped target value back
        /// through the neural network
        /// </summary>
        public void BackPropogate()
        {
            logger.Log("\r\nBACK PROPOGATING\r\n");
            // Calculate the errors of all the hidden units
            for (int i = layers.Count - 1; i >= 1; i--)
            {
                Layer currentLayer = layers[i];
                Layer previousLayer = layers[i - 1];
                Layer aboveLayer = null;
                if( i < layers.Count -1)
                    aboveLayer = layers[i+1];

                foreach (Neuron neuron in currentLayer.Neurons)
                {
                    double errorSum = 0;
                    // If at last layer, use one equation, else calculate the sum matrix
                    if (i == layers.Count - 1)
                        errorSum = neuron.Target - neuron.Value;                        
                    else
                    {
                        // Sum of error value into above neurons times the weights to those neurons
                        Matrix errorMatrix = GetErrorMatrix(aboveLayer);
                        Matrix aboveWeights = GetAboveWeightMatrix(neuron, aboveLayer);
                        var errorSumMatrix = errorMatrix.Transpose * aboveWeights;                        
                        errorSum = errorSumMatrix[0, 0];
                    }
                    double error = errorSum * ActivationFunction.SigmoidPrime(neuron.Sum);
                    neuron.ErrorSum = errorSum;
                    neuron.Error = error;
                    double oldBias = neuron.Bias;
                    neuron.Bias = neuron.Bias + learningRate * neuron.Error;

                    // Logging
                    // Generate logging info
                    logger.Log(String.Format("Neuron: {0} - Layer: {1}",
                        neuron.Index.ToString(), 
                        currentLayer.Index.ToString()));
                    logger.Log("sum:" + neuron.Sum.ToString("N6"));
                    logger.Log("value:" + neuron.Value.ToString("N6"));
                    logger.Log("target:" + neuron.Target.ToString("N6"));
                    logger.Log("error sum:" + neuron.ErrorSum.ToString("N6"));
                    logger.Log("error:" + neuron.Error.ToString("N6"));
                    // Calculate the weight changes
                    logger.Log(String.Format("Weight change bias: {0} + {1} = {2}",
                    oldBias.ToString("N6"),
                    (0.9 * neuron.Error).ToString("N6"),
                    neuron.Bias.ToString("N6")));
 
                    // Calculate weight corrections for the weights coming into this neuron from
                    // the previous neurons
                   foreach (Neuron previousNeuron in previousLayer.Neurons)
                    {
                       double errorDelta = learningRate * neuron.Error * previousNeuron.Value;
                       weightChangesMatrix[previousNeuron.Index, neuron.Index] = errorDelta;
                       // Logging
                       /*
                        logger.Log(String.Format("WeightChange {0}-{1}: {2} + {3} = {4}",
                            previousNeuron.index.ToString("N6"),
                            neuron.index.ToString("N6"),
                            weightsMatrix[previousNeuron.index, neuron.index].ToString("N6"),
                           weightChangesMatrix[previousNeuron.index, neuron.index].ToString("N6"),
                            weightChangesMatrix[previousNeuron.index, neuron.index] + weightsMatrix[previousNeuron.index, neuron.index].ToString("N6")));
                       */
                    }
                }
            }
        }

        /// <summary>
        /// Gets the weights for all the connections coming out of this
        /// neuron into the above layer
        /// </summary>
        /// <param name="neuron">The neuronneuron</param>
        /// <returns>An nx1 matrix containing the weight values</returns>
        public Matrix GetAboveWeightMatrix(Neuron neuron, Layer aboveLayer)
        {
            Matrix weights = new Matrix(aboveLayer.Neurons.Count, 1);
            for (int i = 0; i < aboveLayer.Neurons.Count; i++)
            {
                // Get the weight from the currentMatrix to the going matrix
                weights[i, 0] = weightsMatrix[neuron.Index, aboveLayer.Neurons[i].Index];
            }
            return weights;
        }

        /// <summary>
        /// Returns a matrix representing the error values of
        /// all the neurons in the current layer
        /// </summary>
        /// <param name="layer">The layer to generate the matrix for</param>
        /// <returns>An nx1 matrix representing the values</returns>
        public Matrix GetErrorMatrix(Layer layer)
        {
            // Return input matrix if on the first layer
            Matrix inputs = new Matrix(layer.Neurons.Count, 1);
            for (int i = 0; i < layer.Neurons.Count; i++)
            {
                inputs[i, 0] = layer.Neurons[i].Error;
            }
            return inputs;
        }
    }

    /// <summary>
    /// Clusters the inputs based on perceived similarity
    /// </summary>
    public class ARTNetwork : Network
    {
        private double learnFactor;

        public ARTNetwork(Logger logger) : base(logger) { }

        /// <summary>
        /// Uses an input from data to set the number of layers and the
        /// number of neurons in each layer
        /// </summary>
        /// <param name="pair"></param>
        /// <param name="layerStruct"></param>
        public override void InitializeNetwork(List<InputPair> inputMatrices, int[] layerStruct, double learningRate)
        {
            this.learningRate = learningRate;
            this.inputMatrices = inputMatrices;
            this.layers.Clear();
            int neuronCount = 0;
            var pair = inputMatrices[0];
            // Initialize the input layer
            Layer inputLayer = new Layer(layers.Count);
            //for (int i = layers.Count; i < pair.InputMatrix.RowCount; i++)
            for (int i = 0; i < pair.InputMatrix.RowCount; i++)
            {
                Neuron neuron = new Neuron(neuronCount);
                neuronCount++;
                neurons.Add(neuron);
                inputLayer.Neurons.Add(neuron);
            }
            this.layers.Add(inputLayer);

            // Initialize the target layer
            Layer outputLayer = new Layer(layers.Count);
            for (int i = 0; i < pair.TargetMatrix.RowCount; i++)
            {
                Neuron neuron = new Neuron(neuronCount);
                neuronCount++;
                neurons.Add(neuron);
                outputLayer.Neurons.Add(neuron);
            }
            this.layers.Add(outputLayer);

            // SEt the learning factor
            learnFactor = layerStruct[0];
            // Initialize the weights
            weightsMatrix = new Matrix(new double[neurons.Count, neurons.Count]);
            weightChangesMatrix = new Matrix(new double[neurons.Count, neurons.Count]);
        }

        public override void TrainNetwork(int epochCount)
        {
            ResetWeights();
            for (int i = 0; i < epochCount; i++)
            {
                totalEpochs++;
                logger.Log("\r\nEPOCH " + i.ToString() + "\r\n");
                foreach (var inputPair in inputMatrices)
                {
                    MapInputAndTarget(inputPair);
                    FeedForward();
                    ClassifyInputVector(inputPair);
                    //UpdateWeights();
                }
            }
            GenerateConfusionMatrix(inputMatrices);
        }

        public override void TestNetwork(List<InputPair> inputVectors)
        {            
            foreach (var inputPair in inputVectors)
            {
                MapInputAndTarget(inputPair);
                FeedForward();
                ClassifyInputVector(inputPair);
            }
            GenerateConfusionMatrix(inputVectors);
        }

        /// <summary>
        /// Feeds the values of the input matrix through the network
        /// </summary>
        /// <param name="inputMatrix">Values of the input</param>
        public void FeedForward()
        {
            logger.Log("\r\nFEED FORWARD\r\n");
            // We compute the norm for each input vector, then determine
            // which output neuron has the greatest sum as the current candidate
            Layer inputLayer = layers[0];
            Layer outputLayer = layers[1];
            Matrix inputMatrix = GetInputMatrix(inputLayer);
            // Define the norm
            double norm = GetInputNorm(inputMatrix);
            // For each cluster unit, compute its net input, and its vigilance parameter
            logger.Log("Input Vector Norm: " + norm.ToString("N6"));
            foreach (Neuron neuron in outputLayer.Neurons)
            {
                Matrix weights = GetInputWeightMatrix(neuron, inputLayer);
                Matrix sumMatrix = weights.Transpose * inputMatrix;
                neuron.Sum = sumMatrix[0, 0];
                // BIas represents whether the neuron is inhibited or not
                neuron.Bias = 1;
                // Value represents whether the node is selected
                neuron.Value = 0;
                Matrix newInputMatrix = GetNewInputMatrix(neuron);
                double differencePercent = GetDistanceMatrix(inputMatrix, newInputMatrix);
                double newNorm = GetInputNorm(newInputMatrix);
                // I multiply by a distance factor to further limit selection
                double vigilance = (newNorm / norm) * (1-differencePercent);
                // Error sum will store vigilance
                neuron.ErrorSum = vigilance; 
                neuron.Error = newNorm;

                // Logging
                logger.Log(String.Format("Neuron: {0}",neuron.Index.ToString() ));
                //logger.Log(GetSumString(inputMatrix, weights, neuron.Bias, neuron.Sum));
                logger.Log("sum:" + neuron.Sum.ToString("N6"));
                logger.Log("newNorm: " + newNorm.ToString("N6"));
                logger.Log("norm: " + norm.ToString("N6"));
                logger.Log("vigilance:" + neuron.ErrorSum.ToString("N6"));
                logger.Log("difference: " + differencePercent.ToString("N6"));

            }
            Neuron selectedNeuron = GetLargestUninhibSum(outputLayer);
            double currentVigilance = learningRate;
            while (selectedNeuron != null)
            {
                if(selectedNeuron.ErrorSum > currentVigilance)
                {
                    logger.Log("Classified to Neuron " + selectedNeuron.Index);
                    selectedNeuron.Value = 1;
                    break;
                }
                else
                {
                    selectedNeuron.Bias = -2.0;
                    selectedNeuron = GetLargestUninhibSum(outputLayer);
                }
                if (AllNeuronsInhibited())
                {
                    currentVigilance -= 0.05;
                    logger.Log("Reducing vigilance to " + currentVigilance.ToString());
                    selectedNeuron = GetLargestUninhibSum(outputLayer);
                }
            }
                // Update bottom up and matrix values for this neuron
            for (int i = 0; i < inputLayer.Neurons.Count; i++)
            {
                var inputNeuron = inputLayer.Neurons[i];
                double updateTerm = learnFactor/ (learnFactor - 1.0 + selectedNeuron.Error);
                weightsMatrix[inputNeuron.Index, selectedNeuron.Index] = inputNeuron.Value * updateTerm;
                weightsMatrix[selectedNeuron.Index, inputNeuron.Index] = inputNeuron.Value;
                ///*
                logger.Log(String.Format("bw{0}-{1}= {2} * {3} = {4}   ---    td{5}-{6} = {7}",
                    inputNeuron.Index,
                    selectedNeuron.Index,
                    inputNeuron.Value.ToString("N3"),
                    updateTerm.ToString("N3"),
                    weightsMatrix[inputNeuron.Index, selectedNeuron.Index].ToString("N3"),
                    selectedNeuron.Index,
                    inputNeuron.Index,
                    inputNeuron.Value));
                //*/
            }            

        }

        /// <summary>
        /// Gets a n x1 matrix of the cluster which is a representation of
        /// the held letters
        /// </summary>
        /// <returns></returns>
        public List<Matrix> GetClusterWeights()
        {
            List<Matrix> clusters = new List<Matrix>();

            foreach (var neuron in layers[layers.Count - 1].Neurons)
            {
                Matrix topWeights = GetOutputWeightMatrix(neuron, layers[0]);
                clusters.Add(topWeights);
            }
            return clusters;
        }

        #region Private Neural Methods

        /// <summary>
        /// Gets the norm for the matrix, using whatever
        /// equation I deem necessary
        /// </summary>
        /// <param name="inputMatrix"></param>
        private double GetInputNorm(Matrix inputMatrix)
        {
                        double inputNorm = 0;
            for (int i = 0; i < inputMatrix.RowCount; i++)
            {
                inputNorm += inputMatrix[i, 0];
            }
            return inputNorm;
        }

        protected Neuron GetLargestUninhibSum(Layer layer)
        {
            Neuron reneuron = null;
            double sum = -1000;
            foreach (var neuron in layer.Neurons)
            {
                if (neuron.Sum > sum && neuron.Bias > 0)
                {
                    sum = neuron.Sum;
                    reneuron = neuron;
                }
            }
            return reneuron;
        }

        /// <summary>
        /// Computes the matrix of the input values mutiplied
        /// by the top down matrx values from the cluster unit
        /// </summary>
        /// <param name="?"></param>
        /// <returns></returns>
        private Matrix GetNewInputMatrix(Neuron currentCandidate)
        {
            int inputMatrixSize = layers[0].Neurons.Count;
            Matrix inputMatrix = new Matrix(inputMatrixSize, 1);
            for(int i = 0; i < inputMatrix.RowCount; i++)
            {
                var inputNeuron = layers[0].Neurons[i];
                
                double factor = weightsMatrix[currentCandidate.Index, inputNeuron.Index];
                inputMatrix[i, 0] = inputNeuron.Value * factor;
                //logger.Log(String.Format("New Input: {0} * {1} = {2} ", inputNeuron.Value, factor, inputMatrix[i,0]));
            }
            return inputMatrix;
        }

        /// <summary>
        /// Sets the weight to more appropriate values for a ART network
        /// </summary>
        private void ResetWeights()
        {
            foreach (Neuron clusterNeuron in layers[layers.Count - 1].Neurons)
            {
                foreach (Neuron inputNeuron in layers[0].Neurons)
                {
                    weightsMatrix[clusterNeuron.Index, inputNeuron.Index] = 1.0;
                    double bottomFactor = 1.0/(1.0 +  (double)layers[0].Neurons.Count);
                    weightsMatrix[inputNeuron.Index, clusterNeuron.Index] = bottomFactor;                    
                    weightChangesMatrix[clusterNeuron.Index, inputNeuron.Index] = 0;
                    weightChangesMatrix[inputNeuron.Index, clusterNeuron.Index] = 0;
                }
            }
        }

        /// <summary>
        /// Returns true if all the neurons are currently inhibited, and resets their status
        /// </summary>
        /// <returns></returns>
        private bool AllNeuronsInhibited()
        {
            bool allInhibited = true;
            foreach (var neuron in layers[1].Neurons)
            {
                if (neuron.Bias > 0)
                {
                    allInhibited = false;
                    break;
                }
            }
            // Reset them if they are inhibited for a lower vigilance run
            if (allInhibited)
            {
                foreach (var neuron in layers[1].Neurons)
                {
                    neuron.Bias = 1;
                }
            }
            return allInhibited;
        }

        #endregion Private Neural Methods
    }

    /// <summary>
    /// A simple network that updates the weights for training
    /// </summary>
    public class HRMNetwork : Network
    {
        public HRMNetwork(Logger logger)
            : base(logger)       {

        }
        #region Overrides

        /// <summary>
        /// Uses an input from data to set the number of layers and the
        /// number of neurons in each layer
        /// </summary>
        /// <param name="pair"></param>
        /// <param name="layerStruct"></param>
        public override void InitializeNetwork(List<InputPair> inputMatrices, int[] layerStruct, double learningRate)
        {
            this.learningRate = learningRate;
            this.inputMatrices = inputMatrices;
            this.layers.Clear();
            int neuronCount = 0;
            var pair = inputMatrices[0];
            // Initialize the input layer
            Layer inputLayer = new Layer(layers.Count);
            //for (int i = layers.Count; i < pair.InputMatrix.RowCount; i++)
            for (int i = 0; i < pair.InputMatrix.RowCount; i++)
            {
                Neuron neuron = new Neuron(neuronCount);
                neuronCount++;
                neurons.Add(neuron);
                inputLayer.Neurons.Add(neuron);
            }
            this.layers.Add(inputLayer);

            // Initialize the target layer
            Layer outputLayer = new Layer(layers.Count);
            for (int i = 0; i < pair.TargetMatrix.RowCount; i++)
            {
                Neuron neuron = new Neuron(neuronCount);
                neuronCount++;
                neurons.Add(neuron);
                outputLayer.Neurons.Add(neuron);
            }
            this.layers.Add(outputLayer);

            // Initialize the weights
            weightsMatrix = new Matrix(new double[neurons.Count, neurons.Count]);
            weightChangesMatrix = new Matrix(new double[neurons.Count, neurons.Count]);
        }

        public override void TrainNetwork(int epochCount)
        {
            for (int i = 0; i < epochCount; i++)
            {
                totalEpochs++;
                logger.Log("\r\nEPOCH " + i.ToString() + "\r\n");
                foreach (var inputPair in inputMatrices)
                {
                    MapInputAndTarget(inputPair);
                    TrainWeights();
                    FeedForward();
                    ClassifyInputVector(inputPair);                 
                }
            }
            GenerateConfusionMatrix(inputMatrices);
        }

        public override void TestNetwork(List<InputPair> inputVectors)
        {
            foreach (var inputPair in inputVectors)
            {
                MapInputAndTarget(inputPair);
                FeedForward();
                ClassifyInputVector(inputPair);
            }
            GenerateConfusionMatrix(inputVectors);
        }

        #endregion Overrides

        /// <summary>
        /// Feeds the values of the input matrix through the network
        /// </summary>
        /// <param name="inputMatrix">Values of the input</param>
        public void FeedForward()
        {
            logger.Log("\r\nFEED FORWARD\r\n");
            //Go through each layer, starting with the second since the first in the input layer
            Layer inputLayer = layers[0];
            Layer outputLayer = layers[1];
            // Go through each neuron in the current layer
           foreach (Neuron neuron in outputLayer.Neurons)
           {
                // Calculate values
                Matrix inputs = GetInputMatrix(inputLayer);
                Matrix weights = GetInputWeightMatrix(neuron, inputLayer);
                Matrix sum_matrix = weights.Transpose * inputs;
                double sumValue = sum_matrix[0, 0];
                double value = sumValue;// ActivationFunction.Sigmoid(sumValue);
                neuron.Sum = sumValue;
                neuron.Value = value;

                logger.Log(String.Format("Neuron: "  +neuron.Index.ToString() ));
                logger.Log("sum:" + neuron.Sum.ToString("N6"));
                logger.Log("value:" + neuron.Value.ToString("N6"));
            }
        }

        /// <summary>
        /// Iterates through and updates the values of the weights
        /// </summary>
        private void TrainWeights()
        {
            Layer inputLayer = layers[0];
            Layer outputLayer = layers[1];
            foreach (var inputNeuron in inputLayer.Neurons)
            {
                foreach (var outputNeuron in outputLayer.Neurons)
                {
                    double old = weightsMatrix[inputNeuron.Index, outputNeuron.Index];
                    weightsMatrix[inputNeuron.Index, outputNeuron.Index] =
                        old + inputNeuron.Value * outputNeuron.Target;
                    logger.Log(String.Format( "w{0}-{1} = {2} + {3}*{4} = {5}",
                        inputNeuron.Index,
                        outputNeuron.Index,
                        old,
                        inputNeuron.Value,
                        outputNeuron.Target,
                        weightsMatrix[inputNeuron.Index, outputNeuron.Index]));
                }
            }
        }
    }


}
