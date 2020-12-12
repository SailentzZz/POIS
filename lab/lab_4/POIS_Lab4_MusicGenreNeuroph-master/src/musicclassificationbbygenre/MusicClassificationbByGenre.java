package musicclassificationbbygenre;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import org.neuroph.core.NeuralNetwork;
import org.neuroph.core.learning.DataSet;
import org.neuroph.core.learning.DataSetRow;
import org.neuroph.nnet.MultiLayerPerceptron;
import org.neuroph.nnet.learning.MomentumBackpropagation;
import org.neuroph.util.TrainingSetImport;
import org.neuroph.util.TransferFunctionType;

public class MusicClassificationbByGenre {

    public static void main(String[] args) {

        String trainingSetFileName = "Music.txt";
        int inputsCount = 8;
        int outputsCount = 4;

        System.out.println("Running Sample");
        System.out.println("Using training set " + trainingSetFileName);

        // create training set
        DataSet trainingSet = null;
        try {
            trainingSet = TrainingSetImport.importFromFile(trainingSetFileName, inputsCount, outputsCount, ",");
        } catch (FileNotFoundException ex) {
            System.out.println("File not found!");
        } catch (IOException | NumberFormatException ex) {
            System.out.println("Error reading file or bad number format!");
        }


        // create multi layer perceptron
        System.out.println("Creating neural network");
        MultiLayerPerceptron neuralNet = new MultiLayerPerceptron(TransferFunctionType.SIGMOID, 8, 20, 4);

        // set learning parametars
        MomentumBackpropagation learningRule = (MomentumBackpropagation) neuralNet.getLearningRule();
        learningRule.setLearningRate(0.5);
        learningRule.setMomentum(0.8);

        // learn the training set
        System.out.println("Training neural network...");
        neuralNet.learn(trainingSet);
        System.out.println("Done!");

        // test perceptron
        System.out.println("Testing trained neural network");
        testMusicClassification(neuralNet, trainingSet);

    }

    public static void testMusicClassification(NeuralNetwork nnet, DataSet dset) {

        for (DataSetRow trainingElement : dset.getRows()) {

            nnet.setInput(trainingElement.getInput());
            nnet.calculate();
            double[] networkOutput = nnet.getOutput();
            System.out.print("Input: " + Arrays.toString(trainingElement.getInput()));
            System.out.println(" Output: " + Arrays.toString(networkOutput));
        }

    }
}
