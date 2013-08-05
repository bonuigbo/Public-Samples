using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace NeuralNet
{
    /// <summary>
    /// Any configuration for file names and network stats go here. I could
    /// have used the .NET configuration app, but decided since I would
    /// be using this once and locally, it wasn't necessary
    /// </summary>
    public struct Configuration
    {
        public static string rootDir = @"D:\neural_data";
        //public static string rootDir = "";
        public static string Images = Path.Combine(rootDir, "cluster_images");
        public static string log = Path.Combine(rootDir, "log.txt");
        public static string TrainingData = Path.Combine(rootDir, "training_data");
        public static string TestData = Path.Combine(rootDir, "test_data");
        public static string TrainingImages = Path.Combine(rootDir, "training_images");
        public static string TestImages = Path.Combine(rootDir, "test_images");
        public static string dataFileName = "data.csv";
      
    }
}
