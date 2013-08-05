using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ClassifierProgram
{
    /*
            Generates the computable vectors
     */
    class Generator
    {
        /// <summary>
        /// Each dataSet object maps the class to the List of nx1 matices of data
        /// </summary>
        private Dictionary<string, List<Matrix>> dataSet;


        private List<Matrix> featureSet;

        public Generator(List<string[]> rawDataSet)
        {
            dataSet = new Dictionary<string, List<Matrix>>();
            featureSet = new List<Matrix>();
            Generate(rawDataSet);
        }

        public void Generate(List<string[]> rawDataSet)
        {
            // skip headers
            for (int i = 0; i < rawDataSet.Count; i++)
            {
                var classID = rawDataSet[i][0].Trim();
                if (!dataSet.ContainsKey(classID))
                    dataSet[classID] = new List<Matrix>();
                Matrix convertedMatrix = ConvertLine(rawDataSet[i]);
                dataSet[classID].Add( convertedMatrix );
                featureSet.Add(convertedMatrix);
            }
            
        }

        /// <summary>
        /// Takes each line of data and turns it into an n x 1 matrix
        /// </summary>
        /// <param name="dataLine"></param>
        /// <returns></returns>
        private Matrix ConvertLine(string[] dataLine)
        {
            // Skip the class ID
            Matrix matrix = new Matrix(dataLine.Length-1, 1);
            
            for(int i = 0; i < dataLine.Length-1; i++)
            {
                double value = 0;
                if (Double.TryParse(dataLine[i + 1], out value))
                    matrix[i, 0] = value;
                else
                    matrix[i, 0] = 0;

                //matrix[i, 0] = Double.Parse(dataLine[i+1]);
            }
            return matrix;
        }

        public Dictionary<string, List<Matrix>> DataSet
        {
            get { return this.dataSet; } 
        }

        public List<Matrix> FeatureSet
        {
            get { return this.featureSet; } 
        }

        public static List<string[]> GenerateDataSets()
        {
            return null;
        }
    }

}
