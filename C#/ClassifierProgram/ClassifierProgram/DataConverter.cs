using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Data;
using System.Drawing;
using System.IO;


namespace ClassifierProgram.NeuralNet
{
    /*  
     *      For reading csv delimited files into memory
     * 
     */
    class CSVReader
    {
        #region Members

        private string fileNameAndPath;
        private List<string[]> rawData;
        private int totalLines;

        #endregion Members

        #region Constructors

        public CSVReader(string fileNameAndLocation)
        {
            this.fileNameAndPath = fileNameAndLocation;
            rawData = new List<string[]>();
        }

        #endregion  Constructors

        #region Methods

        /// <summary>
        /// Reads the data into memory
        /// </summary>
        public void Process()
        {
            using (FileStream file = new FileStream(this.fileNameAndPath, FileMode.Open, FileAccess.Read))
            {
                file.Seek(0, SeekOrigin.Begin);
                using (StreamReader fileReader = new StreamReader(this.fileNameAndPath))
                {
                    string rawLine;
                    int lineNumber = 0;
                    while ((rawLine = fileReader.ReadLine()) != null)
                    {
                        lineNumber++;
                        string[] parsedLine = ParseLine(rawLine);
                        rawData.Add(parsedLine);
                    }
                }
            }
        }
        /// <summary>
        /// Splits the line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private string[] ParseLine(string line)
        {
            if (line == null)
            {
                return null;
            }
            else if (line == "")
            {
                return new string[0];
            }

            List<string> result = new List<string>();
            StringBuilder token = new StringBuilder();
            bool inQuotes = false;
            for (int i = 0; i < line.Length; ++i)
            {
                char currentChar = line[i];
                if (inQuotes)
                {
                    if (currentChar == '"')
                        inQuotes = false;
                    token.Append(currentChar);
                }
                else
                {
                    if (currentChar == '"')
                    {
                        inQuotes = true;
                        token.Append(currentChar);
                    }

                    else if (currentChar == ',')
                    {
                        result.Add(token.ToString());
                        token = new StringBuilder();
                    }
                    else
                    {
                        token.Append(currentChar);
                    }
                }
            }
            result.Add(token.ToString().Trim());
            return result.ToArray();
        }


        #endregion Methods

        #region Properties


        public List<string[]> RawData
        {
            get
            {
                return rawData;
            }
        }

        #endregion Properties
    }

    /*
     *      Uses the records in the data file, as well
     *      as a data map, to generate the records
     *      into a form that can be used by the neural
     *      network processes 
     */
    class RecordsGenerator
    {

    }
    /*
     *      This class takes a sample data file and a data map,
     *      and generates a grid of doubles to be used for the
     *      neural network 
     */
    class DataConverter
    {
        private string inputFile;
        private string dataMapFile;

        public DataConverter(string inputFile, string dataMapFile)
        {
            this.inputFile = inputFile;
            this.dataMapFile = dataMapFile;
        }

        public void Process()
        {
            CSVReader dataReader = new CSVReader(inputFile);
            CSVReader mapReader = new CSVReader(dataMapFile);
            // Convert the raw data into string[]
            dataReader.Process();
            mapReader.Process();
            List<string[]> rawData = dataReader.RawData;
            List<string[]> dataMap = mapReader.RawData;
        }

        private void ConvertToNeuralMatrix(List<string[]> rawData, List<string[]> dataMap)
        {
            
        }
    }

}
