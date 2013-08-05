using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Data;
using System.IO;
using System.Drawing;
using System.Drawing.Imaging;

namespace NeuralNet
{
        /// <summary>
        /// This class is used to convert data from various formats to other various formats
        /// </summary>
        public class DataConverter
        {

            /// <summary>
            /// Takes a list of file names and converts that into a List of input Vectors
            /// </summary>
            /// <param name="imageFiles">List of file names to read</param>
            /// <returns></returns>
            public static List<InputPair> ConvertMapToInputPairs(Bitmap imageMap, string letter, bool binary, int rescaleFactor)
            {
                List<InputPair> inputMatrices = new List<InputPair>();
                // Get the letter from the full file name
                imageMap = ResizeImage(imageMap, rescaleFactor, rescaleFactor);
                imageMap = FilterBitmap(imageMap);
                Matrix imageMatrix = ConvertImageToMatrix(imageMap, binary);
                Matrix targetMatrix = ConvertLetterToTargetMatrix(letter);
                inputMatrices.Add(new InputPair(letter, imageMatrix, targetMatrix));
                return inputMatrices;
            }

            /// <summary>
            /// Converts a list of bitmap images into a data file to be stored on the system
            /// </summary>
            /// <param name="imageFiles"></param>
            public static void ConvertImagesToDataFile(List<string> imageFiles, string dataFile, bool binary, int rescaleFactor)
            {
                List<string[]> dataLines = new List<string[]>();
                bool headersAdded = false;
                foreach (var imageFile in imageFiles)
                {
                    // Get the letter from the full file name
                    string fileName = Path.GetFileName(imageFile);
                    string letter = fileName[0].ToString();
                    Matrix targetMatrix = ConvertLetterToTargetMatrix(letter);
                    Bitmap imageMap = (Bitmap)Image.FromFile(imageFile);
                    // Scale the image to the desired dimension
                    imageMap = DataConverter.ResizeImage(imageMap, rescaleFactor, rescaleFactor);
                    imageMap = DataConverter.FilterBitmap(imageMap);
                    Matrix imageMatrix = ConvertImageToMatrix(imageMap, binary);

                    // Now convert the matrices into string representations
                    List<string> currentLine = new List<string>();
                    // Add the headers
                    if (!headersAdded)
                    {
                        headersAdded = true;
                        List<string> headers = new List<string>();
                        headers.Add("ID");
                        for (int i = 0; i < imageMatrix.RowCount; i++)
                        {
                            headers.Add("x-" + i.ToString());
                        }
                        for (int i = 0; i < targetMatrix.RowCount; i++)
                        {
                            headers.Add("t-" + i.ToString());
                        }
                        dataLines.Add(headers.ToArray());
                    }
                    // Add the current image
                    currentLine.Add(letter);
                    for (int i = 0; i < imageMatrix.RowCount; i++)
                    {
                        currentLine.Add(imageMatrix[i,0].ToString());
                    }
                    for (int i = 0; i < targetMatrix.RowCount; i++)
                    {
                        currentLine.Add(targetMatrix[i,0].ToString());
                    }
                    dataLines.Add(currentLine.ToArray());

                    // print the lines to a file
                    using (StreamWriter outputStream = new StreamWriter(dataFile))
                    {
                        foreach (string[] line in dataLines)
                        {
                            outputStream.WriteLine(String.Join(",", line));
                        }
                    }
                }
            }

            /// <summary>
            /// Resize the image to the specified width and height
            /// </summary>
            /// <param name="image">The image to resize</param>
            /// <param name="width">The width to resize to</param>
            /// <param name="height">The height to resize to</param>
            /// <returns>The resized image</returns>
            public static Bitmap ResizeImage(System.Drawing.Image image, int width, int height)
            {
                Bitmap result = new Bitmap(width, height);
                result.SetResolution(image.HorizontalResolution, image.VerticalResolution);
                using (Graphics graphics = Graphics.FromImage(result))
                {
                    graphics.CompositingQuality = System.Drawing.Drawing2D.CompositingQuality.HighQuality;
                    graphics.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.HighQualityBicubic;
                    graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;
                    graphics.DrawImage(image, 0, 0, result.Width, result.Height);
                }
                return result;
            }

            /// <summary>
            /// Turns the image into a completely black and white image based on
            /// a in function defined threshold
            /// </summary>
            /// <param name="image"></param>
            /// <returns></returns>
            public static Bitmap FilterBitmap(Bitmap image)
            {
                int max = 0;
                int min = 800;
                // First get the max and min points
                for (int i = 0; i < image.Width; i++)
                {
                    for (int j = 0; j < image.Height; j++)
                    {
                        var value = image.GetPixel(i, j).R + image.GetPixel(i, j).B + image.GetPixel(i, j).G;
                        if (value > max)
                            max = value;
                        if (value < min)
                            min = value;
                    }
                }
                int midpoint = (int)(max + min / 2);
                Bitmap result = new Bitmap(image.Width, image.Height);
                for (int i = 0; i < image.Width; i++)
                {
                    for (int j = 0; j < image.Height; j++)
                    {
                        var value = image.GetPixel(i, j).R + image.GetPixel(i, j).B + image.GetPixel(i, j).G;
                        if (value < 700)
                            result.SetPixel(i, j, Color.Black);
                        else
                            result.SetPixel(i, j, Color.White);
                    }
                }
                return result;
            }

            /// <summary>
            /// Converts a bitmap image into a nx1 matrix where each
            /// value is 1 if there is color in the image, 0 if there isn't
            /// </summary>
            /// <param name="image"></param>
            /// <returns></returns>
            public static Matrix ConvertImageToMatrix(Bitmap image, bool binary)
            {
                Matrix inputMatrix = new Matrix(image.Width * image.Height, 1);
                int index = 0;
                for (int i = 0; i < image.Width; i++)
                {
                    for (int j = 0; j < image.Height; j++)
                    {
                        int binFactor = 0;
                        if (binary)
                            binFactor = -1;
                        inputMatrix[index, 0] = image.GetPixel(i, j).R + image.GetPixel(i, j).B +
                        image.GetPixel(i, j).G > 700 ? binFactor : 1;
                        index++;
                    }
                }
                return inputMatrix;
            }

            /// <summary>
            /// Converts a bitmap image into a nx1 matrix where each
            /// value is 1 if there is color in the image, 0 if there isn't
            /// </summary>
            /// <param name="image"></param>
            /// <returns></returns>
            public static Bitmap ConvertMatrixToImage(Matrix matrix, bool binary)
            {
                int imageSize = (int)Math.Sqrt(matrix.RowCount);
                Bitmap imageMap = new Bitmap(imageSize, imageSize);
                int index = 0;
                for (int i = 0; i < imageMap.Width; i++)
                {
                    for (int j = 0; j < imageMap.Height; j++)
                    {                        
                        if (matrix[index, 0] > 0)
                            imageMap.SetPixel(i, j, Color.Black);
                        else
                            imageMap.SetPixel(i, j, Color.White);
                        index++;
                    }
                }
                //imageMap = ResizeImage(imageMap, 200, 200);
                //imageMap = FilterBitmap(imageMap);
                return imageMap;
            }

            /// <summary>
            /// Converts a letter, A-G, into a 7x1 vector, where all the elements are 0
            /// except for 1 per letter
            /// </summary>
            /// <param name="letter">Letter to convert</param>
            /// <returns>7x1 Matrix</returns>
            public static Matrix ConvertLetterToTargetMatrix(string letter)
            {
                if (String.Equals(letter, "A"))
                {
                    return new Matrix( new double[,] { {1}, {0}, {0}, {0}, {0}, {0}, {0} } );
                }
                else if (String.Equals(letter, "B"))
                {
                    return new Matrix(new double[,] { { 0 }, { 1 }, { 0 }, { 0 }, { 0 }, { 0 }, { 0 } });
                }
                else if (String.Equals(letter, "C"))
                {
                    return new Matrix(new double[,] { { 0 }, { 0 }, { 1 }, { 0 }, { 0 }, { 0 }, { 0 } });
                }
                else if (String.Equals(letter, "D"))
                {
                    return new Matrix(new double[,] { { 0 }, { 0 }, { 0 }, { 1 }, { 0 }, { 0 }, { 0 } });
                }
                else if (String.Equals(letter, "E"))
                {
                    return new Matrix(new double[,] { { 0 }, { 0 }, { 0 }, { 0 }, { 1 }, { 0 }, { 0 } });
                }
                else if (String.Equals(letter, "F"))
                {
                    return new Matrix(new double[,] { { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 1 }, { 0 } });
                }
                else if (String.Equals(letter, "G"))
                {
                    return new Matrix(new double[,] { { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 0 }, { 1 } });
                }
                return new Matrix(1, 1);
            }

            /// <summary>
            /// Convert a data file into a list of classified input and target vectors
            /// </summary>
            /// <param name="dataFile"></param>
            /// <returns></returns>
            public static List<InputPair> ConvertDataFileToInputPairs(string dataFile)
       {
           List<InputPair> pairs = new List<InputPair>();
           CSVReader reader = new CSVReader(dataFile);
           var fileData = reader.raw_data;
           // The data file format is, classID, x1,x2..,xn, t1,t2,...,tn
           // We get the size of the vectors from the headers
           int inputVectorSize = DataConverter.GetInputVectorSize(reader.headers);
           int targetVectorSize = DataConverter.GetTargetVectorSize(reader.headers);
           foreach(var line in fileData)
           {
               string ID = line[0];
               Matrix inputMatrix = new Matrix(inputVectorSize, 1);
               for(int i = 0; i < inputVectorSize; i++)
               {
                   int inputColumn = i+1;
                   inputMatrix[i, 0] = Double.Parse(line[inputColumn]);
               }
               
               Matrix targetMatrix = new Matrix(targetVectorSize, 1);
               for(int j = 0; j < targetVectorSize; j++)
               {
                   int targetColumn = j + 1 + inputVectorSize;
                   targetMatrix[j, 0] = Double.Parse(line[targetColumn]);
               }
               pairs.Add(new InputPair(ID, inputMatrix, targetMatrix));
           }
           return pairs;
       }

            /// <summary>
            /// Determines the length of the input vector size from the headers
            /// of the data file
            /// </summary>
            /// <param name="line"></param>
            /// <returns></returns>
            private static int GetInputVectorSize(string[] line)
            {
                int size = 0;
                foreach (string header in line)
                {
                    if (header.ToLower().Contains("x-"))
                        size++;
                }
                return size;
            }

            /// <summary>
            /// Determines the length of the target vector size from the headers
            /// of the data file
            /// </summary>
            /// <param name="line"></param>
            /// <returns></returns>
            private static int GetTargetVectorSize(string[] line)
            {
                int size = 0;
                foreach (string header in line)
                {
                    if (header.ToLower().Contains("t-"))
                        size++;
                }
                return size;
            }

            #region DataTables

            /// <summary>
            /// This data should have a rows header, and a columns header, but
            /// in general it displays everything exactly as it is
            /// </summary>
            /// <param name="raw_data">Rows of the data that should be printed</param>
            /// <returns>DataTable representation</returns>
            public static DataTable ConvertConfusionMatrix(Matrix confusionMatrix)
            {
                List<string[]> rawData = new List<string[]>();
                Dictionary<string, int> tempMap = new Dictionary<string, int>();
                tempMap["A"] = 0; tempMap["B"] = 1; tempMap["C"] = 2; tempMap["D"] = 3; tempMap["E"] = 4; tempMap["F"] = 5;
                tempMap["G"] = 6;
                // Add headers
                List<string> headers = new List<string>();
                headers.Add(""); headers.Add("A");headers.Add("B");headers.Add("C");headers.Add("D");headers.Add("E");
                headers.Add("F");headers.Add("G");headers.Add("Total");
                rawData.Add(headers.ToArray());
                // Add formatted rows
                double percentTotal = 0;
                foreach(var letterNum in tempMap)
                {
                    List<string> row = new List<string>();
                    row.Add(letterNum.Key);
                    double total = 0;
                    for(int i = 0; i < confusionMatrix.ColumnCount; i++)
                    {
                        row.Add( confusionMatrix[ tempMap[letterNum.Key], i ].ToString("N3") );
                        total += confusionMatrix[ tempMap[letterNum.Key], i];
                    }
                    double totalRight = confusionMatrix[ tempMap[letterNum.Key], tempMap[letterNum.Key]];
                    double percentCorrect = totalRight/total;
                    percentTotal += percentCorrect;
                    row.Add( percentCorrect.ToString("0.000%") );
                    rawData.Add(row.ToArray());
                }
                List<string> lastLine = new List<string>();
                lastLine.Add("CCR");
                for (int i = 0; i < confusionMatrix.ColumnCount; i++)
                {
                    lastLine.Add("");
                }
                lastLine.Add( (percentTotal/confusionMatrix.RowCount).ToString("0.000%") );
                rawData.Add(lastLine.ToArray());
                DataTable table = new DataTable("ConfusionMatrix");                
                
                // Using this should mean the column names wont display
                for (int count = 0; count < rawData[0].Length; count++)
                {
                    table.Columns.Add(count.ToString());
                 }
                 foreach (string[] rowOfLines in rawData)
                {
                    table.Rows.Add(rowOfLines);
                }
                
                return table;
            }

            /// <summary>
            /// Prints a custom table for best displaying ART results
            /// </summary>
            /// <param name="confusionMatrix"></param>
            /// <returns></returns>
            public static DataTable ConvertARTMatrix(Matrix confusionMatrix)
            {
                List<string[]> rawData = new List<string[]>();
                Dictionary<string, int> tempMap = new Dictionary<string, int>();
                tempMap["A"] = 0; tempMap["B"] = 1; tempMap["C"] = 2; tempMap["D"] = 3; tempMap["E"] = 4; tempMap["F"] = 5;
                tempMap["G"] = 6;

                // Add headers
                List<string> headers = new List<string>();
                headers.Add(""); headers.Add("C1"); headers.Add("C2"); headers.Add("C3"); headers.Add("C4"); headers.Add("C5");
                headers.Add("C6"); headers.Add("C7"); headers.Add("Total");
                rawData.Add(headers.ToArray());
                // Add formatted rows
                double percentTotal = 0;
                foreach (var letterNum in tempMap)
                {
                    List<string> row = new List<string>();
                    row.Add(letterNum.Key);
                    double total = 0;
                    double largest = 0;
                    string winningCluster = "";
                    for (int i = 0; i < confusionMatrix.ColumnCount; i++)
                    {
                        double currentCount = confusionMatrix[tempMap[letterNum.Key], i];
                        if (currentCount > largest)
                        {
                            largest = currentCount;
                            winningCluster = headers[i + 1];
                        }
                        row.Add(currentCount.ToString("N3"));
                        total += currentCount;
                    }
                    double percentCorrect = largest / total;
                    percentTotal += percentCorrect;
                    //row.Add(percentCorrect.ToString("0.000%"));
                    row.Add(winningCluster + " : " + percentCorrect.ToString("0.000%"));
                    rawData.Add(row.ToArray());
                }
                List<string> lastLine = new List<string>();
                lastLine.Add("CCR");
                for (int i = 0; i < confusionMatrix.ColumnCount; i++)
                {
                    lastLine.Add("");
                }
                lastLine.Add((percentTotal / confusionMatrix.RowCount).ToString("0.000%"));
                //lastLine.Add("-");
                rawData.Add(lastLine.ToArray());
                DataTable table = new DataTable("ConfusionMatrix");

                // Using this should mean the column names wont display
                for (int count = 0; count < rawData[0].Length; count++)
                {
                    table.Columns.Add(count.ToString());
                }
                foreach (string[] rowOfLines in rawData)
                {
                    table.Rows.Add(rowOfLines);
                }

                return table;
            }

            #endregion DataTables
        }
    }

