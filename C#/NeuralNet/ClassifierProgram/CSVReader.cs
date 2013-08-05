using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace NeuralNet
{
    public class CSVReader
    {
        #region Members

        public string fileNameAndPath;
        public List<string[]> raw_data;
        public List<Dictionary<string, string>> records;
        public string[] headers;
        public int totalLines;

        #endregion Members

        #region Constructors

        public CSVReader(string fileNameAndLocation)
        {
            this.fileNameAndPath = fileNameAndLocation;
            records = new List<Dictionary<string, string>>();
            headers = null;
            raw_data = new List<string[]>();

            GetTotalLines();
            ParseFile();
            Format();
        }

        #endregion  Constructors

        #region Methods

        private void ParseFile()
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
                        if (lineNumber == 1)
                        {
                            this.headers = parsedLine;
                            continue;
                        }
                        raw_data.Add(parsedLine);
                    }
                }
            }
        }

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

        private void GetTotalLines()
        {
            StreamReader lineReader = new StreamReader(fileNameAndPath);
            totalLines = 0;
            while (lineReader.ReadLine() != null)
            {
                totalLines++;
            }
            lineReader.Close();
        }

        private void Format()
        {
            foreach (string[] rawLine in raw_data)
            {
                Dictionary<string, string> currentRecord = new Dictionary<string, string>();
                for (int i = 0; i < headers.Length; i++)
                {
                    currentRecord[headers[i]] = rawLine[i];
                }
                records.Add(currentRecord);
            }
        }

        public void PrintCSV(string output, List<Dictionary<string, string>> records, string[] headers)
        {
            using (StreamWriter outputStream = new StreamWriter(output))
            {
                outputStream.Write(String.Join(",", headers));
                outputStream.WriteLine();
                foreach (Dictionary<string, string> record in records)
                {
                    List<string> currentRecord = new List<string>();
                    foreach (string header in headers)
                    {
                        currentRecord.Add(record[header]);
                    }
                    outputStream.Write(String.Join(",", currentRecord.ToArray()));
                    outputStream.WriteLine();
                }
            }
        }

        public void PrintSelfCSV(string output)
        {
            using (StreamWriter outputStream = new StreamWriter(output))
            {
                outputStream.Write(String.Join(",", headers));
                outputStream.WriteLine();
                foreach (Dictionary<string, string> record in records)
                {
                    List<string> currentRecord = new List<string>();
                    foreach (string header in headers)
                    {
                        currentRecord.Add(record[header]);
                    }
                    outputStream.Write(String.Join(",", currentRecord.ToArray()));
                    outputStream.WriteLine();
                }
            }

        }
        #endregion Methods

        public List<string[]> DataWithHeaders
        {
            get
            {
                List<string[]> data_with_headers = new List<string[]>();
                data_with_headers.Add(headers);
                foreach (var row in raw_data)
                {
                    data_with_headers.Add(row);
                }
                return data_with_headers;
            }
        }
    }
}
