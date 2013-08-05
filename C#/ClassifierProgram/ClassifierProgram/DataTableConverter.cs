using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Data;
using System.Drawing;
using System.IO;

namespace ClassifierProgram
{
    /*
     *  Whenever anything needs to be shown on the WindowsForm
     *  in a chart, it is converted to the propert form here
     * 
     */
    class DataTableConverter
    {
        public static DataTable Convert(List<ClassifierClass> classList)
        {
            DataTable table = new DataTable("Gaussian Stats");
            table.Columns.Add("Statistics", typeof(string));
            foreach (var gaussClass in classList)
            {
                table.Columns.Add(gaussClass.ClassID, typeof(string));
            }
            List<string> sampleSizeRow = new List<string>();
            sampleSizeRow.Add("Sample Size");
            List<string> meanRow = new List<string>();
            meanRow.Add("Mean");
            List<string> covarianceRow = new List<string>();
            covarianceRow.Add("Covariance");
            List<string> inverseCoRow = new List<string>();
            inverseCoRow.Add("InverseCo");
            List<string> inverseDetRow = new List<string>();
            inverseDetRow.Add("Inv Determ");
            StreamWriter writer = new StreamWriter("d:\\stuff.csv");
            for(int i = 0; i < classList.Count; i++)
            {
                sampleSizeRow.Add(classList[i].FeatureList.Count.ToString());
                meanRow.Add(classList[i].Mean.ToString());
                Matrix Covariance = classList[i].Covariance;
                Matrix InverseCo = classList[i].Covariance.Inverse;
                
                for (int m = 0; m < Covariance.RowCount; m++)
                {
                    for (int j = 0; j < Covariance.ColumnCount; j++)
                    {
                        writer.Write(Covariance[m, j].ToString());
                        if (j != Covariance.ColumnCount - 1)
                            writer.Write(',');
                    }
                    writer.WriteLine();
                }
                for (int m = 0; m < InverseCo.RowCount; m++)
                {
                    for (int j = 0; j < InverseCo.ColumnCount; j++)
                    {
                        writer.Write(InverseCo[m, j].ToString());
                        if (j != InverseCo.ColumnCount - 1)
                            writer.Write(',');
                    }
                    writer.WriteLine();
                }
                covarianceRow.Add(classList[i].Covariance.ToString());

                inverseCoRow.Add(classList[i].Covariance.Inverse.ToString());
                inverseDetRow.Add(classList[i].Covariance.Inverse.Determinant.ToString());
            }
            writer.Close();

            table.Rows.Add(sampleSizeRow.ToArray());
            table.Rows.Add(meanRow.ToArray());
            table.Rows.Add(covarianceRow.ToArray());
            table.Rows.Add(inverseCoRow.ToArray());
            table.Rows.Add(inverseDetRow.ToArray());
            return table;
        }

        public static DataTable ConvertConfusionMatrix(Matrix confusionMatrix, List<ClassifierClass> classList)
        {
            DataTable table = new DataTable("Confusion Table");
            table.Columns.Add("Class", typeof(string));
            for (int i = 0; i < classList.Count; i++)
            {
                table.Columns.Add(classList[i].ClassID, typeof(string));
            }
            table.Columns.Add("CCR", typeof(string));
            for (int i = 0; i < confusionMatrix.RowCount; i++)
            {
                List<string> rowStuff = new List<string>();
                if (i < classList.Count)
                    rowStuff.Add(classList[i].ClassID);
                else
                    rowStuff.Add("Total");
                for (int j = 0; j < confusionMatrix.ColumnCount; j++)
                {
                    if (j == confusionMatrix.ColumnCount - 1)
                        rowStuff.Add(confusionMatrix[i, j].ToString("0.000%"));
                    else
                        rowStuff.Add(confusionMatrix[i, j].ToString());
                }
                table.Rows.Add(rowStuff.ToArray());
            }
            return table;
        }

        public static DataTable ConvertStringList(List<string[]> rawData)
        {
            DataTable table = new DataTable("Gaussian Stats");
            table.Columns.Add("FIeld");
            table.Columns.Add("Value");
            foreach (string[] rowOfLines in rawData)
            {
                table.Rows.Add(rowOfLines);               
            }
            
            return table;
        }
    }
}
