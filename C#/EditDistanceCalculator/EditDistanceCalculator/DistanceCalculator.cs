using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.IO;

namespace EditDistanceCalculator
{
    /*
            Manages the logic of the program
     */
    class DistanceCalculator
    {
        #region Members

        private string input1;
        private string input2;
        private int editDistance;
        private int[,] editMatrix;
        private List<string> alignment;
        private List<int[]> alignmentVertices;

        #endregion Members

        #region Methods

        /*
                This class does all the necessary work when initialized
         */
        public DistanceCalculator(string input1, string input2)
        {
            this.input1 = input1;
            this.input2 = input2;
            this.editMatrix = new int[input1.Length+1, input2.Length+1];
            this.alignment = new List<string>();
            this.alignmentVertices = new List<int[]>();
            CalculateEditDistance();
            CalculateAlignment();
        }

        /*
                Generates the edit distance matrix
         */
        public void CalculateEditDistance()
        {
            for (int i = 1; i < input1.Length + 1; i++)
            {
                editMatrix[i, 0] = i;
            }
            for (int j = 1; j < input2.Length + 1; j++)
            {
                editMatrix[0, j] = j;
            }
            for (int j = 1; j < input2.Length + 1; j++)
            {
                for (int i = 1; i < input1.Length+1; i++)
                {
                    if (input1[i-1] == input2[j-1])
                        editMatrix[i, j] = editMatrix[i - 1, j - 1];
                    else
                    {
                        editMatrix[i, j] = Minimum(editMatrix[i-1, j]+1, editMatrix[i, j-1]+1, editMatrix[i-1, j-1]+1);
                    }
                }
            }
            editDistance = editMatrix[input1.Length, input2.Length];
        }

        /*
                Helper method
         */
        private int Minimum(int value1, int value2, int value3)
        {
            if (value1 < value2 && value1 < value3)
                return value1;
            else if (value2 < value1 && value2 < value3)
                return value2;
            else
                return value3;
        }

        /*
                Works back from the last element in the matrix and 
                generates the alignment words while storing the
                indices of the alignment matrix
         */
        public void CalculateAlignment()
        {
            string alignment1 = "";
            string alignment2 = "";
            int i = input1.Length;
            int j = input2.Length;
            alignmentVertices.Add(new int[] { i, j });
            while (i > 0 || j > 0)
            {
                // If we are at the edge of the Matrix, move in the only allowed direction
                if (i == 0)
                {
                    j--;
                    alignment1 += "_";
                    alignment2 += input2[j].ToString();
                    alignmentVertices.Add(new int[] { i, j });
                    
                }
                else if (j == 0)
                {
                    i--;
                    alignment1 += input1[i].ToString();
                    alignment2 += "_";
                    alignmentVertices.Add(new int[] { i, j });

                }
                
                else
                {
                    int leftScore = editMatrix[i, j - 1];
                    int upScore = editMatrix[i - 1, j];
                    int diagonalScore = editMatrix[i - 1, j - 1];
                    if (upScore < leftScore && upScore <= diagonalScore && input1[i - 1] != input2[j - 1])
                    {
                        i--;
                        alignment1 += input1[i].ToString();
                        alignment2 += "_";
                        alignmentVertices.Add(new int[] { i, j });
                    }
                    else if (leftScore <= diagonalScore && input1[i - 1] != input2[j - 1])
                    {
                        j--;
                        alignment1 += "_";
                        alignment2 += input2[j].ToString();
                        alignmentVertices.Add(new int[] { i, j });
                    }
                    else
                    {
                        i--; j--;
                        alignment1 += input1[i].ToString();
                        alignment2 += input2[j].ToString();
                        alignmentVertices.Add(new int[] { i, j });
                    }
                }
            }
            alignment.Add(Reverse(alignment1));
            alignment.Add(Reverse(alignment2));
        }

        /*
                Helper method for reversing strings 
         */
        private string Reverse(string original)
        {
            char[] arr = original.ToCharArray();
            Array.Reverse(arr);
            return new string(arr);
        }

        #endregion Methods

        #region Properties

        public int[,] EditMatrix
        {
            get
            {
                return editMatrix;
            }
        }

        public int EditDistance
        {
            get
            {
                return editDistance;
            }
        }

        public List<string> Alignment
        {
            get
            {
                return alignment;
            }
        }

        public List<int[]> AlignmentVertices
        {
            get
            {
                return alignmentVertices;
            }
        }

        #endregion Properties
    }
}