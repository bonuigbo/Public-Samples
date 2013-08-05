using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MathNet.Numerics.LinearAlgebra.Double;

namespace ClassifierProgram
{
    class MatrixError : Exception
    {
        private string errorMessage;
        public MatrixError(string errorMessage)
        {
            this.errorMessage = errorMessage;
        }
        public override string Message
        {
            get { return errorMessage; }
        }
    }

    /*
     *  Matrix operations
     */
    class Matrix
    {
        #region Members

        private int rowCount;
        private int columnCount;
        private DenseMatrix matrixArray;

        #endregion Members

        #region Constructors

        /// <summary>
        /// Initialize a matrix
        /// </summary>
        /// <param name="rowCount">Number of rows</param>
        /// <param name="columnCount">Number of columns</param>
        public Matrix(int rowCount, int columnCount)
        {
            matrixArray = new DenseMatrix(rowCount, columnCount, 0);
            this.rowCount = rowCount;
            this.columnCount = columnCount;
        }

        /// <summary>
        /// Initialize using a doube[,]
        /// </summary>
        /// <param name="matrixArrayd"></param>
        public Matrix(double[,] matrixArrayd)
        {
            this.matrixArray = new DenseMatrix(matrixArrayd);
            this.rowCount = matrixArray.RowCount;
            this.columnCount = matrixArray.ColumnCount;
        }

        /// <summary>
        /// Can be initialized using a dense matrix);
        /// </summary>
        /// <param name="matrix"></param>
        public Matrix(DenseMatrix matrix)
        {
            this.matrixArray = matrix;
            this.rowCount = matrixArray.RowCount;
            this.columnCount = matrixArray.ColumnCount;
        }

        #endregion Constructors

        #region Static Methods


        public static Matrix operator +(Matrix matrix1, Matrix matrix2)
        {
            if (matrix1.RowCount != matrix2.RowCount && matrix1.ColumnCount != matrix2.ColumnCount)
                throw new MatrixError(String.Format("A {0}-{1} matrix cannot be added to a {2}-{3} matrix",
                matrix1.rowCount, matrix1.ColumnCount, matrix2.rowCount, matrix2.ColumnCount));
            return new Matrix(matrix1.matrixArray + matrix2.matrixArray);  
        }

        public static Matrix operator -(Matrix matrix1, Matrix matrix2)
        {
            if (matrix1.RowCount != matrix2.RowCount && matrix1.RowCount != matrix2.ColumnCount)
                throw new MatrixError(String.Format("A {0}-{1} matrix cannot be added to a {2}-{3} matrix",
                matrix1.rowCount, matrix1.ColumnCount, matrix2.rowCount, matrix2.ColumnCount));
            return new Matrix(matrix1.MatrixArray - matrix2.matrixArray);  
        }

        public static Matrix operator*(double scalar, Matrix matrix)
        {
            return new Matrix(scalar * matrix.matrixArray);
        }
         
        public static Matrix operator *(Matrix matrix1, Matrix matrix2)
        {
            if (matrix1.ColumnCount != matrix2.RowCount)
                throw new MatrixError(String.Format("Column count {0} of matrix one does not match" +
                    " {1} of matrix two", matrix1.ColumnCount, matrix2.rowCount));
            return new Matrix(matrix1.matrixArray * matrix2.matrixArray);              
        }        

        private static double sumVector(int row, int column, Matrix matrix1, Matrix matrix2)
        {
            double value = 0;
            for (int matrix1Col = 0; matrix1Col < matrix1.ColumnCount; matrix1Col++)
            {
                    value += matrix1[row, matrix1Col] * matrix2[matrix1Col, column];
            }
            return value;
        }

        #endregion Static Methods

        #region Methods



        public override string ToString()
        {
            string value = " {{ ";
            for (int i = 0; i < this.rowCount; i++)
            {
                value += "[";
                for (int j = 0; j < this.columnCount; j++)
                {
                    value += " " + String.Format("{0:0.000000}", matrixArray[i, j]) + " ";
                }
                value += "]";
            }
            return value + " }} ";
        }

        #endregion Methods

        #region Properties

        public int RowCount
        {
            get { return rowCount; }
        }

        public int ColumnCount
        {
            get { return columnCount; }
        }

        public DenseMatrix MatrixArray
        {
            get{ return this.matrixArray; }
        }

        public double this[int row, int column]
        {
            get 
            {
                if(row >= matrixArray.RowCount || column >= matrixArray.ColumnCount)
                {
                    throw new MatrixError(String.Format("Index [{0},{1}] outside of bounds [{2},{3}]", row, column, this.rowCount, this.columnCount));
                }
                return matrixArray[row, column];                 
            }
            set 
            {
                if (row >= matrixArray.RowCount || column >= matrixArray.ColumnCount)
                {
                    throw new MatrixError(String.Format("Index [{0},{1}] outside of bounds [{2},{3}]", row, column, this.rowCount, this.columnCount));
                }
                matrixArray[row, column] = value; 
            }
        }

        public string Values
        {
            get
            {
                string value = " {{ ";
                for (int i = 0; i < this.rowCount; i++)
                {
                    value += "[";
                    for (int j = 0; j < this.columnCount; j++)
                    {
                        value += " " + String.Format("{0:0.000000}", matrixArray[i, j]) + " ";
                    }
                    value += "]";
                }
                return value + " }} ";
            }
        }

        public Matrix Clone
        {
            
            get
            {
                Matrix clone = new Matrix(this.rowCount, this.columnCount);
                for (int i = 0; i < clone.rowCount; i++)
                {
                    for (int j = 0; j < clone.ColumnCount; j++)
                    {
                        clone[i, j] = this[i, j];
                    }
                }
                return clone;
            }
        }

        public Matrix Transpose
        {
            get
            {
                DenseMatrix transposeMatrix = (DenseMatrix)this.matrixArray.Transpose();
                return new Matrix(transposeMatrix);
            }
        }

        public Matrix Inverse
        {
            get
            {
                if (this.rowCount != this.columnCount)
                {
                    throw new MatrixError("Matrix with different row and column count has no inverse");
                }
                DenseMatrix inverseMatrix = (DenseMatrix)this.matrixArray.Inverse();
                return new Matrix(inverseMatrix);            }
        }

        public double Determinant
        {
            get
            {
                return this.matrixArray.Determinant();
            }
        }
        #endregion Properties
    }
}
