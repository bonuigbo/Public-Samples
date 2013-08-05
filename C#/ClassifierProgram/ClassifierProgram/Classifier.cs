using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace ClassifierProgram
{
    class ClassifierError : Exception
    {
        private string errorMessage;
        public ClassifierError(string errorMessage)
        {
            this.errorMessage = errorMessage;
        }
        public override string Message
        {
            get { return errorMessage; }
        }
    }

    /*
     *      Mean t for storing relevant information about each class, including
     *      its list of features, mean, covariance. etc     * 
     */
    class ClassifierClass
    {

        #region Members

        private string identifier;
        private List<Matrix> featuresList;
        private Matrix mean;
        private Matrix covariance;

        #endregion Members

        #region Methods

        public ClassifierClass(string identifier, List<Matrix> featuresList)
        {
            this.identifier = identifier;
            this.featuresList = featuresList;
            ComputeMean();
            ComputeCovariance();
        }

        private void ComputeMean()
        {
            if (featuresList.Count < 1)
                throw new ClassifierError("Cannot compute mean of 0 vectors");
            mean = new Matrix(featuresList[0].RowCount, 1);
            foreach (Matrix matrix in featuresList)
            {
                mean = mean + matrix;
            }
            mean = (double)(1.0 / (double)featuresList.Count) * mean;
        }

        private void ComputeCovariance()
        {
            Matrix meanSquare = mean * mean.Transpose;
            Matrix featureSquare = new Matrix(mean.RowCount, mean.RowCount);
            foreach (Matrix matrix in featuresList)
            {
                featureSquare = featureSquare + (matrix * matrix.Transpose);
            }
            featureSquare = (double)(1.0 / (double)featuresList.Count) * featureSquare;
            covariance = featureSquare - meanSquare;
        }

        #endregion Methods

        #region Properties

        public Matrix Mean
        {
            get { return mean; }
        }

        public Matrix Covariance
        {
            get { return covariance; }
        }

        public string ClassID
        {
            get { return identifier; }
        }

        public List<Matrix> FeatureList
        {
            get { return featuresList; }
        }

        #endregion Properties

    }

    /*
            This struct stores each classified feature, meaning its original class, 
            its classified class, and its score for each class in its set.             
     */
    class ClassifiedPoint
    {
        public Matrix point;
        public string originalClassID;
        public Dictionary<string, double> classScores;
        public string classifiedClass;


        public ClassifiedPoint(Matrix point, Dictionary<string, double> classScores,
            string originalClassID, string classifiedClass)
        {
            this.point = point;
            this.originalClassID = originalClassID;
            this.classScores = classScores;
            this.classifiedClass = classifiedClass;
        }
    }

    /*
            Handles loading, converting data            
     */
    abstract class Classifier
    {
        #region Members

        protected List<ClassifierClass> classList;
        protected List<Matrix> designDataSet;
        protected List<Matrix> featureDataSet;
        protected List<ClassifiedPoint> classifiedPoints;
        protected Matrix confusionMatrix;
        protected StreamWriter debugger;
        protected bool debugMode;
        protected string debugFile;

        #endregion Members

        #region Methods

        public Classifier()
        {
            confusionMatrix = null;
            designDataSet = null;
            featureDataSet = null;
            this.classList = new List<ClassifierClass>();
            this.classifiedPoints = new List<ClassifiedPoint>();
            debugMode = false;
        }

        protected void Reset()
        {
            this.classifiedPoints.Clear();
            confusionMatrix = null;
            this.designDataSet = new List<Matrix>();
        }
        public Classifier(string debugFile)
        {
            confusionMatrix = null;
            this.classList = new List<ClassifierClass>();
            this.classifiedPoints = new List<ClassifiedPoint>();
            this.debugFile = debugFile;
            debugMode = true;
            
        }

        public static void Initialize(Dictionary<string, List<Matrix>> dataSet, List<ClassifierClass> classList)
        {
            classList.Clear();
            foreach (var keyCode in dataSet.Keys)
            {
                ClassifierClass currentClass = new ClassifierClass(keyCode, dataSet[keyCode]);
                classList.Add(currentClass);
            }
        }

        public virtual void Classify(Dictionary<string, List<Matrix>> dataSet, List<ClassifierClass> classList)
        {
            Reset();
            this.classList = classList;
            if (debugMode)
                debugger = new StreamWriter(debugFile);
            this.classifiedPoints = new List<ClassifiedPoint>();
            foreach (var classID in dataSet.Keys)
            {
                foreach (var matrix in dataSet[classID])
                {
                    ClassifyVector(matrix, classID);
                }
            }
            GenerateConfusionMatrix();
            if (debugMode)
                debugger.Close();
        }

        public virtual void Classify(List<ClassifierClass> classList)
        {
            Reset();
            this.classList = classList;
            if (debugMode)
                debugger = new StreamWriter(debugFile);
            this.classifiedPoints = new List<ClassifiedPoint>();
            foreach (var gaussClass in classList)
            {
                foreach (var matrix in gaussClass.FeatureList)
                {
                    ClassifyVector(matrix, gaussClass.ClassID);
                }
            }
            GenerateConfusionMatrix();
            if (debugMode)
                debugger.Close();
        }

        public void GenerateConfusionMatrix()
        {
            confusionMatrix = new Matrix(classList.Count + 1, classList.Count + 1);
            Dictionary<string, int> map = new Dictionary<string, int>();
            int startPos = 0;
            //A way to map the classes in order by referring to them as ints
            for (int i = 0; i < classList.Count; i++)
            {
                map[classList[i].ClassID] = startPos;
                startPos++;
            }
            // Map matches and miss matches
            foreach (var featureStruct in classifiedPoints)
            {
                int row = 0;
                try
                {
                    row = map[featureStruct.originalClassID];
                }
                catch (KeyNotFoundException e)
                {         
                    throw new ClassifierError("The class list in the test data must match that of the design data - " + e.ToString());
                }
                int column = map[featureStruct.classifiedClass];
                confusionMatrix[row, column]++;
            }
            double percentageCount = 0;
            for (int i = 0; i < classList.Count; i++)
            {
                double count = 0;

                for (int j = 0; j < classList.Count; j++)
                {
                    count += confusionMatrix[i, j];
                }
                confusionMatrix[i, classList.Count] = confusionMatrix[i, i] / count;
                percentageCount += confusionMatrix[i, classList.Count];
            }
            //Set last variable
            confusionMatrix[confusionMatrix.RowCount - 1, confusionMatrix.ColumnCount - 1]
                = percentageCount / (confusionMatrix.ColumnCount - 1);
        }

        #endregion Methods

        #region AbstractMethods

        public abstract void ClassifyVector(Matrix matrix, string origClass);

        #endregion AbstractMethods

        #region Properties

        public List<ClassifierClass> ClassList
        {
            get { return classList; }
        }

        public List<ClassifiedPoint> ClassifiedFeatures
        {
            get { return classifiedPoints; }
        }

        public Matrix ConfusionMatrix
        {
            get { return confusionMatrix; }
        }

        #endregion Properties
    }

    class MinDistanceClassifier : Classifier
    {
        public MinDistanceClassifier()
            : base()
        {
        }

        public MinDistanceClassifier(string debugFile)
            : base(debugFile)
        {
        }

        public override void ClassifyVector(Matrix matrix, string origClass)
        {
            if (debugMode)
                debugger.WriteLine(String.Format("X = {0} -- Class: {1}", matrix.ToString(), origClass));
            Dictionary<string, double> classScores = new Dictionary<string, double>();
            foreach (var minimumClass in classList)
            {
                Matrix classifiedMatrix = ((2 * matrix.Transpose) * minimumClass.Mean) - (minimumClass.Mean.Transpose * minimumClass.Mean);
                classScores[minimumClass.ClassID] = classifiedMatrix[0, 0];
            }
            double minScore = -10000000;
            string classifiedClass = "";
            foreach (var score in classScores)
            {
                if (score.Value > minScore)
                {
                    minScore = score.Value;
                    classifiedClass = score.Key;
                }
            }
            ClassifiedPoint featureStruct = new ClassifiedPoint(matrix, classScores, origClass, classifiedClass);
            classifiedPoints.Add(featureStruct);
        }
    }

    class GaussianClassifier : Classifier
    {
        #region Methods

        public GaussianClassifier()
            : base()
        {
        }

        public GaussianClassifier(string debugFile)
            : base(debugFile)
        {
        }

        public override void ClassifyVector(Matrix matrix, string origClass)
        {
            if (debugMode)
                debugger.WriteLine(String.Format("X = {0} -- Class: {1}", matrix.ToString(), origClass));
            Dictionary<string, double> classScores = new Dictionary<string, double>();
            foreach (var classifierClass in classList)
            {
                Matrix distanceMatrix = matrix - classifierClass.Mean;
                Matrix classifiedMatrix = (distanceMatrix.Transpose * classifierClass.Covariance.Inverse) * distanceMatrix;
                classScores[classifierClass.ClassID] = classifiedMatrix[0, 0];
                if (debugMode)
                    debugger.WriteLine(String.Format("Score {0}  for class {1}", classifiedMatrix.ToString(), classifierClass.ClassID));
            }
            // This classifies the vector based on the gaussian metric
            double maxScore = 10000000;
            string classifiedClass = "";
            foreach (var score in classScores)
            {
                if (score.Value < maxScore)
                {
                    maxScore = score.Value;
                    classifiedClass = score.Key;
                }
            }
            if(debugMode)
                debugger.WriteLine("Classify X to " + classifiedClass);
            ClassifiedPoint featureStruct = new ClassifiedPoint(matrix, classScores, origClass, classifiedClass);
            classifiedPoints.Add(featureStruct);
        }

        #endregion Methods
    }

    class KNNClassifier : Classifier
    {
        private struct Point
        {
            public double distance;
            public string pointClass;
            public Point(double distance, string pointClass)
            {
                this.distance = distance; this.pointClass = pointClass;
            }
        }
        private int k;
        private Dictionary<string, List<Matrix>> dataSet;
        private string status;

        #region Methods

        public KNNClassifier(int k)
            : base()
        {
            status = "Initializing KNN";
            dataSet = null;
            this.k = k;
        }

        public KNNClassifier(int k, string debugFile)
            : base(debugFile)
        {
            status = "Initializing KNN";
            dataSet = null;
            this.k = k;
        }

        public override void Classify(Dictionary<string, List<Matrix>> dataSet, List<ClassifierClass> classList)
        {
            Reset();
            this.classList = classList;
            this.dataSet = dataSet;
            if (debugMode)
                debugger = new StreamWriter(debugFile);
            this.classifiedPoints = new List<ClassifiedPoint>();
            foreach (var classID in dataSet.Keys)
            {
                
                foreach (var matrix in dataSet[classID])
                {
                    ClassifyVector(matrix, classID);
                }
            }
            GenerateConfusionMatrix();
            if (debugMode)
                debugger.Close();
        }

        /*
         *      Classifies each vector based on the particular classifier algorithm
         */
        public override void ClassifyVector(Matrix matrix, string origClass)
        {
            if (dataSet != null)
                ClassifyTestVector(matrix, origClass);
            else
                ClassifyDesignVector(matrix, origClass);
        }

        public void ClassifyDesignVector(Matrix matrix, string origClass)
        {
            if (debugMode)
                debugger.WriteLine(String.Format("X = {0} -- Class: {1}", matrix.ToString(), origClass));
            // Class scores represents the count of each class within the K cluster of this sample point
            Dictionary<string, double> classScores = new Dictionary<string, double>();
            List<Point> pointList = new List<Point>();
            // Determine the distance for each matrix, which are stored in their respective classes
            foreach (var classifierClass in classList)
            {
                classScores[classifierClass.ClassID] = 0;
                foreach (var externalMatrix in classifierClass.FeatureList)
                {
                    // Skip the matrix when X encounters itself
                    Matrix distanceMatrix = ((matrix - externalMatrix).Transpose) * (matrix - externalMatrix);
                    if (distanceMatrix[0, 0] == 0)
                        continue;
                    double distance = (double)(Math.Sqrt((double)distanceMatrix[0, 0]));

                    Point point = new Point(distance, classifierClass.ClassID);
                    if (debugMode)
                        debugger.WriteLine(String.Format("Y = {0} -- Class = {1} -- " +
                            "X-Y = {2} -- DistanceMatrix = {3} -- Distance = {4}", externalMatrix.ToString(), classifierClass.ClassID,
                            (matrix - externalMatrix).ToString(), distanceMatrix.ToString(), distance.ToString("#.####")));
                    // Add the points to the List according to distance
                    int i = 0;
                    while (i < pointList.Count)
                    {
                        if (distance > pointList[i].distance)
                            i++;
                        else
                            break;
                    }
                    pointList.Insert(i, point);
                }
            }
            if (debugMode)
            {
                for (int i = 0; i < pointList.Count; i++)
                {
                    debugger.WriteLine(String.Format("Distance: {0} -- Class: {1}", pointList[i].distance.ToString("#.####"), pointList[i].pointClass));
                }
            }

            if (k > pointList.Count)
                throw new ClassifierError("The Value of K cannot be greater than the number of sample points");
            //Remove the first vector since its the same as the current vector so has a score of 0
            //pointList.RemoveAt(0);
            // Now rank the classes by adding 1 each time its within K units of the matrix
            for (int i = 0; i < k; i++)
            {
                string currentClass = pointList[i].pointClass;
                classScores[currentClass]++;
            }
            // This classifies the vector based on the gaussian metric
            double maxScore = -10000000;
            string classifiedClass = "";
            foreach (var score in classScores)
            {
                if (score.Value > maxScore)
                {
                    maxScore = score.Value;
                    classifiedClass = score.Key;
                }
            }
            if (debugMode)
                debugger.WriteLine("Classify X to " + classifiedClass);
            ClassifiedPoint featureStruct = new ClassifiedPoint(matrix, classScores, origClass, classifiedClass);
            classifiedPoints.Add(featureStruct);
        }

        public void ClassifyTestVector(Matrix matrix, string origClass)
        {
            if (debugMode)
                debugger.WriteLine(String.Format("X = {0} -- Class: {1}", matrix.ToString(), origClass));
            // Class scores represents the count of each class within the K cluster of this sample point
            Dictionary<string, double> classScores = new Dictionary<string, double>();
            List<Point> pointList = new List<Point>();
            // Determine the distance for each matrix, which are stored in their respective classes

            status = String.Format("Computing distance for vector from class {0}", origClass);

            foreach (var classifierClass in classList)
            {
                classScores[classifierClass.ClassID] = 0;
                foreach (var externalMatrix in classifierClass.FeatureList)
                {
                    // Skip the matrix when X encounters itself
                    Matrix distanceMatrix = ((matrix - externalMatrix).Transpose) * (matrix - externalMatrix);
                    if (distanceMatrix[0, 0] == 0)
                        continue;
                    double distance = (double)(Math.Sqrt((double)distanceMatrix[0, 0]));

                    Point point = new Point(distance, classifierClass.ClassID);
                    if (debugMode)
                        debugger.WriteLine(String.Format("Y = {0} -- Class = {1} -- " +
                            "X-Y = {2} -- DistanceMatrix = {3} -- Distance = {4}", externalMatrix.ToString(), classifierClass.ClassID,
                            (matrix - externalMatrix).ToString(), distanceMatrix.ToString(), distance.ToString("#.####")));
                    // Add the points to the List according to distance
                    int i = 0;
                    while (i < pointList.Count)
                    {
                        if (distance > pointList[i].distance)
                            i++;
                        else
                            break;
                    }
                    pointList.Insert(i, point);
                }
            }
            foreach (var classID in dataSet.Keys)
            {
                foreach (var externalMatrix in dataSet[classID])
                {
                    // Skip the matrix when X encounters itself
                    Matrix distanceMatrix = ((matrix - externalMatrix).Transpose) * (matrix - externalMatrix);
                    if (distanceMatrix[0, 0] == 0)
                        continue;
                    double distance = (double)(Math.Sqrt((double)distanceMatrix[0, 0]));

                    Point point = new Point(distance, classID);
                    if (debugMode)
                        debugger.WriteLine(String.Format("Y = {0} -- Class = {1} -- " +
                            "X-Y = {2} -- DistanceMatrix = {3} -- Distance = {4}", externalMatrix.ToString(), classID,
                            (matrix - externalMatrix).ToString(), distanceMatrix.ToString(), distance.ToString("#.####")));
                    // Add the points to the List according to distance
                    int i = 0;
                    while (i < pointList.Count)
                    {
                        if (distance > pointList[i].distance)
                            i++;
                        else
                            break;
                    }
                    pointList.Insert(i, point);
                }
            }
            if (debugMode)
            {
                for (int i = 0; i < pointList.Count; i++)
                {
                    debugger.WriteLine(String.Format("Distance: {0} -- Class: {1}", pointList[i].distance.ToString("#.####"), pointList[i].pointClass));
                }
            }

            if (k > pointList.Count)
                throw new ClassifierError("The Value of K cannot be greater than the number of sample points");
            //Remove the first vector since its the same as the current vector so has a score of 0
            //pointList.RemoveAt(0);
            // Now rank the classes by adding 1 each time its within K units of the matrix
            status = String.Format("Finding the nearest within {0}", k);
            for (int i = 0; i < k; i++)
            {
                string currentClass = pointList[i].pointClass;
                classScores[currentClass]++;
            }
            // This classifies the vector based on the gaussian metric
            double maxScore = -10000000;
            string classifiedClass = "";
            foreach (var score in classScores)
            {
                if (score.Value > maxScore)
                {
                    maxScore = score.Value;
                    classifiedClass = score.Key;
                }
            }
            status = String.Format("Classified to {0}", classifiedClass);
            if (debugMode)
                debugger.WriteLine("Classify X to " + classifiedClass);
            ClassifiedPoint featureStruct = new ClassifiedPoint(matrix, classScores, origClass, classifiedClass);
            classifiedPoints.Add(featureStruct);
        }

        #endregion Methods

        public int K
        {
            get { return k; }
        }

        public string Status
        {
            get { return status; }
        }
    }
}
