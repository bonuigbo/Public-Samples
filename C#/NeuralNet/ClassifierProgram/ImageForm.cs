using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Drawing.Drawing2D;

namespace NeuralNet
{
    public partial class ImageForm : Form
    {
        #region Members

        private int x;
        private int y;

        private Bitmap map;
        private GraphicsPath g_path;
        private Network net;

        #endregion Members

        public ImageForm()
        {
            InitializeComponent();
            // Load the image files
            InitializeDrawingMap();
        }

        private void buttonInitialize_Click(object sender, EventArgs e)
        {
            Logger logger = new Logger(checkBoxLog.Checked);
            double learningRate = Double.Parse(textBoxLearningRate.Text);
            int[] layerStruct = ConvertToLayerStruct(textBoxLayerStructure.Text);
            string dataFile = Path.Combine(Configuration.TrainingData, Configuration.dataFileName);
            List<InputPair> inputVectors = DataConverter.ConvertDataFileToInputPairs(dataFile);

            if (radioButtonBackProp.Checked)
                net = new BackPropagationNetwork(logger);
            else if (radioButtonART.Checked)
                net = new ARTNetwork(logger);
            else if (radioButtonHRM.Checked)
                net = new HRMNetwork(logger);

            net.InitializeNetwork(inputVectors, layerStruct, learningRate);
            net.InitializeWeightsAndBiases(Double.Parse(textBoxWeightsFactor.Text),
                Double.Parse(textBoxBiasFactor.Text),
                radioButtonRandom.Checked);
            labelTotalEpochs.Text = net.Epochs.ToString();
        }

        /// <summary>
        /// Trains the net using all of the images in the image folder
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void buttonTrain_Click(object sender, EventArgs e)
        {
            if (net != null)
            {
                net.TrainNetwork(Int32.Parse(this.textBoxEpocjhs.Text));

                this.dataGridViewConfusion.DataSource = DataConverter.ConvertConfusionMatrix(net.ConfusionMatrix);
                // Separate procedure for the ART network
                if (radioButtonART.Checked)
                {
                    this.dataGridViewConfusion.DataSource = DataConverter.ConvertARTMatrix(net.ConfusionMatrix);
                    ARTNetwork network = (ARTNetwork)net;
                    List<Matrix> clusterWeights = network.GetClusterWeights();
                    for(int i = 0; i < clusterWeights.Count; i++)
                    {
                        Bitmap imageMap = DataConverter.ConvertMatrixToImage(clusterWeights[i], radioButtonBipolar.Checked);
                        string letter = CheckedLetter();
                        string imageName = i.ToString() + "-cluster.bmp";
                        string imagePath = Path.Combine(Configuration.Images, imageName);
                        imageMap.Save(imagePath);
                    }
                }
                labelTotalEpochs.Text = net.Epochs.ToString();
                net.Logger.PrintLogs();
            }
        }

        private void buttonTest_Click(object sender, EventArgs e)
        {
            if (net != null)
            {
                string dataFile = Path.Combine(Configuration.TestData, Configuration.dataFileName);
                List<InputPair> inputVectors = DataConverter.ConvertDataFileToInputPairs(dataFile);
                net.TestNetwork(inputVectors);
                this.dataGridViewConfusion.DataSource = DataConverter.ConvertConfusionMatrix(net.ConfusionMatrix);
                if (radioButtonART.Checked)
                    this.dataGridViewConfusion.DataSource = DataConverter.ConvertARTMatrix(net.ConfusionMatrix);
                net.Logger.PrintLogs();
            }
        }

        #region PictureBoxMethods

        /// <summary>
        /// This sets up the mouse event handlers for the object
        /// we wish to write letters on
        /// </summary>
        public void InitializeDrawingMap()
        {
            // Setup the drawing box
            map = new Bitmap(picture_box_drawing.Width, picture_box_drawing.Height);
            g_path = null;
            for (int x = 0; x < map.Width; x++)
            {
                for (int y = 0; y < map.Height; y++)
                {
                    map.SetPixel(x, y, Color.White);
                }
            }
            picture_box_drawing.Image = map;
            // Event handlers for the mouse
            picture_box_drawing.MouseDown += new MouseEventHandler(p_MouseDown);
            picture_box_drawing.MouseMove += new MouseEventHandler(p_MouseMove);
            picture_box_drawing.MouseUp += new MouseEventHandler(p_MouseUp);
            picture_box_drawing.Paint += new PaintEventHandler(p_Paint);
        }

        /// <summary>
        /// When the mouse is clicked, initialize the start point for the graphics path
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void p_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)            
           {
                this.x = e.X;
                this.y = e.Y;
                if (g_path == null)
                    g_path = new GraphicsPath();
            }
        }

        private void p_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                this.g_path.AddLine(new Point(this.x, this.y), new Point(e.X, e.Y));
                this.x = e.X;
                this.y = e.Y;
            }
        }

        private void p_MouseUp(object sender, MouseEventArgs e)
        {
            if (g_path != null)
            {
                using (Graphics g = Graphics.FromImage(map))
                {
                    if (e.Button == MouseButtons.Left)
                    {
                        g.SmoothingMode = SmoothingMode.AntiAlias;
                        using (Pen pen = new Pen(Color.Black, float.Parse(this.text_box_pen_width.Text)))
                            g.DrawPath(pen, g_path);
                    }
                }
                this.picture_box_drawing.Image = map;
                g_path.Dispose();
                g_path = null;
            }
            if (e.Button == MouseButtons.Left )
            {
                this.x = e.X;
                this.y = e.Y;
                if (g_path == null)
                    g_path = new GraphicsPath();
            }
        }

        private void p_Paint(object sender, PaintEventArgs e)
        {
            if (map != null)
                e.Graphics.DrawImage(map, 0, 0);
            if(g_path != null)
            {
                e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;
                using(Pen pen = new Pen(Color.Black, float.Parse(this.text_box_pen_width.Text)))
                    e.Graphics.DrawPath(pen, g_path);
            }
            this.picture_box_drawing.Image = map;
        }

        #endregion PictureBoxMethods

        /// <summary>
        /// Resets the bitmap image with a white background
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void button_clear_Click(object sender, EventArgs e)
        {
            Bitmap new_map = map;
            map = new Bitmap(new_map.Width, new_map.Height);
            for (int x = 0; x < map.Width; x++)
            {
                for (int y = 0; y < map.Height; y++)
                {
                    map.SetPixel(x, y, Color.White);
                }
            }
            if (new_map != null)
                new_map.Dispose();
            this.picture_box_drawing.Image = map;
        }

        /// <summary>
        /// Saves the current letter image to a file. They are named
        /// by the letter following a count of the letter file
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void button_save_Click(object sender, EventArgs e)
        {
            if (map != null)
            {
                string letter = CheckedLetter();
                //int rescaleFactor = Int32.Parse(textBoxRescale.Text);
                //Bitmap imageMap = DataConverter.ResizeImage(map, rescaleFactor, rescaleFactor);
                //imageMap = DataConverter.FilterBitmap(imageMap);
                string imageName = string.Format(@"{0}_{1}.bmp", letter, Guid.NewGuid());
                string imagePath;
                if (radioButtonTraining.Checked)
                    imagePath = Path.Combine(Configuration.TrainingImages, imageName);
                else
                    imagePath = Path.Combine(Configuration.TestImages, imageName);
                map.Save(imagePath);
            }
        }

        /// <summary>
        /// Returns the letter that is currently checked in the radio box
        /// </summary>
        /// <returns>string representing the single letter</returns>
        private string CheckedLetter()
        {
            if (radioButtonA.Checked)
            {
                return "A";
            }
            else if (radioButtonB.Checked)
            {
                return "B";
            }
            else if (radioButtonC.Checked)
            {
                return "C";
            }
            else if (radioButtonD.Checked)
            {
                return "D";
            }
            else if (radioButtonE.Checked)
            {
                return "E";
            }
            else if (radioButtonF.Checked)
            {
                return "F";
            }
            else
            {
                return "G";
            }
        }

        /// <summary>
        /// Generates the struct representing the struct of the hidden layers
        /// </summary>
        /// <param name="data"></param>
        /// <returns></returns>
        private int[] ConvertToLayerStruct(string data)
        {
            string[] rawStruct = data.Split(',');
            int[] layerStruct = new int[rawStruct.Length];
            for (int i = 0; i < layerStruct.Length; i++)
            {
                layerStruct[i] = Int32.Parse(rawStruct[i]);
            }
            return layerStruct;
        }

        /// <summary>
        /// Convert the images in the training and test image folders to csv files
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void buttonImageToData_Click(object sender, EventArgs e)
        {
            int rescaleFactor = Int32.Parse(textBoxRescale.Text);
            bool binary = radioButtonBipolar.Checked;
            List<string> imageFiles = Directory.GetFiles(Configuration.TrainingImages).ToList();
            string outputFile = Path.Combine(Configuration.TrainingData, "data.csv");
            DataConverter.ConvertImagesToDataFile(imageFiles, outputFile, binary, rescaleFactor);

            imageFiles = Directory.GetFiles(Configuration.TestImages).ToList();
            outputFile = Path.Combine(Configuration.TestData, "data.csv");
            DataConverter.ConvertImagesToDataFile(imageFiles, outputFile, binary, rescaleFactor);
        }
        
        private void buttonScale_Click(object sender, EventArgs e)
        {
            List<string> imageFiles = Directory.GetFiles(Configuration.TrainingImages).ToList();
            foreach (var imageFile in imageFiles)
            {
                int rescaleFactor = Int32.Parse(textBoxRescale.Text);
                string dataFile = Path.Combine(Configuration.TestData, Configuration.dataFileName);
                //Bitmap imageMap = DataConverter.ResizeImage(map, rescaleFactor, rescaleFactor);
                //imageMap = DataConverter.FilterBitmap(imageMap);
                List<InputPair> inputVectors = DataConverter.ConvertMapToInputPairs(map, CheckedLetter(), radioButtonBipolar.Checked, rescaleFactor );
                net.TestNetwork(inputVectors);
                textBoxClassified.Text = inputVectors[0].ClassifiedID;
                //this.dataGridViewConfusion.DataSource = DataConverter.ConvertConfusionMatrix(net.ConfusionMatrix);
                net.Logger.PrintLogs();
            }
        }
    }
}
