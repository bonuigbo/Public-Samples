using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ClassifierProgram
{
    public partial class ClassifierForm : Form
    {
        #region Members

        private readonly string AssemblyPath =   System.IO.Path.GetDirectoryName(Application.ExecutablePath);
        private Classifier classifier;
        private Generator generator;
        private bool dataOk;
        private List<ClassifierClass> classList;

        #endregion Members

        public ClassifierForm()
        {
            generator = null;
            classifier = null;
            dataOk = false;
            InitializeComponent();
        }


        private void HideStuff()
        {
            this.confusionGridView.DataSource = null;
            this.StatusLabel.Text = "";

        }

        private void classifierForm_Load(object sender, EventArgs e)
        {
            string[] classifiers = {"Gaussian","Minimum","KNN"};
            this.gaussianRadioButton.Checked = true;
        }

        private void loadButton_Click(object sender, EventArgs e)
        {
            dataOk = false;
            InitializeOpenDialog("Design Data", false);

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                this.designDataTextBox.Text = openFileDialog.FileName;
            }
        }

        private void InitializeOpenDialog(string titleWord, bool multiselect)
        {
            openFileDialog.Title = "Select the \"" + titleWord + "\" file to process";
            openFileDialog.Filter =
                "CSV|*.csv;|All files|*.*";
            openFileDialog.Multiselect = multiselect;
            if (openFileDialog.FileName != null &&
                    openFileDialog.FileName != "")
                openFileDialog.InitialDirectory =
                    System.IO.Path.GetDirectoryName(openFileDialog.FileName);
            else
                openFileDialog.InitialDirectory = AssemblyPath;
        }

        /*
                Initializes The Data
         */
        private void classifyButton_Click(object sender, EventArgs e)
        {
            HideStuff();
            this.classList = new List<ClassifierClass>();
            try
            {
                if (!String.IsNullOrEmpty(this.designDataTextBox.Text))
                {
                    CSVReader reader = new CSVReader(this.designDataTextBox.Text);
                    generator = new Generator(reader.RawData);
                    dataOk = true;

                    Classifier.Initialize(generator.DataSet, classList);
                    this.testDataGridView.DataSource = DataTableConverter.Convert(classList);
                }
            }
            catch (ClassifierError error)
            {
                this.StatusLabel.Text = error.Message;
            }

        }

        private void testDataButton_Click(object sender, EventArgs e)
        {
            InitializeOpenDialog("Design Data", false);

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                this.testDataTextBox.Text = openFileDialog.FileName;
            }
        }

        /*
                If no data is supplied, runs the Gaussian data on the desing data
                else, on the test data, and generates the confusion matrix         
        */ 
        private void classifyTestButton_Click(object sender, EventArgs e)
        {
            HideStuff();
            InitializeClassifier();
            try
            {
                if (dataOk)
                {
                    if (String.IsNullOrEmpty(this.testDataTextBox.Text))
                    {
                        classifier.Classify(classList);
                        this.confusionGridView.DataSource =
                            DataTableConverter.ConvertConfusionMatrix(
                            classifier.ConfusionMatrix, classifier.ClassList);

                    }
                    else
                    {
                        CSVReader reader = new CSVReader(this.testDataTextBox.Text);
                        Generator generator = new Generator(reader.RawData);
                        classifier.Classify(generator.DataSet, classList);
                        this.confusionGridView.DataSource =
                            DataTableConverter.ConvertConfusionMatrix(
                            classifier.ConfusionMatrix, classifier.ClassList);

                    }
                }
                //this.StatusLabel.Text = "Classification complete.";
            }
            catch (ClassifierError error)
            {
                this.StatusLabel.Text = error.Message;
            }
        }

        private void knnRadioButton_CheckedChanged(object sender, EventArgs e)
        {
             if (this.knnRadioButton.Checked == true)
            {
                   this.knnLabel.Visible = this.knnTextBox.Visible = true;
            }
             else
                 this.knnLabel.Visible = this.knnTextBox.Visible = false;
        }

        private void debugCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (this.debugCheckBox.Checked == true)
            {
                this.debugTextBox.Visible =  true;
            }
            else
                this.debugTextBox.Visible =  false;
        }

        private void InitializeClassifier()
        {
            if (this.gaussianRadioButton.Checked == true)
            {
                if (this.debugCheckBox.Checked == true)
                    classifier = new GaussianClassifier(this.debugTextBox.Text);
                else
                    classifier = new GaussianClassifier();
            }
            else if (this.minimumRadioButton.Checked == true)
            {
                if (this.debugCheckBox.Checked == true)
                    classifier = new MinDistanceClassifier(this.debugTextBox.Text);
                else
                    classifier = new MinDistanceClassifier();
            }

            else if (this.knnRadioButton.Checked == true)
            {
                if (String.IsNullOrEmpty(this.knnTextBox.Text))
                    throw new ClassifierError("Please select a valid value for k");
                int k;
                bool isNum = Int32.TryParse(this.knnTextBox.Text, out k);
                if (!isNum)
                {
                    throw new ClassifierError("Please select a valid value for k");
                }
                if (this.debugCheckBox.Checked == true)
                    classifier = new KNNClassifier(k, this.debugTextBox.Text);
                else
                    classifier = new KNNClassifier(k);
            }
            else
                throw new ClassifierError("This should not be possible");
        }

    }
}
