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
    public partial class DataInterpreter : Form
    {
        private bool dataOk;
        public DataInterpreter()
        {
            InitializeComponent();
        }

        private void LoadDataButton_Click(object sender, EventArgs e)
        {
            dataOk = false;
            InitializeOpenDialog("Data", false);

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                this.LoadDesignTextBox.Text = openFileDialog.FileName;
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
                    System.IO.Path.GetDirectoryName(openFileDialog.FileName);;
        }

        private void LaunchClassifier()
        {
            Application.Run(new ClassifierForm());
        }

        private void LoadClassifierButton_Click(object sender, EventArgs e)
        {
            ClassifierForm classForm = new ClassifierForm();
            this.Hide();
            classForm.ShowDialog();
            this.Show();
        }

        private void ScaledButton_Click(object sender, EventArgs e)
        {
            CSVReader reader = new CSVReader(this.LoadDesignTextBox.Text);
            Interpreter interpreter = new Interpreter(reader.RawData);
            this.BuyBusinessDataGrid.DataSource = DataTableConverter.ConvertStringList(interpreter.buyers.BusinessesTable);
            this.BuyCountryDataGrid.DataSource = DataTableConverter.ConvertStringList(interpreter.buyers.CountriesTable);
            this.NotBuyBusinessDataGrid.DataSource = DataTableConverter.ConvertStringList(interpreter.nonbuyers.BusinessesTable);
            this.NotBuyCountryDataGrid.DataSource = DataTableConverter.ConvertStringList(interpreter.nonbuyers.CountriesTable);
            this.ScaledBusinessDataGrid.DataSource = DataTableConverter.ConvertStringList(interpreter.scaledScores.BusinessesTable);
            this.ScaledCountriesDataGrid.DataSource = DataTableConverter.ConvertStringList(interpreter.scaledScores.CountriesTable);
        }

    }
}
