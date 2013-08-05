namespace ClassifierProgram
{
    partial class ClassifierForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle1 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle2 = new System.Windows.Forms.DataGridViewCellStyle();
            this.testDataLabel = new System.Windows.Forms.Label();
            this.dataFileLabel = new System.Windows.Forms.Label();
            this.designDataTextBox = new System.Windows.Forms.TextBox();
            this.loadButton = new System.Windows.Forms.Button();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.classifyButton = new System.Windows.Forms.Button();
            this.gaussianTestLabel = new System.Windows.Forms.Label();
            this.testDataTextBox = new System.Windows.Forms.TextBox();
            this.classifyTestButton = new System.Windows.Forms.Button();
            this.testDataButton = new System.Windows.Forms.Button();
            this.confusionGridView = new System.Windows.Forms.DataGridView();
            this.StatusLabel = new System.Windows.Forms.Label();
            this.testDataGridView = new System.Windows.Forms.DataGridView();
            this.classifierGroupBox = new System.Windows.Forms.GroupBox();
            this.knnRadioButton = new System.Windows.Forms.RadioButton();
            this.minimumRadioButton = new System.Windows.Forms.RadioButton();
            this.gaussianRadioButton = new System.Windows.Forms.RadioButton();
            this.knnTextBox = new System.Windows.Forms.TextBox();
            this.knnLabel = new System.Windows.Forms.Label();
            this.debugCheckBox = new System.Windows.Forms.CheckBox();
            this.debugTextBox = new System.Windows.Forms.TextBox();
            this.ConfusionMatrixLabel = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.confusionGridView)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.testDataGridView)).BeginInit();
            this.classifierGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // testDataLabel
            // 
            this.testDataLabel.AutoSize = true;
            this.testDataLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.testDataLabel.Location = new System.Drawing.Point(12, 271);
            this.testDataLabel.Name = "testDataLabel";
            this.testDataLabel.Size = new System.Drawing.Size(106, 16);
            this.testDataLabel.TabIndex = 4;
            this.testDataLabel.Text = "Test Data File";
            // 
            // dataFileLabel
            // 
            this.dataFileLabel.AutoSize = true;
            this.dataFileLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.dataFileLabel.Location = new System.Drawing.Point(12, 19);
            this.dataFileLabel.Name = "dataFileLabel";
            this.dataFileLabel.Size = new System.Drawing.Size(124, 16);
            this.dataFileLabel.TabIndex = 5;
            this.dataFileLabel.Text = "Design Data File";
            // 
            // designDataTextBox
            // 
            this.designDataTextBox.Location = new System.Drawing.Point(143, 18);
            this.designDataTextBox.Name = "designDataTextBox";
            this.designDataTextBox.Size = new System.Drawing.Size(509, 20);
            this.designDataTextBox.TabIndex = 6;
            // 
            // loadButton
            // 
            this.loadButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.loadButton.Location = new System.Drawing.Point(679, 18);
            this.loadButton.Name = "loadButton";
            this.loadButton.Size = new System.Drawing.Size(115, 27);
            this.loadButton.TabIndex = 8;
            this.loadButton.Text = "Load Design Data";
            this.loadButton.UseVisualStyleBackColor = true;
            this.loadButton.Click += new System.EventHandler(this.loadButton_Click);
            // 
            // openFileDialog
            // 
            this.openFileDialog.FileName = "openFileDialog";
            // 
            // classifyButton
            // 
            this.classifyButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.classifyButton.Location = new System.Drawing.Point(820, 19);
            this.classifyButton.Name = "classifyButton";
            this.classifyButton.Size = new System.Drawing.Size(118, 26);
            this.classifyButton.TabIndex = 9;
            this.classifyButton.Text = "Initialize Data";
            this.classifyButton.UseVisualStyleBackColor = true;
            this.classifyButton.Click += new System.EventHandler(this.classifyButton_Click);
            // 
            // gaussianTestLabel
            // 
            this.gaussianTestLabel.AutoSize = true;
            this.gaussianTestLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 20.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.gaussianTestLabel.Location = new System.Drawing.Point(564, 48);
            this.gaussianTestLabel.Name = "gaussianTestLabel";
            this.gaussianTestLabel.Size = new System.Drawing.Size(217, 31);
            this.gaussianTestLabel.TabIndex = 11;
            this.gaussianTestLabel.Text = "Class Statistics";
            // 
            // testDataTextBox
            // 
            this.testDataTextBox.Location = new System.Drawing.Point(124, 270);
            this.testDataTextBox.Name = "testDataTextBox";
            this.testDataTextBox.Size = new System.Drawing.Size(509, 20);
            this.testDataTextBox.TabIndex = 12;
            // 
            // classifyTestButton
            // 
            this.classifyTestButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.classifyTestButton.Location = new System.Drawing.Point(415, 380);
            this.classifyTestButton.Name = "classifyTestButton";
            this.classifyTestButton.Size = new System.Drawing.Size(169, 35);
            this.classifyTestButton.TabIndex = 13;
            this.classifyTestButton.Text = "Classify Test Data";
            this.classifyTestButton.UseVisualStyleBackColor = true;
            this.classifyTestButton.Click += new System.EventHandler(this.classifyTestButton_Click);
            // 
            // testDataButton
            // 
            this.testDataButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.testDataButton.Location = new System.Drawing.Point(641, 263);
            this.testDataButton.Name = "testDataButton";
            this.testDataButton.Size = new System.Drawing.Size(153, 33);
            this.testDataButton.TabIndex = 14;
            this.testDataButton.Text = "Load Test Data";
            this.testDataButton.UseVisualStyleBackColor = true;
            this.testDataButton.Click += new System.EventHandler(this.testDataButton_Click);
            // 
            // confusionGridView
            // 
            this.confusionGridView.AllowUserToAddRows = false;
            this.confusionGridView.AllowUserToDeleteRows = false;
            this.confusionGridView.AllowUserToResizeColumns = false;
            this.confusionGridView.AllowUserToResizeRows = false;
            this.confusionGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells;
            this.confusionGridView.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells;
            this.confusionGridView.BackgroundColor = System.Drawing.SystemColors.ControlLightLight;
            this.confusionGridView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.confusionGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            dataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Window;
            dataGridViewCellStyle1.Font = new System.Drawing.Font("Microsoft Sans Serif", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.ControlText;
            dataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.confusionGridView.DefaultCellStyle = dataGridViewCellStyle1;
            this.confusionGridView.EditMode = System.Windows.Forms.DataGridViewEditMode.EditProgrammatically;
            this.confusionGridView.Location = new System.Drawing.Point(630, 380);
            this.confusionGridView.MultiSelect = false;
            this.confusionGridView.Name = "confusionGridView";
            this.confusionGridView.ReadOnly = true;
            this.confusionGridView.RowHeadersVisible = false;
            this.confusionGridView.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders;
            this.confusionGridView.ScrollBars = System.Windows.Forms.ScrollBars.None;
            this.confusionGridView.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
            this.confusionGridView.ShowCellErrors = false;
            this.confusionGridView.ShowCellToolTips = false;
            this.confusionGridView.ShowEditingIcon = false;
            this.confusionGridView.ShowRowErrors = false;
            this.confusionGridView.Size = new System.Drawing.Size(426, 227);
            this.confusionGridView.TabIndex = 15;
            // 
            // StatusLabel
            // 
            this.StatusLabel.AutoSize = true;
            this.StatusLabel.Location = new System.Drawing.Point(-3, 165);
            this.StatusLabel.Name = "StatusLabel";
            this.StatusLabel.Size = new System.Drawing.Size(51, 16);
            this.StatusLabel.TabIndex = 16;
            this.StatusLabel.Text = "Status";
            // 
            // testDataGridView
            // 
            this.testDataGridView.AllowUserToAddRows = false;
            this.testDataGridView.AllowUserToDeleteRows = false;
            this.testDataGridView.AllowUserToResizeColumns = false;
            this.testDataGridView.AllowUserToResizeRows = false;
            this.testDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells;
            this.testDataGridView.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells;
            this.testDataGridView.BackgroundColor = System.Drawing.SystemColors.ControlLightLight;
            this.testDataGridView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.testDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            dataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Window;
            dataGridViewCellStyle2.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.ControlText;
            dataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.testDataGridView.DefaultCellStyle = dataGridViewCellStyle2;
            this.testDataGridView.EditMode = System.Windows.Forms.DataGridViewEditMode.EditProgrammatically;
            this.testDataGridView.Location = new System.Drawing.Point(15, 82);
            this.testDataGridView.MultiSelect = false;
            this.testDataGridView.Name = "testDataGridView";
            this.testDataGridView.ReadOnly = true;
            this.testDataGridView.RowHeadersVisible = false;
            this.testDataGridView.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders;
            this.testDataGridView.ScrollBars = System.Windows.Forms.ScrollBars.Horizontal;
            this.testDataGridView.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
            this.testDataGridView.ShowCellErrors = false;
            this.testDataGridView.ShowCellToolTips = false;
            this.testDataGridView.ShowEditingIcon = false;
            this.testDataGridView.ShowRowErrors = false;
            this.testDataGridView.Size = new System.Drawing.Size(1467, 175);
            this.testDataGridView.TabIndex = 17;
            // 
            // classifierGroupBox
            // 
            this.classifierGroupBox.Controls.Add(this.knnRadioButton);
            this.classifierGroupBox.Controls.Add(this.minimumRadioButton);
            this.classifierGroupBox.Controls.Add(this.gaussianRadioButton);
            this.classifierGroupBox.Controls.Add(this.StatusLabel);
            this.classifierGroupBox.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.classifierGroupBox.Location = new System.Drawing.Point(189, 325);
            this.classifierGroupBox.Name = "classifierGroupBox";
            this.classifierGroupBox.Size = new System.Drawing.Size(170, 178);
            this.classifierGroupBox.TabIndex = 18;
            this.classifierGroupBox.TabStop = false;
            this.classifierGroupBox.Text = "Classifiers";
            // 
            // knnRadioButton
            // 
            this.knnRadioButton.AutoSize = true;
            this.knnRadioButton.Location = new System.Drawing.Point(0, 84);
            this.knnRadioButton.Name = "knnRadioButton";
            this.knnRadioButton.Size = new System.Drawing.Size(57, 20);
            this.knnRadioButton.TabIndex = 2;
            this.knnRadioButton.TabStop = true;
            this.knnRadioButton.Text = "KNN";
            this.knnRadioButton.UseVisualStyleBackColor = true;
            this.knnRadioButton.CheckedChanged += new System.EventHandler(this.knnRadioButton_CheckedChanged);
            // 
            // minimumRadioButton
            // 
            this.minimumRadioButton.AutoSize = true;
            this.minimumRadioButton.Location = new System.Drawing.Point(0, 55);
            this.minimumRadioButton.Name = "minimumRadioButton";
            this.minimumRadioButton.Size = new System.Drawing.Size(151, 20);
            this.minimumRadioButton.TabIndex = 1;
            this.minimumRadioButton.TabStop = true;
            this.minimumRadioButton.Text = "Minimum Distance";
            this.minimumRadioButton.UseVisualStyleBackColor = true;
            // 
            // gaussianRadioButton
            // 
            this.gaussianRadioButton.AutoSize = true;
            this.gaussianRadioButton.Location = new System.Drawing.Point(0, 31);
            this.gaussianRadioButton.Name = "gaussianRadioButton";
            this.gaussianRadioButton.Size = new System.Drawing.Size(91, 20);
            this.gaussianRadioButton.TabIndex = 0;
            this.gaussianRadioButton.TabStop = true;
            this.gaussianRadioButton.Text = "Gaussian";
            this.gaussianRadioButton.UseVisualStyleBackColor = true;
            // 
            // knnTextBox
            // 
            this.knnTextBox.Location = new System.Drawing.Point(143, 409);
            this.knnTextBox.Name = "knnTextBox";
            this.knnTextBox.Size = new System.Drawing.Size(35, 20);
            this.knnTextBox.TabIndex = 19;
            this.knnTextBox.Visible = false;
            // 
            // knnLabel
            // 
            this.knnLabel.AutoSize = true;
            this.knnLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.knnLabel.Location = new System.Drawing.Point(12, 411);
            this.knnLabel.Name = "knnLabel";
            this.knnLabel.Size = new System.Drawing.Size(118, 16);
            this.knnLabel.TabIndex = 20;
            this.knnLabel.Text = "Choose K Value";
            this.knnLabel.Visible = false;
            // 
            // debugCheckBox
            // 
            this.debugCheckBox.AutoSize = true;
            this.debugCheckBox.Location = new System.Drawing.Point(1011, 4);
            this.debugCheckBox.Name = "debugCheckBox";
            this.debugCheckBox.Size = new System.Drawing.Size(88, 17);
            this.debugCheckBox.TabIndex = 21;
            this.debugCheckBox.Text = "Debug Mode";
            this.debugCheckBox.UseVisualStyleBackColor = true;
            this.debugCheckBox.CheckedChanged += new System.EventHandler(this.debugCheckBox_CheckedChanged);
            // 
            // debugTextBox
            // 
            this.debugTextBox.Location = new System.Drawing.Point(1011, 27);
            this.debugTextBox.Name = "debugTextBox";
            this.debugTextBox.Size = new System.Drawing.Size(313, 20);
            this.debugTextBox.TabIndex = 22;
            this.debugTextBox.Text = "D:\\\\classifier_debug.txt";
            this.debugTextBox.Visible = false;
            // 
            // ConfusionMatrixLabel
            // 
            this.ConfusionMatrixLabel.AutoSize = true;
            this.ConfusionMatrixLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 18F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ConfusionMatrixLabel.Location = new System.Drawing.Point(731, 338);
            this.ConfusionMatrixLabel.Name = "ConfusionMatrixLabel";
            this.ConfusionMatrixLabel.Size = new System.Drawing.Size(207, 29);
            this.ConfusionMatrixLabel.TabIndex = 23;
            this.ConfusionMatrixLabel.Text = "Confusion Matrix";
            // 
            // ClassifierForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1548, 660);
            this.Controls.Add(this.ConfusionMatrixLabel);
            this.Controls.Add(this.debugTextBox);
            this.Controls.Add(this.debugCheckBox);
            this.Controls.Add(this.knnLabel);
            this.Controls.Add(this.knnTextBox);
            this.Controls.Add(this.classifierGroupBox);
            this.Controls.Add(this.testDataGridView);
            this.Controls.Add(this.confusionGridView);
            this.Controls.Add(this.testDataButton);
            this.Controls.Add(this.classifyTestButton);
            this.Controls.Add(this.testDataTextBox);
            this.Controls.Add(this.gaussianTestLabel);
            this.Controls.Add(this.classifyButton);
            this.Controls.Add(this.loadButton);
            this.Controls.Add(this.designDataTextBox);
            this.Controls.Add(this.dataFileLabel);
            this.Controls.Add(this.testDataLabel);
            this.Name = "ClassifierForm";
            this.Text = "Pattern Recognition Program";
            this.Load += new System.EventHandler(this.classifierForm_Load);
            ((System.ComponentModel.ISupportInitialize)(this.confusionGridView)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.testDataGridView)).EndInit();
            this.classifierGroupBox.ResumeLayout(false);
            this.classifierGroupBox.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label testDataLabel;
        private System.Windows.Forms.Label dataFileLabel;
        private System.Windows.Forms.TextBox designDataTextBox;
        private System.Windows.Forms.Button loadButton;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.Button classifyButton;
        private System.Windows.Forms.Label gaussianTestLabel;
        private System.Windows.Forms.TextBox testDataTextBox;
        private System.Windows.Forms.Button classifyTestButton;
        private System.Windows.Forms.Button testDataButton;
        private System.Windows.Forms.DataGridView confusionGridView;
        private System.Windows.Forms.Label StatusLabel;
        private System.Windows.Forms.DataGridView testDataGridView;
        private System.Windows.Forms.GroupBox classifierGroupBox;
        private System.Windows.Forms.RadioButton minimumRadioButton;
        private System.Windows.Forms.RadioButton gaussianRadioButton;
        private System.Windows.Forms.RadioButton knnRadioButton;
        private System.Windows.Forms.TextBox knnTextBox;
        private System.Windows.Forms.Label knnLabel;
        private System.Windows.Forms.CheckBox debugCheckBox;
        private System.Windows.Forms.TextBox debugTextBox;
        private System.Windows.Forms.Label ConfusionMatrixLabel;
    }
}

