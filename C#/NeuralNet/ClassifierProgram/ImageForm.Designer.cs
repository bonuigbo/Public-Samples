namespace NeuralNet
{
    partial class ImageForm
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
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle3 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle4 = new System.Windows.Forms.DataGridViewCellStyle();
            this.picture_box_drawing = new System.Windows.Forms.PictureBox();
            this.button_clear = new System.Windows.Forms.Button();
            this.button_save = new System.Windows.Forms.Button();
            this.text_box_pen_width = new System.Windows.Forms.TextBox();
            this.label_penwidth = new System.Windows.Forms.Label();
            this.groupBoxRadioButtons = new System.Windows.Forms.GroupBox();
            this.radioButtonA = new System.Windows.Forms.RadioButton();
            this.radioButtonB = new System.Windows.Forms.RadioButton();
            this.radioButtonC = new System.Windows.Forms.RadioButton();
            this.radioButtonD = new System.Windows.Forms.RadioButton();
            this.radioButtonE = new System.Windows.Forms.RadioButton();
            this.radioButtonG = new System.Windows.Forms.RadioButton();
            this.radioButtonF = new System.Windows.Forms.RadioButton();
            this.buttonInitialize = new System.Windows.Forms.Button();
            this.buttonTrain = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.textBoxLearningRate = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.textBoxLayerStructure = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.textBoxEpocjhs = new System.Windows.Forms.TextBox();
            this.textBoxClassified = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.checkBoxLog = new System.Windows.Forms.CheckBox();
            this.buttonTest = new System.Windows.Forms.Button();
            this.buttonImageToData = new System.Windows.Forms.Button();
            this.radioButtonTraining = new System.Windows.Forms.RadioButton();
            this.radioButtonTest = new System.Windows.Forms.RadioButton();
            this.groupBoxDataType = new System.Windows.Forms.GroupBox();
            this.groupBoxNetwork = new System.Windows.Forms.GroupBox();
            this.radioButtonHRM = new System.Windows.Forms.RadioButton();
            this.radioButtonBackProp = new System.Windows.Forms.RadioButton();
            this.radioButtonART = new System.Windows.Forms.RadioButton();
            this.buttonTestImage = new System.Windows.Forms.Button();
            this.dataGridViewConfusion = new System.Windows.Forms.DataGridView();
            this.textBoxBiasFactor = new System.Windows.Forms.TextBox();
            this.textBoxWeightsFactor = new System.Windows.Forms.TextBox();
            this.labelWeights = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.textBoxRescale = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.labelTotalEpochs = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.radioButtonBinary = new System.Windows.Forms.RadioButton();
            this.radioButtonBipolar = new System.Windows.Forms.RadioButton();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.radioButtonRandom = new System.Windows.Forms.RadioButton();
            this.radioButtonDirect = new System.Windows.Forms.RadioButton();
            ((System.ComponentModel.ISupportInitialize)(this.picture_box_drawing)).BeginInit();
            this.groupBoxRadioButtons.SuspendLayout();
            this.groupBoxDataType.SuspendLayout();
            this.groupBoxNetwork.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewConfusion)).BeginInit();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // picture_box_drawing
            // 
            this.picture_box_drawing.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.picture_box_drawing.Location = new System.Drawing.Point(651, 34);
            this.picture_box_drawing.Name = "picture_box_drawing";
            this.picture_box_drawing.Size = new System.Drawing.Size(200, 200);
            this.picture_box_drawing.TabIndex = 15;
            this.picture_box_drawing.TabStop = false;
            // 
            // button_clear
            // 
            this.button_clear.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.button_clear.Location = new System.Drawing.Point(869, 34);
            this.button_clear.Name = "button_clear";
            this.button_clear.Size = new System.Drawing.Size(111, 41);
            this.button_clear.TabIndex = 16;
            this.button_clear.Text = "Clear";
            this.button_clear.UseVisualStyleBackColor = true;
            this.button_clear.Click += new System.EventHandler(this.button_clear_Click);
            // 
            // button_save
            // 
            this.button_save.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.button_save.Location = new System.Drawing.Point(869, 89);
            this.button_save.Name = "button_save";
            this.button_save.Size = new System.Drawing.Size(111, 41);
            this.button_save.TabIndex = 17;
            this.button_save.Text = "Save";
            this.button_save.UseVisualStyleBackColor = true;
            this.button_save.Click += new System.EventHandler(this.button_save_Click);
            // 
            // text_box_pen_width
            // 
            this.text_box_pen_width.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.text_box_pen_width.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.text_box_pen_width.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.text_box_pen_width.Location = new System.Drawing.Point(1009, 345);
            this.text_box_pen_width.Name = "text_box_pen_width";
            this.text_box_pen_width.Size = new System.Drawing.Size(106, 29);
            this.text_box_pen_width.TabIndex = 18;
            this.text_box_pen_width.Text = "10";
            this.text_box_pen_width.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label_penwidth
            // 
            this.label_penwidth.AutoSize = true;
            this.label_penwidth.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label_penwidth.Location = new System.Drawing.Point(1024, 320);
            this.label_penwidth.Name = "label_penwidth";
            this.label_penwidth.Size = new System.Drawing.Size(91, 20);
            this.label_penwidth.TabIndex = 19;
            this.label_penwidth.Text = "Pen Width";
            // 
            // groupBoxRadioButtons
            // 
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonA);
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonB);
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonC);
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonD);
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonE);
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonG);
            this.groupBoxRadioButtons.Controls.Add(this.radioButtonF);
            this.groupBoxRadioButtons.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBoxRadioButtons.Location = new System.Drawing.Point(1034, 29);
            this.groupBoxRadioButtons.Name = "groupBoxRadioButtons";
            this.groupBoxRadioButtons.Size = new System.Drawing.Size(81, 185);
            this.groupBoxRadioButtons.TabIndex = 20;
            this.groupBoxRadioButtons.TabStop = false;
            this.groupBoxRadioButtons.Text = "Letter";
            // 
            // radioButtonA
            // 
            this.radioButtonA.AutoSize = true;
            this.radioButtonA.Checked = true;
            this.radioButtonA.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonA.Location = new System.Drawing.Point(-1, 25);
            this.radioButtonA.Name = "radioButtonA";
            this.radioButtonA.Size = new System.Drawing.Size(39, 24);
            this.radioButtonA.TabIndex = 21;
            this.radioButtonA.TabStop = true;
            this.radioButtonA.Text = "A";
            this.radioButtonA.UseVisualStyleBackColor = true;
            // 
            // radioButtonB
            // 
            this.radioButtonB.AutoSize = true;
            this.radioButtonB.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonB.Location = new System.Drawing.Point(0, 45);
            this.radioButtonB.Name = "radioButtonB";
            this.radioButtonB.Size = new System.Drawing.Size(39, 24);
            this.radioButtonB.TabIndex = 22;
            this.radioButtonB.Text = "B";
            this.radioButtonB.UseVisualStyleBackColor = true;
            // 
            // radioButtonC
            // 
            this.radioButtonC.AutoSize = true;
            this.radioButtonC.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonC.Location = new System.Drawing.Point(0, 68);
            this.radioButtonC.Name = "radioButtonC";
            this.radioButtonC.Size = new System.Drawing.Size(39, 24);
            this.radioButtonC.TabIndex = 23;
            this.radioButtonC.Text = "C";
            this.radioButtonC.UseVisualStyleBackColor = true;
            // 
            // radioButtonD
            // 
            this.radioButtonD.AutoSize = true;
            this.radioButtonD.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonD.Location = new System.Drawing.Point(0, 91);
            this.radioButtonD.Name = "radioButtonD";
            this.radioButtonD.Size = new System.Drawing.Size(40, 24);
            this.radioButtonD.TabIndex = 24;
            this.radioButtonD.Text = "D";
            this.radioButtonD.UseVisualStyleBackColor = true;
            // 
            // radioButtonE
            // 
            this.radioButtonE.AutoSize = true;
            this.radioButtonE.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonE.Location = new System.Drawing.Point(0, 114);
            this.radioButtonE.Name = "radioButtonE";
            this.radioButtonE.Size = new System.Drawing.Size(39, 24);
            this.radioButtonE.TabIndex = 22;
            this.radioButtonE.Text = "E";
            this.radioButtonE.UseVisualStyleBackColor = true;
            // 
            // radioButtonG
            // 
            this.radioButtonG.AutoSize = true;
            this.radioButtonG.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonG.Location = new System.Drawing.Point(0, 160);
            this.radioButtonG.Name = "radioButtonG";
            this.radioButtonG.Size = new System.Drawing.Size(41, 24);
            this.radioButtonG.TabIndex = 26;
            this.radioButtonG.Text = "G";
            this.radioButtonG.UseVisualStyleBackColor = true;
            // 
            // radioButtonF
            // 
            this.radioButtonF.AutoSize = true;
            this.radioButtonF.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonF.Location = new System.Drawing.Point(0, 137);
            this.radioButtonF.Name = "radioButtonF";
            this.radioButtonF.Size = new System.Drawing.Size(38, 24);
            this.radioButtonF.TabIndex = 25;
            this.radioButtonF.Text = "F";
            this.radioButtonF.UseVisualStyleBackColor = true;
            // 
            // buttonInitialize
            // 
            this.buttonInitialize.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.buttonInitialize.Location = new System.Drawing.Point(165, 436);
            this.buttonInitialize.Name = "buttonInitialize";
            this.buttonInitialize.Size = new System.Drawing.Size(204, 41);
            this.buttonInitialize.TabIndex = 28;
            this.buttonInitialize.Text = "Initialize Network";
            this.buttonInitialize.UseVisualStyleBackColor = true;
            this.buttonInitialize.Click += new System.EventHandler(this.buttonInitialize_Click);
            // 
            // buttonTrain
            // 
            this.buttonTrain.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.buttonTrain.Location = new System.Drawing.Point(382, 436);
            this.buttonTrain.Name = "buttonTrain";
            this.buttonTrain.Size = new System.Drawing.Size(204, 41);
            this.buttonTrain.TabIndex = 29;
            this.buttonTrain.Text = "Train Network";
            this.buttonTrain.UseVisualStyleBackColor = true;
            this.buttonTrain.Click += new System.EventHandler(this.buttonTrain_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(13, 222);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(211, 20);
            this.label1.TabIndex = 31;
            this.label1.Text = "Learning Rate / Vigilance";
            // 
            // textBoxLearningRate
            // 
            this.textBoxLearningRate.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxLearningRate.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxLearningRate.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxLearningRate.Location = new System.Drawing.Point(16, 251);
            this.textBoxLearningRate.Name = "textBoxLearningRate";
            this.textBoxLearningRate.Size = new System.Drawing.Size(63, 29);
            this.textBoxLearningRate.TabIndex = 32;
            this.textBoxLearningRate.Text = "0.9";
            this.textBoxLearningRate.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(19, 292);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(231, 20);
            this.label2.TabIndex = 33;
            this.label2.Text = "Hidden Layer / ART L Value";
            // 
            // textBoxLayerStructure
            // 
            this.textBoxLayerStructure.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxLayerStructure.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxLayerStructure.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxLayerStructure.Location = new System.Drawing.Point(16, 324);
            this.textBoxLayerStructure.Name = "textBoxLayerStructure";
            this.textBoxLayerStructure.Size = new System.Drawing.Size(102, 29);
            this.textBoxLayerStructure.TabIndex = 34;
            this.textBoxLayerStructure.Text = "15";
            this.textBoxLayerStructure.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(378, 350);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(113, 20);
            this.label3.TabIndex = 35;
            this.label3.Text = "Epoch Count";
            // 
            // textBoxEpocjhs
            // 
            this.textBoxEpocjhs.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxEpocjhs.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxEpocjhs.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxEpocjhs.Location = new System.Drawing.Point(383, 378);
            this.textBoxEpocjhs.Name = "textBoxEpocjhs";
            this.textBoxEpocjhs.Size = new System.Drawing.Size(102, 29);
            this.textBoxEpocjhs.TabIndex = 36;
            this.textBoxEpocjhs.Text = "1";
            this.textBoxEpocjhs.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // textBoxClassified
            // 
            this.textBoxClassified.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxClassified.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxClassified.Font = new System.Drawing.Font("Microsoft Sans Serif", 120F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxClassified.Location = new System.Drawing.Point(419, 45);
            this.textBoxClassified.Name = "textBoxClassified";
            this.textBoxClassified.Size = new System.Drawing.Size(200, 189);
            this.textBoxClassified.TabIndex = 37;
            this.textBoxClassified.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(414, 9);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(172, 25);
            this.label4.TabIndex = 38;
            this.label4.Text = "Guessed Value";
            // 
            // checkBoxLog
            // 
            this.checkBoxLog.AutoSize = true;
            this.checkBoxLog.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.checkBoxLog.Location = new System.Drawing.Point(18, 177);
            this.checkBoxLog.Name = "checkBoxLog";
            this.checkBoxLog.Size = new System.Drawing.Size(173, 24);
            this.checkBoxLog.TabIndex = 39;
            this.checkBoxLog.Text = "Generate Log File";
            this.checkBoxLog.UseVisualStyleBackColor = true;
            // 
            // buttonTest
            // 
            this.buttonTest.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.buttonTest.Location = new System.Drawing.Point(607, 436);
            this.buttonTest.Name = "buttonTest";
            this.buttonTest.Size = new System.Drawing.Size(204, 41);
            this.buttonTest.TabIndex = 40;
            this.buttonTest.Text = "Test Network";
            this.buttonTest.UseVisualStyleBackColor = true;
            this.buttonTest.Click += new System.EventHandler(this.buttonTest_Click);
            // 
            // buttonImageToData
            // 
            this.buttonImageToData.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.buttonImageToData.Location = new System.Drawing.Point(869, 139);
            this.buttonImageToData.Name = "buttonImageToData";
            this.buttonImageToData.Size = new System.Drawing.Size(111, 44);
            this.buttonImageToData.TabIndex = 41;
            this.buttonImageToData.Text = "Convert";
            this.buttonImageToData.UseVisualStyleBackColor = true;
            this.buttonImageToData.Click += new System.EventHandler(this.buttonImageToData_Click);
            // 
            // radioButtonTraining
            // 
            this.radioButtonTraining.AutoSize = true;
            this.radioButtonTraining.Checked = true;
            this.radioButtonTraining.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonTraining.Location = new System.Drawing.Point(0, 25);
            this.radioButtonTraining.Name = "radioButtonTraining";
            this.radioButtonTraining.Size = new System.Drawing.Size(91, 24);
            this.radioButtonTraining.TabIndex = 42;
            this.radioButtonTraining.TabStop = true;
            this.radioButtonTraining.Text = "Training";
            this.radioButtonTraining.UseVisualStyleBackColor = true;
            // 
            // radioButtonTest
            // 
            this.radioButtonTest.AutoSize = true;
            this.radioButtonTest.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonTest.Location = new System.Drawing.Point(0, 49);
            this.radioButtonTest.Name = "radioButtonTest";
            this.radioButtonTest.Size = new System.Drawing.Size(62, 24);
            this.radioButtonTest.TabIndex = 43;
            this.radioButtonTest.Text = "Test";
            this.radioButtonTest.UseVisualStyleBackColor = true;
            // 
            // groupBoxDataType
            // 
            this.groupBoxDataType.Controls.Add(this.radioButtonTraining);
            this.groupBoxDataType.Controls.Add(this.radioButtonTest);
            this.groupBoxDataType.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBoxDataType.Location = new System.Drawing.Point(869, 275);
            this.groupBoxDataType.Name = "groupBoxDataType";
            this.groupBoxDataType.Size = new System.Drawing.Size(111, 71);
            this.groupBoxDataType.TabIndex = 44;
            this.groupBoxDataType.TabStop = false;
            this.groupBoxDataType.Text = "Data Type";
            // 
            // groupBoxNetwork
            // 
            this.groupBoxNetwork.Controls.Add(this.radioButtonHRM);
            this.groupBoxNetwork.Controls.Add(this.radioButtonBackProp);
            this.groupBoxNetwork.Controls.Add(this.radioButtonART);
            this.groupBoxNetwork.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBoxNetwork.Location = new System.Drawing.Point(23, 37);
            this.groupBoxNetwork.Name = "groupBoxNetwork";
            this.groupBoxNetwork.Size = new System.Drawing.Size(171, 107);
            this.groupBoxNetwork.TabIndex = 46;
            this.groupBoxNetwork.TabStop = false;
            this.groupBoxNetwork.Text = "Network";
            // 
            // radioButtonHRM
            // 
            this.radioButtonHRM.AutoSize = true;
            this.radioButtonHRM.Checked = true;
            this.radioButtonHRM.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonHRM.Location = new System.Drawing.Point(3, 25);
            this.radioButtonHRM.Name = "radioButtonHRM";
            this.radioButtonHRM.Size = new System.Drawing.Size(124, 24);
            this.radioButtonHRM.TabIndex = 44;
            this.radioButtonHRM.TabStop = true;
            this.radioButtonHRM.Text = "HRM Hebbs";
            this.radioButtonHRM.UseVisualStyleBackColor = true;
            // 
            // radioButtonBackProp
            // 
            this.radioButtonBackProp.AutoSize = true;
            this.radioButtonBackProp.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonBackProp.Location = new System.Drawing.Point(3, 50);
            this.radioButtonBackProp.Name = "radioButtonBackProp";
            this.radioButtonBackProp.Size = new System.Drawing.Size(169, 24);
            this.radioButtonBackProp.TabIndex = 42;
            this.radioButtonBackProp.Text = "Back Propogation";
            this.radioButtonBackProp.UseVisualStyleBackColor = true;
            // 
            // radioButtonART
            // 
            this.radioButtonART.AutoSize = true;
            this.radioButtonART.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonART.Location = new System.Drawing.Point(3, 72);
            this.radioButtonART.Name = "radioButtonART";
            this.radioButtonART.Size = new System.Drawing.Size(62, 24);
            this.radioButtonART.TabIndex = 43;
            this.radioButtonART.Text = "ART";
            this.radioButtonART.UseVisualStyleBackColor = true;
            // 
            // buttonTestImage
            // 
            this.buttonTestImage.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.buttonTestImage.Location = new System.Drawing.Point(826, 438);
            this.buttonTestImage.Name = "buttonTestImage";
            this.buttonTestImage.Size = new System.Drawing.Size(225, 39);
            this.buttonTestImage.TabIndex = 48;
            this.buttonTestImage.Text = "Test Current Image";
            this.buttonTestImage.UseVisualStyleBackColor = true;
            this.buttonTestImage.Click += new System.EventHandler(this.buttonScale_Click);
            // 
            // dataGridViewConfusion
            // 
            this.dataGridViewConfusion.AllowUserToAddRows = false;
            this.dataGridViewConfusion.AllowUserToDeleteRows = false;
            dataGridViewCellStyle1.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle1.NullValue = null;
            this.dataGridViewConfusion.AlternatingRowsDefaultCellStyle = dataGridViewCellStyle1;
            this.dataGridViewConfusion.BackgroundColor = System.Drawing.SystemColors.Control;
            dataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Control;
            dataGridViewCellStyle2.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.WindowText;
            dataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.True;
            this.dataGridViewConfusion.ColumnHeadersDefaultCellStyle = dataGridViewCellStyle2;
            this.dataGridViewConfusion.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridViewConfusion.ColumnHeadersVisible = false;
            dataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle3.BackColor = System.Drawing.SystemColors.Window;
            dataGridViewCellStyle3.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle3.ForeColor = System.Drawing.SystemColors.ControlText;
            dataGridViewCellStyle3.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle3.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle3.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.dataGridViewConfusion.DefaultCellStyle = dataGridViewCellStyle3;
            this.dataGridViewConfusion.GridColor = System.Drawing.SystemColors.Control;
            this.dataGridViewConfusion.Location = new System.Drawing.Point(165, 483);
            this.dataGridViewConfusion.Name = "dataGridViewConfusion";
            this.dataGridViewConfusion.ReadOnly = true;
            this.dataGridViewConfusion.RowHeadersVisible = false;
            this.dataGridViewConfusion.RowHeadersWidth = 20;
            dataGridViewCellStyle4.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.dataGridViewConfusion.RowsDefaultCellStyle = dataGridViewCellStyle4;
            this.dataGridViewConfusion.Size = new System.Drawing.Size(931, 209);
            this.dataGridViewConfusion.TabIndex = 49;
            // 
            // textBoxBiasFactor
            // 
            this.textBoxBiasFactor.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxBiasFactor.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxBiasFactor.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxBiasFactor.Location = new System.Drawing.Point(16, 463);
            this.textBoxBiasFactor.Name = "textBoxBiasFactor";
            this.textBoxBiasFactor.Size = new System.Drawing.Size(102, 29);
            this.textBoxBiasFactor.TabIndex = 50;
            this.textBoxBiasFactor.Text = "0.2";
            this.textBoxBiasFactor.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // textBoxWeightsFactor
            // 
            this.textBoxWeightsFactor.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxWeightsFactor.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxWeightsFactor.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxWeightsFactor.Location = new System.Drawing.Point(15, 400);
            this.textBoxWeightsFactor.Name = "textBoxWeightsFactor";
            this.textBoxWeightsFactor.Size = new System.Drawing.Size(102, 29);
            this.textBoxWeightsFactor.TabIndex = 51;
            this.textBoxWeightsFactor.Text = "0.2";
            this.textBoxWeightsFactor.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // labelWeights
            // 
            this.labelWeights.AutoSize = true;
            this.labelWeights.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelWeights.Location = new System.Drawing.Point(18, 371);
            this.labelWeights.Name = "labelWeights";
            this.labelWeights.Size = new System.Drawing.Size(131, 20);
            this.labelWeights.TabIndex = 52;
            this.labelWeights.Text = "Weights Factor";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(18, 440);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(120, 20);
            this.label5.TabIndex = 53;
            this.label5.Text = "Biases Factor";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(998, 234);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(131, 20);
            this.label6.TabIndex = 54;
            this.label6.Text = "Rescale Factor";
            // 
            // textBoxRescale
            // 
            this.textBoxRescale.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.textBoxRescale.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxRescale.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxRescale.Location = new System.Drawing.Point(1013, 268);
            this.textBoxRescale.Name = "textBoxRescale";
            this.textBoxRescale.Size = new System.Drawing.Size(102, 29);
            this.textBoxRescale.TabIndex = 55;
            this.textBoxRescale.Text = "10";
            this.textBoxRescale.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label7.Location = new System.Drawing.Point(376, 413);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(124, 20);
            this.label7.TabIndex = 56;
            this.label7.Text = "Total Epochs: ";
            // 
            // labelTotalEpochs
            // 
            this.labelTotalEpochs.AutoSize = true;
            this.labelTotalEpochs.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelTotalEpochs.Location = new System.Drawing.Point(506, 413);
            this.labelTotalEpochs.Name = "labelTotalEpochs";
            this.labelTotalEpochs.Size = new System.Drawing.Size(19, 20);
            this.labelTotalEpochs.TabIndex = 57;
            this.labelTotalEpochs.Text = "0";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.radioButtonBinary);
            this.groupBox1.Controls.Add(this.radioButtonBipolar);
            this.groupBox1.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox1.Location = new System.Drawing.Point(869, 195);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(123, 71);
            this.groupBox1.TabIndex = 58;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Vector Type";
            // 
            // radioButtonBinary
            // 
            this.radioButtonBinary.AutoSize = true;
            this.radioButtonBinary.Checked = true;
            this.radioButtonBinary.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonBinary.Location = new System.Drawing.Point(6, 23);
            this.radioButtonBinary.Name = "radioButtonBinary";
            this.radioButtonBinary.Size = new System.Drawing.Size(77, 24);
            this.radioButtonBinary.TabIndex = 42;
            this.radioButtonBinary.TabStop = true;
            this.radioButtonBinary.Text = "Binary";
            this.radioButtonBinary.UseVisualStyleBackColor = true;
            // 
            // radioButtonBipolar
            // 
            this.radioButtonBipolar.AutoSize = true;
            this.radioButtonBipolar.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonBipolar.Location = new System.Drawing.Point(0, 49);
            this.radioButtonBipolar.Name = "radioButtonBipolar";
            this.radioButtonBipolar.Size = new System.Drawing.Size(83, 24);
            this.radioButtonBipolar.TabIndex = 43;
            this.radioButtonBipolar.Text = "Bipolar";
            this.radioButtonBipolar.UseVisualStyleBackColor = true;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.radioButtonRandom);
            this.groupBox2.Controls.Add(this.radioButtonDirect);
            this.groupBox2.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBox2.Location = new System.Drawing.Point(18, 496);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(130, 71);
            this.groupBox2.TabIndex = 59;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Scale Type";
            // 
            // radioButtonRandom
            // 
            this.radioButtonRandom.AutoSize = true;
            this.radioButtonRandom.Checked = true;
            this.radioButtonRandom.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonRandom.Location = new System.Drawing.Point(0, 25);
            this.radioButtonRandom.Name = "radioButtonRandom";
            this.radioButtonRandom.Size = new System.Drawing.Size(94, 24);
            this.radioButtonRandom.TabIndex = 42;
            this.radioButtonRandom.TabStop = true;
            this.radioButtonRandom.Text = "Random";
            this.radioButtonRandom.UseVisualStyleBackColor = true;
            // 
            // radioButtonDirect
            // 
            this.radioButtonDirect.AutoSize = true;
            this.radioButtonDirect.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.radioButtonDirect.Location = new System.Drawing.Point(0, 49);
            this.radioButtonDirect.Name = "radioButtonDirect";
            this.radioButtonDirect.Size = new System.Drawing.Size(75, 24);
            this.radioButtonDirect.TabIndex = 43;
            this.radioButtonDirect.Text = "Direct";
            this.radioButtonDirect.UseVisualStyleBackColor = true;
            // 
            // ImageForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Control;
            this.ClientSize = new System.Drawing.Size(1141, 702);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.labelTotalEpochs);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.textBoxRescale);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.labelWeights);
            this.Controls.Add(this.textBoxWeightsFactor);
            this.Controls.Add(this.textBoxBiasFactor);
            this.Controls.Add(this.dataGridViewConfusion);
            this.Controls.Add(this.buttonTestImage);
            this.Controls.Add(this.groupBoxNetwork);
            this.Controls.Add(this.groupBoxDataType);
            this.Controls.Add(this.buttonImageToData);
            this.Controls.Add(this.buttonTest);
            this.Controls.Add(this.checkBoxLog);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.textBoxClassified);
            this.Controls.Add(this.textBoxEpocjhs);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.textBoxLayerStructure);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.textBoxLearningRate);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.buttonTrain);
            this.Controls.Add(this.buttonInitialize);
            this.Controls.Add(this.groupBoxRadioButtons);
            this.Controls.Add(this.label_penwidth);
            this.Controls.Add(this.text_box_pen_width);
            this.Controls.Add(this.button_save);
            this.Controls.Add(this.button_clear);
            this.Controls.Add(this.picture_box_drawing);
            this.Name = "ImageForm";
            this.Text = "Neural Network Tool";
            ((System.ComponentModel.ISupportInitialize)(this.picture_box_drawing)).EndInit();
            this.groupBoxRadioButtons.ResumeLayout(false);
            this.groupBoxRadioButtons.PerformLayout();
            this.groupBoxDataType.ResumeLayout(false);
            this.groupBoxDataType.PerformLayout();
            this.groupBoxNetwork.ResumeLayout(false);
            this.groupBoxNetwork.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewConfusion)).EndInit();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.PictureBox picture_box_drawing;
        private System.Windows.Forms.Button button_clear;
        private System.Windows.Forms.Button button_save;
        private System.Windows.Forms.TextBox text_box_pen_width;
        private System.Windows.Forms.Label label_penwidth;
        private System.Windows.Forms.GroupBox groupBoxRadioButtons;
        private System.Windows.Forms.RadioButton radioButtonA;
        private System.Windows.Forms.RadioButton radioButtonB;
        private System.Windows.Forms.RadioButton radioButtonC;
        private System.Windows.Forms.RadioButton radioButtonD;
        private System.Windows.Forms.RadioButton radioButtonE;
        private System.Windows.Forms.RadioButton radioButtonF;
        private System.Windows.Forms.RadioButton radioButtonG;
        private System.Windows.Forms.Button buttonInitialize;
        private System.Windows.Forms.Button buttonTrain;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBoxLearningRate;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox textBoxLayerStructure;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox textBoxEpocjhs;
        private System.Windows.Forms.TextBox textBoxClassified;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.CheckBox checkBoxLog;
        private System.Windows.Forms.Button buttonTest;
        private System.Windows.Forms.Button buttonImageToData;
        private System.Windows.Forms.RadioButton radioButtonTraining;
        private System.Windows.Forms.RadioButton radioButtonTest;
        private System.Windows.Forms.GroupBox groupBoxDataType;
        private System.Windows.Forms.GroupBox groupBoxNetwork;
        private System.Windows.Forms.RadioButton radioButtonBackProp;
        private System.Windows.Forms.RadioButton radioButtonART;
        private System.Windows.Forms.Button buttonTestImage;
        private System.Windows.Forms.DataGridView dataGridViewConfusion;
        private System.Windows.Forms.TextBox textBoxBiasFactor;
        private System.Windows.Forms.TextBox textBoxWeightsFactor;
        private System.Windows.Forms.Label labelWeights;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox textBoxRescale;
        private System.Windows.Forms.RadioButton radioButtonHRM;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label labelTotalEpochs;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.RadioButton radioButtonBinary;
        private System.Windows.Forms.RadioButton radioButtonBipolar;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RadioButton radioButtonRandom;
        private System.Windows.Forms.RadioButton radioButtonDirect;
    }
}