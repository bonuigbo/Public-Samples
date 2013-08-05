namespace EditDistanceCalculator
{
    partial class EditForm
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
            this.string1Label = new System.Windows.Forms.Label();
            this.string2Label = new System.Windows.Forms.Label();
            this.string1TextBox = new System.Windows.Forms.TextBox();
            this.string2TextBox = new System.Windows.Forms.TextBox();
            this.editDataGridView = new System.Windows.Forms.DataGridView();
            this.calculateButton = new System.Windows.Forms.Button();
            this.word1Label = new System.Windows.Forms.Label();
            this.word2Label = new System.Windows.Forms.Label();
            this.editDistanceLabel = new System.Windows.Forms.Label();
            this.alignmentLabel = new System.Windows.Forms.Label();
            this.alignDataGridView = new System.Windows.Forms.DataGridView();
            ((System.ComponentModel.ISupportInitialize)(this.editDataGridView)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.alignDataGridView)).BeginInit();
            this.SuspendLayout();
            // 
            // string1Label
            // 
            this.string1Label.AutoSize = true;
            this.string1Label.Location = new System.Drawing.Point(50, 28);
            this.string1Label.Name = "string1Label";
            this.string1Label.Size = new System.Drawing.Size(84, 13);
            this.string1Label.TabIndex = 0;
            this.string1Label.Text = "Enter Word One";
            // 
            // string2Label
            // 
            this.string2Label.AutoSize = true;
            this.string2Label.Location = new System.Drawing.Point(50, 65);
            this.string2Label.Name = "string2Label";
            this.string2Label.Size = new System.Drawing.Size(85, 13);
            this.string2Label.TabIndex = 1;
            this.string2Label.Text = "Enter Word Two";
            // 
            // string1TextBox
            // 
            this.string1TextBox.Location = new System.Drawing.Point(140, 25);
            this.string1TextBox.Name = "string1TextBox";
            this.string1TextBox.Size = new System.Drawing.Size(499, 20);
            this.string1TextBox.TabIndex = 2;
            // 
            // string2TextBox
            // 
            this.string2TextBox.Location = new System.Drawing.Point(140, 58);
            this.string2TextBox.Name = "string2TextBox";
            this.string2TextBox.Size = new System.Drawing.Size(499, 20);
            this.string2TextBox.TabIndex = 3;
            // 
            // editDataGridView
            // 
            this.editDataGridView.AccessibleRole = System.Windows.Forms.AccessibleRole.None;
            this.editDataGridView.AllowUserToAddRows = false;
            this.editDataGridView.AllowUserToDeleteRows = false;
            this.editDataGridView.AllowUserToResizeColumns = false;
            this.editDataGridView.AllowUserToResizeRows = false;
            this.editDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.DisplayedCellsExceptHeader;
            this.editDataGridView.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.DisplayedCellsExceptHeaders;
            this.editDataGridView.BackgroundColor = System.Drawing.SystemColors.Control;
            this.editDataGridView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.editDataGridView.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.Raised;
            this.editDataGridView.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.Disable;
            this.editDataGridView.ColumnHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.Single;
            this.editDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            this.editDataGridView.EditMode = System.Windows.Forms.DataGridViewEditMode.EditProgrammatically;
            this.editDataGridView.GridColor = System.Drawing.SystemColors.ControlLight;
            this.editDataGridView.Location = new System.Drawing.Point(53, 133);
            this.editDataGridView.MultiSelect = false;
            this.editDataGridView.Name = "editDataGridView";
            this.editDataGridView.ReadOnly = true;
            this.editDataGridView.RowHeadersWidth = 20;
            this.editDataGridView.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing;
            this.editDataGridView.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
            this.editDataGridView.ShowCellErrors = false;
            this.editDataGridView.ShowCellToolTips = false;
            this.editDataGridView.ShowEditingIcon = false;
            this.editDataGridView.ShowRowErrors = false;
            this.editDataGridView.Size = new System.Drawing.Size(422, 285);
            this.editDataGridView.TabIndex = 4;
            // 
            // calculateButton
            // 
            this.calculateButton.Location = new System.Drawing.Point(53, 94);
            this.calculateButton.Name = "calculateButton";
            this.calculateButton.Size = new System.Drawing.Size(139, 22);
            this.calculateButton.TabIndex = 5;
            this.calculateButton.Text = "Calculate Edit Distance";
            this.calculateButton.UseVisualStyleBackColor = true;
            this.calculateButton.Click += new System.EventHandler(this.calculateButton_Click);
            // 
            // word1Label
            // 
            this.word1Label.AutoSize = true;
            this.word1Label.Location = new System.Drawing.Point(500, 133);
            this.word1Label.Name = "word1Label";
            this.word1Label.Size = new System.Drawing.Size(48, 13);
            this.word1Label.TabIndex = 6;
            this.word1Label.Text = "Word 1: ";
            this.word1Label.Visible = false;
            // 
            // word2Label
            // 
            this.word2Label.AutoSize = true;
            this.word2Label.Location = new System.Drawing.Point(500, 157);
            this.word2Label.Name = "word2Label";
            this.word2Label.Size = new System.Drawing.Size(45, 13);
            this.word2Label.TabIndex = 7;
            this.word2Label.Text = "Word 2:";
            this.word2Label.Visible = false;
            // 
            // editDistanceLabel
            // 
            this.editDistanceLabel.AutoSize = true;
            this.editDistanceLabel.Location = new System.Drawing.Point(500, 311);
            this.editDistanceLabel.Name = "editDistanceLabel";
            this.editDistanceLabel.Size = new System.Drawing.Size(76, 13);
            this.editDistanceLabel.TabIndex = 8;
            this.editDistanceLabel.Text = "Edit Distance: ";
            this.editDistanceLabel.Visible = false;
            // 
            // alignmentLabel
            // 
            this.alignmentLabel.AutoSize = true;
            this.alignmentLabel.Location = new System.Drawing.Point(50, 434);
            this.alignmentLabel.Name = "alignmentLabel";
            this.alignmentLabel.Size = new System.Drawing.Size(56, 13);
            this.alignmentLabel.TabIndex = 9;
            this.alignmentLabel.Text = "Alignment:";
            this.alignmentLabel.Visible = false;
            // 
            // alignDataGridView
            // 
            this.alignDataGridView.AllowUserToAddRows = false;
            this.alignDataGridView.AllowUserToDeleteRows = false;
            this.alignDataGridView.AllowUserToResizeColumns = false;
            this.alignDataGridView.AllowUserToResizeRows = false;
            this.alignDataGridView.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCellsExceptHeader;
            this.alignDataGridView.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCellsExceptHeaders;
            this.alignDataGridView.BackgroundColor = System.Drawing.SystemColors.Control;
            this.alignDataGridView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.alignDataGridView.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None;
            this.alignDataGridView.ColumnHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.None;
            this.alignDataGridView.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.alignDataGridView.EditMode = System.Windows.Forms.DataGridViewEditMode.EditProgrammatically;
            this.alignDataGridView.Location = new System.Drawing.Point(53, 466);
            this.alignDataGridView.MultiSelect = false;
            this.alignDataGridView.Name = "alignDataGridView";
            this.alignDataGridView.Size = new System.Drawing.Size(422, 72);
            this.alignDataGridView.TabIndex = 13;
            // 
            // EditForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(733, 602);
            this.Controls.Add(this.alignDataGridView);
            this.Controls.Add(this.alignmentLabel);
            this.Controls.Add(this.editDistanceLabel);
            this.Controls.Add(this.word2Label);
            this.Controls.Add(this.word1Label);
            this.Controls.Add(this.calculateButton);
            this.Controls.Add(this.editDataGridView);
            this.Controls.Add(this.string2TextBox);
            this.Controls.Add(this.string1TextBox);
            this.Controls.Add(this.string2Label);
            this.Controls.Add(this.string1Label);
            this.Name = "EditForm";
            this.Text = "Edit Distance Calculator";
            ((System.ComponentModel.ISupportInitialize)(this.editDataGridView)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.alignDataGridView)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label string1Label;
        private System.Windows.Forms.Label string2Label;
        private System.Windows.Forms.TextBox string1TextBox;
        private System.Windows.Forms.TextBox string2TextBox;
        private System.Windows.Forms.DataGridView editDataGridView;
        private System.Windows.Forms.Button calculateButton;
        private System.Windows.Forms.Label word1Label;
        private System.Windows.Forms.Label word2Label;
        private System.Windows.Forms.Label editDistanceLabel;
        private System.Windows.Forms.Label alignmentLabel;
        private System.Windows.Forms.DataGridView alignDataGridView;
    }
}

