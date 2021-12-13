namespace MILibrary
{
    partial class LoginForm
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
            this.serverTypeComboBox = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.accessPathTextBox = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.openFileDialogBtn = new System.Windows.Forms.Button();
            this.serverNameTextBox = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.useSQLAuthCheckBox = new System.Windows.Forms.CheckBox();
            this.label4 = new System.Windows.Forms.Label();
            this.userTextBox = new System.Windows.Forms.TextBox();
            this.pwdTextBox = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.connectBtn = new System.Windows.Forms.Button();
            this.dbNameTextBox = new System.Windows.Forms.TextBox();
            this.useWHsettingsBtn = new System.Windows.Forms.Button();
            this.useLightSettingsBtn = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // serverTypeComboBox
            // 
            this.serverTypeComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.serverTypeComboBox.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.serverTypeComboBox.Items.AddRange(new object[] {
            "MS SQL Server",
            "MS Access"});
            this.serverTypeComboBox.Location = new System.Drawing.Point(173, 53);
            this.serverTypeComboBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.serverTypeComboBox.Name = "serverTypeComboBox";
            this.serverTypeComboBox.Size = new System.Drawing.Size(320, 24);
            this.serverTypeComboBox.TabIndex = 3;
            this.serverTypeComboBox.SelectedValueChanged += new System.EventHandler(this.serverTypeComboBox_SelectedValueChanged);
            this.serverTypeComboBox.EnabledChanged += new System.EventHandler(this.serverTypeComboBox_EnabledChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(33, 57);
            this.label1.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(130, 17);
            this.label1.TabIndex = 2;
            this.label1.Text = "Тип подключения:";
            // 
            // accessPathTextBox
            // 
            this.accessPathTextBox.Location = new System.Drawing.Point(173, 86);
            this.accessPathTextBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.accessPathTextBox.Name = "accessPathTextBox";
            this.accessPathTextBox.Size = new System.Drawing.Size(295, 22);
            this.accessPathTextBox.TabIndex = 4;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(0, 91);
            this.label2.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(159, 17);
            this.label2.TabIndex = 5;
            this.label2.Text = "Имя файла БД Access:";
            // 
            // openFileDialogBtn
            // 
            this.openFileDialogBtn.Location = new System.Drawing.Point(469, 86);
            this.openFileDialogBtn.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.openFileDialogBtn.Name = "openFileDialogBtn";
            this.openFileDialogBtn.Size = new System.Drawing.Size(25, 25);
            this.openFileDialogBtn.TabIndex = 5;
            this.openFileDialogBtn.Text = "...";
            this.openFileDialogBtn.UseVisualStyleBackColor = true;
            this.openFileDialogBtn.Click += new System.EventHandler(this.openFileDialogBtn_Click);
            // 
            // serverNameTextBox
            // 
            this.serverNameTextBox.Location = new System.Drawing.Point(173, 118);
            this.serverNameTextBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.serverNameTextBox.Name = "serverNameTextBox";
            this.serverNameTextBox.Size = new System.Drawing.Size(320, 22);
            this.serverNameTextBox.TabIndex = 6;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(45, 122);
            this.label3.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(117, 17);
            this.label3.TabIndex = 8;
            this.label3.Text = "Имя SQL Server:";
            // 
            // useSQLAuthCheckBox
            // 
            this.useSQLAuthCheckBox.AutoSize = true;
            this.useSQLAuthCheckBox.Location = new System.Drawing.Point(4, 183);
            this.useSQLAuthCheckBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.useSQLAuthCheckBox.Name = "useSQLAuthCheckBox";
            this.useSQLAuthCheckBox.Size = new System.Drawing.Size(220, 21);
            this.useSQLAuthCheckBox.TabIndex = 8;
            this.useSQLAuthCheckBox.Text = "Аутентификация SQL Server";
            this.useSQLAuthCheckBox.UseVisualStyleBackColor = true;
            this.useSQLAuthCheckBox.CheckedChanged += new System.EventHandler(this.useSQLAuthCheckBox_CheckedChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(31, 154);
            this.label4.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(128, 17);
            this.label4.TabIndex = 11;
            this.label4.Text = "Имя базы данных:";
            // 
            // userTextBox
            // 
            this.userTextBox.Location = new System.Drawing.Point(173, 212);
            this.userTextBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.userTextBox.Name = "userTextBox";
            this.userTextBox.Size = new System.Drawing.Size(320, 22);
            this.userTextBox.TabIndex = 9;
            // 
            // pwdTextBox
            // 
            this.pwdTextBox.Location = new System.Drawing.Point(173, 244);
            this.pwdTextBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.pwdTextBox.Name = "pwdTextBox";
            this.pwdTextBox.PasswordChar = '*';
            this.pwdTextBox.Size = new System.Drawing.Size(320, 22);
            this.pwdTextBox.TabIndex = 10;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(24, 215);
            this.label5.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(135, 17);
            this.label5.TabIndex = 14;
            this.label5.Text = "Имя пользователя:";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(3, 247);
            this.label6.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(157, 17);
            this.label6.TabIndex = 15;
            this.label6.Text = "Пароль пользователя:";
            // 
            // connectBtn
            // 
            this.connectBtn.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.connectBtn.Location = new System.Drawing.Point(348, 1);
            this.connectBtn.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.connectBtn.Name = "connectBtn";
            this.connectBtn.Size = new System.Drawing.Size(147, 47);
            this.connectBtn.TabIndex = 1;
            this.connectBtn.Text = "Подключить";
            this.connectBtn.UseVisualStyleBackColor = true;
            this.connectBtn.Click += new System.EventHandler(this.connectBtn_Click);
            // 
            // dbNameTextBox
            // 
            this.dbNameTextBox.Location = new System.Drawing.Point(173, 151);
            this.dbNameTextBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.dbNameTextBox.Name = "dbNameTextBox";
            this.dbNameTextBox.Size = new System.Drawing.Size(320, 22);
            this.dbNameTextBox.TabIndex = 7;
            // 
            // useWHsettingsBtn
            // 
            this.useWHsettingsBtn.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.useWHsettingsBtn.Location = new System.Drawing.Point(7, 1);
            this.useWHsettingsBtn.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.useWHsettingsBtn.Name = "useWHsettingsBtn";
            this.useWHsettingsBtn.Size = new System.Drawing.Size(147, 47);
            this.useWHsettingsBtn.TabIndex = 16;
            this.useWHsettingsBtn.Text = "Настройки Warehouse Pro";
            this.useWHsettingsBtn.UseVisualStyleBackColor = true;
            this.useWHsettingsBtn.Click += new System.EventHandler(this.useWHsettingsBtn_Click);
            // 
            // useLightSettingsBtn
            // 
            this.useLightSettingsBtn.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.useLightSettingsBtn.Location = new System.Drawing.Point(177, 1);
            this.useLightSettingsBtn.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.useLightSettingsBtn.Name = "useLightSettingsBtn";
            this.useLightSettingsBtn.Size = new System.Drawing.Size(147, 47);
            this.useLightSettingsBtn.TabIndex = 17;
            this.useLightSettingsBtn.Text = "Настройки Warehouse Light";
            this.useLightSettingsBtn.UseVisualStyleBackColor = true;
            this.useLightSettingsBtn.Click += new System.EventHandler(this.useLightSettingsBtn_Click);
            // 
            // LoginForm
            // 
            this.AcceptButton = this.connectBtn;
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.ClientSize = new System.Drawing.Size(504, 278);
            this.Controls.Add(this.useLightSettingsBtn);
            this.Controls.Add(this.useWHsettingsBtn);
            this.Controls.Add(this.dbNameTextBox);
            this.Controls.Add(this.connectBtn);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.pwdTextBox);
            this.Controls.Add(this.userTextBox);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.useSQLAuthCheckBox);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.serverNameTextBox);
            this.Controls.Add(this.openFileDialogBtn);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.accessPathTextBox);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.serverTypeComboBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.Name = "LoginForm";
            this.Text = "Подключение к базе данных";
            this.Load += new System.EventHandler(this.LoginForm_Load);
            this.Shown += new System.EventHandler(this.LoginForm_Shown);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.LoginForm_KeyDown);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ComboBox serverTypeComboBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox accessPathTextBox;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button openFileDialogBtn;
        private System.Windows.Forms.TextBox serverNameTextBox;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.CheckBox useSQLAuthCheckBox;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox userTextBox;
        private System.Windows.Forms.TextBox pwdTextBox;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Button connectBtn;
        private System.Windows.Forms.TextBox dbNameTextBox;
        private System.Windows.Forms.Button useWHsettingsBtn;
        private System.Windows.Forms.Button useLightSettingsBtn;
    }
}