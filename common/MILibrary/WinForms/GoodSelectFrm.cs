using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace MILibrary
{
    /// <summary>
    /// Форма выбора товара.
    /// </summary>
    public partial class GoodSelectFrm : Form
    {
        IDbConnection cnc = null;
        DataSet sdsData;

        private int[] selectedGoodID;

        public Good previuosChoose;

        /// <summary>
        /// Возвращает ID выбранного товара. Если товар не выбран, то возвращается 0
        /// </summary>
        public int[] SelectedID
        {
            get { return selectedGoodID; }
        }

        /// <summary>
        /// Конструктор без указания соединения.
        /// </summary>
        public GoodSelectFrm()
        {
            InitializeComponent();
            this.Shown += new EventHandler(userChooseFrm_Shown);
        }

        /// <summary>
        /// Конструктор с указание соединения.
        /// </summary>
        /// <param name="connection">Соединение с источником данных.</param>
        public GoodSelectFrm(IDbConnection connection)
            : this()
        {
            cnc = connection;
            SetupDVG();
            this.Text = "Товары";
            this.KeyPreview = true;      
        }

        public GoodSelectFrm(IDbConnection connection, Good prevGood)
            : this(connection)
        {
            previuosChoose = prevGood;
        }

        void userChooseFrm_Shown(object sender, EventArgs e)
        {
            sdsData = DbWorks.GetDataSet(cnc, @"SELECT GG2.ID AS ID, GG2.Name AS CAPTION, PARENT = CASE GG1.ID WHEN 1 THEN 0 ELSE GG1.ID END
                                    FROM GoodsGroups AS GG1
                                    JOIN GoodsGroups AS GG2 ON GG1.Code = CASE WHEN LEN(GG2.Code) > 3 THEN SUBSTRING(GG2.Code, 1, LEN(GG2.Code) - 3) ELSE '-1' END
                                    ORDER BY LEN(GG2.Code);");
            if (sdsData.Tables.Count == 0)
            {
                MessageBox.Show("Не удалось загрузить список группы.");
                return;
            }

            foreach (DataRow row in sdsData.Tables[0].Rows)
            {
                if (row["PARENT"].ToString() == "0")
                {
                    TreeNode node = new TreeNode(row["CAPTION"].ToString());
                    if ((int)row["ID"] == 1)
                        node.Tag = -1;
                    else                    
                        node.Tag = (int)row["ID"];
                    if (previuosChoose != null && (int)row["ID"] == (previuosChoose.GroupID == -1 ? 1 : previuosChoose.GroupID))
                        GroupsTreeView.SelectedNode = node;
                    GroupsTreeView.Nodes.Add(node);
                    CreateNodesOfParent((int)row["ID"], node);
                }             
            }
        }

        private void CreateNodesOfParent(int iParent, TreeNode pNode)
        {
            DataView dvwData = new DataView(sdsData.Tables[0]);

            dvwData.RowFilter = "[PARENT] = " + iParent;

            foreach (DataRowView Row in dvwData)
            {
                if (pNode == null)
                {
                    TreeNode zNode = GroupsTreeView.Nodes.Add(Row["ID"].ToString(), Row["CAPTION"].ToString());
                    if (previuosChoose != null && (int)Row["ID"] == (previuosChoose.GroupID == -1 ? 1 : previuosChoose.GroupID))
                        GroupsTreeView.SelectedNode = zNode;
                    zNode.Tag = (int)Row["ID"];
                    CreateNodesOfParent((int)Row["ID"], zNode);
                }
                else
                {
                    TreeNode zNode = pNode.Nodes.Add(Row["ID"].ToString(), Row["CAPTION"].ToString());
                    if (previuosChoose != null && (int)Row["ID"] == (previuosChoose.GroupID == -1 ? 1 : previuosChoose.GroupID))
                        GroupsTreeView.SelectedNode = zNode;
                    zNode.Tag = Row["ID"].ToString();
                    CreateNodesOfParent(Int32.Parse(Row["ID"].ToString()), zNode);
                }
            }
        }
        
        private void FillDGV(string groupID)
        {
            DataSet ds = DbWorks.GetDataSet(cnc, "SELECT G.ID AS ID, G.Code AS Code, G.Name AS Name, G.Measure1 AS Measure, G.PriceIn AS  PriceIn, G.PriceOut1 AS PriceOut FROM Goods AS G WHERE G.GroupID = " + groupID);
            ElementsDGV.DataSource = ds.Tables[0];

            DataGridViewRow selRow = new DataGridViewRow();

            foreach (DataGridViewRow row in ElementsDGV.Rows)
                if ((int)row.Cells[0].Value == previuosChoose.ID)
                    selRow = row;
            if (selRow.DataGridView == ElementsDGV)
            {
                ElementsDGV.ClearSelection();
                selRow.Selected = true;
            }
        }

        private void SetupDVG()
        {
            ElementsDGV.RowHeadersVisible = false;
            ElementsDGV.AutoGenerateColumns = false;
            ElementsDGV.AllowUserToAddRows = false;
            ElementsDGV.AllowUserToDeleteRows = false;
            ElementsDGV.AllowUserToResizeRows = false;
            ElementsDGV.MultiSelect = true;
            ElementsDGV.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            ElementsDGV.Paint += new PaintEventHandler(ElementsDGV_Paint);
            ElementsDGV.CellDoubleClick += new DataGridViewCellEventHandler(ElementsDGV_CellDoubleClick);


            DataGridViewTextBoxColumn IDColumn = new DataGridViewTextBoxColumn();
            IDColumn.Visible = false;
            DataGridViewTextBoxColumn codeColumn = new DataGridViewTextBoxColumn();
            DataGridViewTextBoxColumn nameColumn = new DataGridViewTextBoxColumn();
            DataGridViewTextBoxColumn measureColumn = new DataGridViewTextBoxColumn();
            DataGridViewTextBoxColumn priceInColumn = new DataGridViewTextBoxColumn();
            DataGridViewTextBoxColumn priceOutColumn = new DataGridViewTextBoxColumn();

            IDColumn.DataPropertyName = "ID";
            codeColumn.DataPropertyName = "Code";
            nameColumn.DataPropertyName = "Name";
            measureColumn.DataPropertyName = "Measure";
            priceInColumn.DataPropertyName = "PriceIn";
            priceOutColumn.DataPropertyName = "PriceOut";
            
            codeColumn.HeaderText = "Код";
            nameColumn.HeaderText = "Наименование";
            measureColumn.HeaderText = "Ед.";
            priceInColumn.HeaderText = "Прих.цена";
            priceOutColumn.HeaderText = "Роз.цена";

            codeColumn.ReadOnly = true;
            nameColumn.ReadOnly = true;
            measureColumn.ReadOnly = true;
            priceInColumn.ReadOnly = true;
            priceOutColumn.ReadOnly = true;
                        
            codeColumn.Width = 40;
            nameColumn.AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            measureColumn.Width = 40;
            priceInColumn.Width = 60;
            priceOutColumn.Width = 60;

            codeColumn.Resizable = DataGridViewTriState.False;
            nameColumn.Resizable = DataGridViewTriState.False;
            measureColumn.Resizable = DataGridViewTriState.False;
            priceInColumn.Resizable = DataGridViewTriState.False;
            priceOutColumn.Resizable = DataGridViewTriState.False;

            codeColumn.SortMode = DataGridViewColumnSortMode.NotSortable;
            nameColumn.SortMode = DataGridViewColumnSortMode.NotSortable;
            measureColumn.SortMode = DataGridViewColumnSortMode.NotSortable;
            priceInColumn.SortMode = DataGridViewColumnSortMode.NotSortable;
            priceOutColumn.SortMode = DataGridViewColumnSortMode.NotSortable;

            ElementsDGV.Columns.Add(IDColumn);
            ElementsDGV.Columns.Add(codeColumn);
            ElementsDGV.Columns.Add(nameColumn);
            ElementsDGV.Columns.Add(measureColumn);
            ElementsDGV.Columns.Add(priceInColumn);
            ElementsDGV.Columns.Add(priceOutColumn);            
        }

        void ElementsDGV_CellDoubleClick(object sender, DataGridViewCellEventArgs e)
        {
            if (e.RowIndex >= 0)
            {
                selectedGoodID = new int[1];
                selectedGoodID[0] = Int32.Parse(ElementsDGV[0, e.RowIndex].Value.ToString());
                this.DialogResult = System.Windows.Forms.DialogResult.OK;
            }
        }

        void ElementsDGV_Paint(object sender, PaintEventArgs e)
        {
            foreach (DataGridViewRow row in ElementsDGV.Rows)
            {
                if (row.Index % 2 == 0) row.DefaultCellStyle.BackColor = Color.LightGray;
                row.Height = 18;
            }
        }

        private void cancelBtn_Click(object sender, EventArgs e)
        {
            selectedGoodID = null;
            this.DialogResult = System.Windows.Forms.DialogResult.Cancel;
        }

        private void okBtn_Click(object sender, EventArgs e)
        {
            selectedGoodID = new int[ElementsDGV.SelectedRows.Count];
            for (int i = 0; i < ElementsDGV.SelectedRows.Count; i++)
                selectedGoodID[i] = Int32.Parse(ElementsDGV[0, ElementsDGV.SelectedRows[i].Index].Value.ToString());
            this.DialogResult = System.Windows.Forms.DialogResult.OK;
        }

        private void GroupsTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            FillDGV(e.Node.Tag.ToString());
            if (ElementsDGV.Rows.Count > 0)
            {
                okBtn.Enabled = true;
                editBtn.Enabled = true;
                delBtn.Enabled = true;
            }
            else
            {
                okBtn.Enabled = false;
                editBtn.Enabled = false;
                delBtn.Enabled = false;
            }
        }

        private void goodSelectFrm_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape)
            {
                selectedGoodID = null;
                this.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            }

            if (e.KeyCode == Keys.Enter)
            {
                okBtn.PerformClick();
            }
        }

        private void SelectGroup(int grpID)
        {

        }

        private void SelectElement(int elemID)
        {

        }
    }
}
