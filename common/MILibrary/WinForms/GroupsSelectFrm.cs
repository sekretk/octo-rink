using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace MILibrary
{
    public partial class GroupsSelectFrm : Form
    {
        DataSet sdsData;

        public Group choosenGrp = new Group();

        MIObjectType grpType;

        IDbConnection con;

        public GroupsSelectFrm(IDbConnection connection, MIObjectType type, Group previuos)
        {
            InitializeComponent();

            switch (type)
            {
                case MIObjectType.Good: this.Text = "Выбор группы товаров"; break;
                case MIObjectType.Object: this.Text = "Выбор группы объектов"; break;
                case MIObjectType.Partner: this.Text = "Выбор группы партнеров"; break;
                case MIObjectType.User: this.Text = "Выбор группы пользователя"; break;
            }

            con = connection;
            grpType = type;

            sdsData = DbWorks.GetDataSet(connection, 
                                    String.Format(@"SELECT 
                                            GG2.ID AS ID, 
                                            GG2.Name AS CAPTION, 
                                            PARENT = CASE GG1.ID WHEN 1 THEN 0 ELSE GG1.ID END
                                    FROM {0}sGroups AS GG1
                                    JOIN {0}sGroups AS GG2 
                                            ON GG1.Code = CASE WHEN LEN(GG2.Code) > 3 THEN SUBSTRING(GG2.Code, 1, LEN(GG2.Code) - 3) ELSE '-1' END
                                    ORDER BY LEN(GG2.Code);", type.ToString("G")));
            if (sdsData.Tables.Count == 0)
            {
                MessageBox.Show("Не удалось загрузить список группы.");
                return;
            }
            this.DialogResult = System.Windows.Forms.DialogResult.Cancel;

            foreach (DataRow row in sdsData.Tables[0].Rows)
            {
                if (row["PARENT"].ToString() == "0")
                {
                    TreeNode node = new TreeNode(row["CAPTION"].ToString());
                    if ((int)row["ID"] == 1)
                        node.Tag = 1;
                    else
                        node.Tag = (int)row["ID"];
                    treeViewGrps.Nodes.Add(node);
                    CreateNodesOfParent(Int32.Parse(row["ID"].ToString()), node);
                }
            }

            if (previuos != null)
            {
                choosenGrp = previuos;

                foreach (TreeNode node in treeViewGrps.Nodes)
                    FindNode(node);
            }
        }

        private void FindNode(TreeNode node)
        {
            if ((int)node.Tag == choosenGrp.ID)
                treeViewGrps.SelectedNode = node;

            if (node.Nodes.Count == 0) return;

            foreach (TreeNode nd in node.Nodes)
                FindNode(nd);
        }

        private void CreateNodesOfParent(int iParent, TreeNode pNode)
        {
            DataView dvwData = new DataView(sdsData.Tables[0]);

            dvwData.RowFilter = "[PARENT] = " + iParent;

            foreach (DataRowView Row in dvwData)
            {
                if (pNode == null)
                {
                    TreeNode zNode = treeViewGrps.Nodes.Add(Row["ID"].ToString(), Row["CAPTION"].ToString());
                    zNode.Tag = (int)Row["ID"];
                    CreateNodesOfParent((int)Row["ID"], zNode);
                }
                else
                {
                    TreeNode zNode = pNode.Nodes.Add(Row["ID"].ToString(), Row["CAPTION"].ToString());
                    zNode.Tag = (int)Row["ID"];
                    CreateNodesOfParent((int)Row["ID"], zNode);
                }
            }
        }

        private void treeViewGrps_AfterSelect(object sender, TreeViewEventArgs e)
        {
            SetSelected(treeViewGrps.SelectedNode);
        }

        private void SetSelected(TreeNode tree)
        {
            choosenGrp = new Group();
            choosenGrp = DbWorks.GetGroup(con, (int)tree.Tag, grpType);
        }

        private void treeViewGrps_NodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            SetSelected(treeViewGrps.SelectedNode);
            DialogResult = System.Windows.Forms.DialogResult.OK;
        }

        private void btnOK_Click(object sender, EventArgs e)
        {
            SetSelected(treeViewGrps.SelectedNode);
            DialogResult = System.Windows.Forms.DialogResult.OK;
        }
    }
}
