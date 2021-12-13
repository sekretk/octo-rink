using System.Configuration;
using System.Xml;

namespace IvTx.Core.Config
{
    public class AppConfigSectionHandler : IConfigurationSectionHandler
    {
        /// <summary>
        /// ������� ����������� ������� ������������.
        /// </summary>
        /// <returns>
        /// ��������� ������ ����������� �������.
        /// </returns>
        /// <param name="parent">������������ ������.</param><param name="configContext">������ ��������� ������������.</param><param name="section">���� ������� XML.</param>
        public object Create(object parent, object configContext, XmlNode section)
        {
            return new ConfigData(section);
        }
    }
}