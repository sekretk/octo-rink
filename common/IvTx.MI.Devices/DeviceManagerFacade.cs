using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using IvTx.Core;
using IvTx.Core.Abstract;
using IvTx.Core.Helpers;
using IvTx.Core.Log;
using MILibrary;

namespace IvTx.MI.Devices
{
    public enum PaymentType
    {
        Cash = 0,
        Card = 1,
        Account = 2,
        Voucher = 3,
    }

    public static class DeviceManagerFacade
    {
        private const string FISCAL_NAME_1 = "Fiscal_Printer_00";

        private const string NON_FISCAL_NAME_1 = "Kitchen_Printer_00";

        private const string DISPLAY_1 = "Display_00";

        private const string DISPLAY_2 = "Display_01";

        /// <summary>
        /// Заполнить экран покупателя
        /// </summary>
        /// <param name="line1">Первая линия дисплея покупателя</param>
        /// <param name="line2">Вторая линия дисплея покупателя</param>
        public static void ShowOnVFD(string line1, string line2, int vfdNUmber = 1)
        {
            if (_fp == null)
                _fp = new DeviceManager.FPMain();

            _fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", vfdNUmber == 1 ? DISPLAY_1 : DISPLAY_2, 1);
            //_fp.set_ParentAppPath("\\");
            _fp.DisplayClearText();

            int symbols = _fp.get_DisplaySymbols();
            var l1 = line1.Length < symbols ? line1 : line1.Substring(0, symbols);
            var l2 = line2.Length < symbols ? line2 : line2.Substring(0, symbols);

            _fp.DisplaySetText($"{l1}\r{l2}");
        }

        /// <summary>
        /// Очистить экран покупателя
        /// </summary>
        public static void ClearVFD(int vfdNUmber = 1)
        {
            Failover.Execute(() =>
            {
                DeviceManager.FPMain vdf = new DeviceManager.FPMain();
                vdf.LoadSettings(DMPath() + "\\DeviceTestTool.config", vfdNUmber == 1 ? DISPLAY_1 : DISPLAY_2, 1);
                //vdf.set_ParentAppPath(MediaTypeNames.Application.StartupPath + "\\");
                vdf.DisplayClearText();
                
            }, exception => Logger.Error("Device Manager Error. Очистка дисплея закончилась неудачей.", exception));
        }

        public static void PrintNFPtextT(string text)
        {
            if (_fp == null)
                _fp = new DeviceManager.FPMain();

            // Load settings file
            _fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", FISCAL_NAME_1, 1);

            _fp.ClearFPScriptsInfo();

            _fp.set_NonFiscalReceiptType(1);

            _fp.NonFiscalReceiptOpen();

            _fp.NonFiscalReceiptPrintText(text);

            _fp.NonFiscalReceiptClose();
        }

        /// <summary>
        /// Печатает нефискальный чек 
        /// </summary>
        /// <param name="nfpName">Наименование устройства в файле конфигурации Device Manager</param>
        /// <param name="userID">Идентификатор активного пользователя</param>
        /// <param name="header">массив строк Header</param>
        /// <param name="detail">массив строк Details</param>
        /// <param name="total">массив строк Total</param>
        /// <param name="footer">массив строк Footer</param>
        /// <param name="data">Data Table с колонками Name, Qtty и Price</param>
        /// <returns>Отметку об успехе выполнения операции</returns>
        public static void PrintNonFiscalT(string user, IEnumerable<IPosition> data, string auxilaryInfo = null)
        {
            // Create an object DM
            if (_fp == null)
                _fp = new DeviceManager.FPMain();

            // Load settings file
            _fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", FISCAL_NAME_1, 1);

            _fp.set_NonFiscalReceiptType(1);

            short i = 0;

            _fp.NonFiscalReceiptOpen();

            foreach (var position in data)
                _fp.NonFiscalReceiptPrintItem("1", position.Name, position.Price, position.Quantity, position.VATCode, position.VATPercent, "", FixedVAT: position.FixedVAT);

            if (!String.IsNullOrEmpty(auxilaryInfo))
                _fp.NonFiscalReceiptPrintText(auxilaryInfo);

            _fp.NonFiscalReceiptClose();

            //todo redo with EF
            //if (!DbWorks.AddECRReceipt(connection, 36, 0, DbWorks.GetNextReceiptID(connection), 2, nfp.DeviceName, nfpName, 0, userID))
            //    log.Write("Не удалось сделать запись о чеке в БД", EventType.FatalError);
        }
        
        static DeviceManager.FPMain _fp;

        /// <summary>
        /// Печатает фискальный чек
        /// </summary>
        /// <param name="fpName">Название устройства в файле конфигурации Device Manager</param>
        /// <param name="header">Массив строк Header</param>
        /// <param name="detail">Массив строк Details</param>
        /// <param name="total">Массив строк Total</param>
        /// <param name="footer">Массив строк Footer</param>
        /// <param name="data">Таблица данных. Должна содержать в сете колонки с названием Name, Qtty и Price</param>
        /// <param name="template"></param>
        /// <returns>Отметку об удачном выполнении</returns>
        public static bool PrintFiscalT(string user, IEnumerable<IPosition> data, BillTemplate template, PaymentType paymentType = PaymentType.Cash, string auxilaryInfo = null)
        {
            // Create an object DM
            if (_fp == null)
                _fp = new DeviceManager.FPMain();

            // Load settings file
            _fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", FISCAL_NAME_1, 1);

            if (template.Header != null)
            {
                _fp.set_Headers((short) (template.Header?.Count() ?? 0));
                for (short i = 0; i < _fp.get_Headers(); i++)
                    _fp.SetHeaderLine(i, template.Header[i]);
            }

            if (template.Details != null)
            {
                _fp.set_Details((short) (template.Details?.Count() ?? 0));
                for (short i = 0; i < _fp.get_Details(); i++)
                    _fp.SetDetailLine(i, template.Details[i]);
            }

            if (template.Total != null)
            {
                _fp.set_Totals((short) (template.Total?.Count() ?? 0));
                for (short i = 0; i < _fp.get_Totals(); i++)
                    _fp.SetTotalLine(i, template.Total[i]);
            }

            if (template.Footer != null)
            {
                _fp.set_Footers((short)(1 + template.Footer?.Count() ?? 0));
                _fp.SetFooterLine(0, auxilaryInfo);
                for (short i = 1; i < _fp.get_Footers(); i++)
                    _fp.SetFooterLine(i, template.Footer[i-1]);
            }

            if (_fp.NeedZReport())
            {
                Logger.Warning("Device Manager Error.Требуется закрыть смену на фискальном регистраторе!");
                return false;
            }

            Logger.Trace($@"ЧЕК: Отрытие чека");
            _fp.FiscalReceiptOpen();

            data.ForEach(p =>
            {
                Logger.Trace($@"ЧЕК: Добавлена к чеку позиция {p.GoodID} - {p.Name}, 
                                цена: {p.Price}, к-во {p.Quantity}, 
                                Код налоговой группы - {p.VATCode},
                                Процент {p.VATPercent}, сумма налога: {p.FixedVAT}");
                _fp.FiscalReceiptPrintItem(p.GoodID.ToString(), p.Name, p.Price, p.Quantity, p.VATCode,
                        p.VATPercent, "", FixedVAT: p.FixedVAT);
            });

            Logger.Trace($@"ЧЕК: Итого {data.Sum(x => x.TotalPrice)} - тип {paymentType}");
            _fp.FiscalReceiptPayment(data.Sum(x => x.TotalPrice), (short)paymentType);
            //_fp.InvoicePayment(100, 1);

            //if (!String.IsNullOrEmpty(auxilaryInfo))
            //    _fp.FiscalReceiptPrintText(auxilaryInfo);

            //Enumerable.Range(0,10).ForEach(i => _fp.SetFooterLine((short)i, _fp.GetFooterLine((short)i).Replace(" <Operator>", user)));

            Logger.Trace($@"ЧЕК: Закрытие чека");
            _fp.FiscalReceiptClose();

            return true;

            //todo redo with EF
            //if (!DbWorks.AddECRReceipt(connection, 36, DbWorks.GetNextAcct(connection, 36), DbWorks.GetNextReceiptID(connection), 1, _fp.DeviceName, fpName, money, userID))
            //    log.Write("Не удалось сделать запись о чеке в БД", EventType.FatalError);
        }

        /// <summary>
        /// Печатает чек возврата
        /// </summary>
        /// <param name="data">Таблица данных. Должна содержать в сете колонки с названием Name, Qtty и Price</param>
        /// <param name="template"></param>
        /// <returns>Отметку об удачном выполнении</returns>
        public static bool PrintReturnT(string user, IEnumerable<IPosition> data, BillTemplate template, bool fiscal, PaymentType paymentType = PaymentType.Cash,  string auxilaryInfo = null)
        {
            // Create an object DM
            if (_fp == null)
                _fp = new DeviceManager.FPMain();

            // Load settings file
            _fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", FISCAL_NAME_1, 1);

            if (template.Header != null)
            {
                _fp.set_Headers((short)(template.Header?.Count() ?? 0));
                for (short i = 0; i < _fp.get_Headers(); i++)
                    _fp.SetHeaderLine(i, template.Header[i]);
            }

            if (template.Details != null)
            {
                _fp.set_Details((short)(template.Details?.Count() ?? 0));
                for (short i = 0; i < _fp.get_Details(); i++)
                    _fp.SetDetailLine(i, template.Details[i]);
            }

            if (template.Total != null)
            {
                _fp.set_Totals((short)(template.Total?.Count() ?? 0));
                for (short i = 0; i < _fp.get_Totals(); i++)
                    _fp.SetTotalLine(i, template.Total[i]);
            }

            if (template.Footer != null)
            {
                _fp.set_Footers((short)(1 + template.Footer?.Count() ?? 0));
                _fp.SetFooterLine(0, auxilaryInfo);
                for (short i = 1; i < _fp.get_Footers(); i++)
                    _fp.SetFooterLine(i, template.Footer[i - 1]);
            }

            if (_fp.NeedZReport())
            {
                Logger.Warning("Device Manager Error.Требуется закрыть смену на фискальном регистраторе!");
                return false;
            }

            _fp.ReceiptNew(); 

            data.ForEach(p =>
            _fp.ReceiptAddItem(
                        Convert.ToString(p.GoodID.ToString()),
                        Convert.ToString(p.Name),
                        Convert.ToDouble(p.Price),
                        Convert.ToDouble(p.Quantity),
                        Convert.ToString(0),
                        Convert.ToDouble(0),
                        Convert.ToString(""),
                        0));
            //_fp.FiscalReceiptPrintItem(p.GoodID.ToString(), p.Name, p.Price, p.Quantity, 0, 0, ""));

            _fp.ReceiptAddPayment(data.Sum(x => x.TotalPrice), (short)paymentType, false);

            //_fp.InvoicePayment(100, 1);

            //if (!String.IsNullOrEmpty(auxilaryInfo))
            //    _fp.FiscalReceiptPrintText(auxilaryInfo);

            //Enumerable.Range(0,10).ForEach(i => _fp.SetFooterLine((short)i, _fp.GetFooterLine((short)i).Replace("<Operator>", user)));

            _fp.ReceiptPrint(false, true, false, 0, false);

            return true;

            //todo redo with EF
            //if (!DbWorks.AddECRReceipt(connection, 36, DbWorks.GetNextAcct(connection, 36), DbWorks.GetNextReceiptID(connection), 1, _fp.DeviceName, fpName, money, userID))
            //    log.Write("Не удалось сделать запись о чеке в БД", EventType.FatalError);
        }
        
        /// <summary>
        /// Получает путь к каталогу Device Manager. При нейдаче записывает текс ошибки в лог и возвращает путь к каталогу с запускгым файлом приложения.
        /// </summary>
        /// <returns>Строку пути</returns>
        public static string DMPath()
        {
            //return @"c:\ProgramData\Microinvest\Device Test Tool\";

            string dmPath = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), @"Microinvest\Device Test Tool");
            if (!Directory.Exists(dmPath)) Logger.Warning($"Нет папки DeviceManager {dmPath}, возможно не установлен DeviceManagerTool");
            return dmPath;
        }

        /// <summary>
        /// Печатает X отчет на заданном фискальном регистраторе
        /// </summary>
        /// <param name="prtName">Название принтера в файле настроек Device Manager</param>
        public static void xReport()
        {
            DeviceManager.FPMain fp = new DeviceManager.FPMain();
            fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", FISCAL_NAME_1, 1);
            fp.Report(1, "", "");
        }

        /// <summary>
        /// Печатает Z отчет на заданном фискальном регистраторе
        /// </summary>
        /// <param name="prtName">Название принтер в файле настроек Device Manager</param>
        public static void zReport(string prtName)
        {
            DeviceManager.FPMain fp = new DeviceManager.FPMain();
            fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", prtName, 1);
            //_fp.set_ParentAppPath(MediaTypeNames.Application.StartupPath + "\\");
            fp.Report(0, "", "");
        }

        public static void zReport()
        {
            zReport(FISCAL_NAME_1);
        }

        /// <summary>
        /// Запускает печать тестового не фискального чека
        /// </summary>
        /// <param name="prtName">Название принтера в файле настроек</param>
        public static void nonFiscalPrtTest(string prtName)
        {
            Failover.Execute(() =>
            {
                DeviceManager.FPMain fp = new DeviceManager.FPMain();
                fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", prtName, 1);
                //_fp.set_ParentAppPath(MediaTypeNames.Application.StartupPath + "\\");
                fp.TestNonFiscalKitchen();
            }, exception => Logger.Error("Device Manager Error. Не удалось проверить принтер", exception));
        }

        /// <summary>
        /// Запускает печать тестового фискального чека
        /// </summary>
        /// <param name="prtName">Название принтера в файле настроек</param>
        public static void fiscalPrtTest(string prtName)
        {
            Failover.Execute(() =>
            {
                DeviceManager.FPMain fp = new DeviceManager.FPMain();
                fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", prtName, 1);
                //_fp.set_ParentAppPath(MediaTypeNames.Application.StartupPath + "\\");
                fp.TestFiscalPrintStandart();
            }, exception => Logger.Error(exception, "Device Manager Error. Не удалось проверить принтер {0} .", prtName));
        }

        public static bool CardPayment(double amount)
        {
            if (_fp == null)
                _fp = new DeviceManager.FPMain();

            _fp.LoadSettings(DMPath() + "\\DeviceTestTool.config", FISCAL_NAME_1, 1);

            if (_fp.NeedZReport())
            {
                Logger.Warning("Device Manager Error.Требуется закрыть смену на фискальном регистраторе!");
                return false;
            }

            _fp.FiscalReceiptOpen();

            _fp.FiscalReceiptPayment(amount, 1);

            _fp.FiscalReceiptClose();

            return true;
        }
    }
}
