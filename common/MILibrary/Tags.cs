using System;
using System.Data;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace MILibrary
{
    public static class Tags
    {

        /// <summary>
        /// Возвращает строку с заменными тегами
        /// </summary>
        /// <param name="connection">Активное и откытое подключение к источнику данных</param>
        /// <param name="partner">Активный партнер</param>
        /// <param name="user">Активный пользователь</param>
        /// <param name="obj">Активный объект</param>
        /// <param name="source">Исходная строка</param>
        /// <returns>Строка с заменными тегами</returns>
        public static string ReplaceTags(IDbConnection connection, Partner partner, User user, Object obj, string source)
        {
            LogWriter log = new LogWriter("Замена тегов");
            try
            {
                source = source.Replace("<Time>", DateTime.Now.ToShortTimeString());
                source = source.Replace("<Date>", DateTime.Today.ToShortDateString());
                source = source.Replace("<ReceiptNumber>", DbWorks.GetNextReceiptID(connection).ToString().PadLeft(10, '0'));
                source = source.Replace("<PartnerName>", partner == null ? "" : partner.Company);
                source = source.Replace("<PartnerCard>", partner == null ? "" : partner.CardNumber);
                source = source.Replace("<PartnerBalance>", String.Format("{0:0.00}", partner == null ? 0 : DbWorks.GetBalance(connection, partner)));
                source = source.Replace("<PartnerTodayAdvSum>", String.Format("{0:0.00}", partner == null ? 0 : DbWorks.PartnerAdvanceSum(connection, partner, false)));
                source = source.Replace("<PartnerTodaySaleSum>", String.Format("{0:0.00}", partner == null ? 0 : DbWorks.PartnerSalesSum(connection, partner, false)));

                Organisation org = DbWorks.GetRegOrganisation(connection);

                source = source.Replace("<OrgName>", org.Company);
                source = source.Replace("<OrgMOL>", org.MOL);
                source = source.Replace("<OrgAddress>", org.Address);
                source = source.Replace("<OrgPhone>", org.Phone);
                source = source.Replace("<OrgCity>", org.City);
                source = source.Replace("<OrgEmail>", org.eMail);
            }
            catch (Exception ex)
            {
                log.Write("При замене тегов произошла ошибка\n" + ex.Message);
            }

            return source;
        }

        /// <summary>
        /// Замена тегов в массиве
        /// </summary>
        /// <param name="connection">Активное и окрытое подключение к БД</param>
        /// <param name="partner">Активный партер с которым ведется работа</param>
        /// <param name="user">Активный пользователь от имени которго ведется работа</param>
        /// <param name="obj">Активный объект на котором ведется работа</param>
        /// <param name="source">Входной массив</param>
        /// <returns>Массив с замененными тегами</returns>
        public static ArrayList ReplaceTags(IDbConnection connection, Partner partner, User user, Object obj, ArrayList source)
        {
            LogWriter log = new LogWriter("Замещение тегов");
            try
            {
            ArrayList result = new ArrayList();

            foreach (string str in source)
                result.Add(ReplaceTags(connection, partner, user, obj, str));

            return result;
            }
            catch (Exception ex)
            {
                log.Write("Не удалось заменить теги в массиве\n" + ex.Message);
                return new ArrayList();
            }
        }

        /// <summary>
        /// Применяет в строке теги <Empty>, <Left>, <Right>, <Center>, <Strech>, <LimitL20>, <LimitR15>, <LimitC30>. Наличие лимитирующих тегов подавляет действие выравнивающих тегов.
        /// </summary>
        /// <param name="source">Входная строка</param>
        /// <param name="length">Длина выходной строки. После применения всех тегов строка либо обрезается справа либо пополняется пробелами до достижения заданной длинны</param>
        /// <returns></returns>
        public static string JustTags(string source, int length)
        {
            if (source.Contains("<Empty>"))
            {
                source = new string(' ', length);
                return source;
            }

            if (HasLimitTag(source))
                source = ActionLimitTag(source);

            if (source.Contains("<Left>"))
            {
                source = source.Replace("<Left>", "");
                source = source.Trim();
                source = source.Length > length ? source.Substring(0, length) : source.PadRight(length);
            }

            if (source.Contains("<Right>"))
            {
                source = source.Replace("<Right>", "");
                source = source.Trim();
                source = source.Length > length ? source.Substring(0, length) : source.PadLeft(length);
            }

            if (source.Contains("<Center>"))
            {
                source = source.Replace("<Center>", "");
                source = source.Trim();
                source = (source.PadLeft(source.Length + ((length-source.Length)/2))).PadRight(length);
            }

            if (source.Contains("<Strech>"))
            {
                source = source.Replace("<Strech>", "");
                source = source.Length > length ? source.Substring(0, length) : source;
                source = source.Trim();
                char[] letters = source.ToCharArray();
                source = "";
                foreach (char letter in letters)                                    
                    source += letter + new string(' ', (length - letters.Length) / letters.Length);
                source.PadRight(length);
            }

            source = source.Length > length ? source.Substring(0, length) : source.PadRight(length);

            return source;
        }

        /// <summary>
        /// Рекурсивный метод применяющий лимитирующие теги (LimitL, LimitR, LimitC)
        /// </summary>
        /// <param name="source">Исходная строка</param>
        /// <returns>Строка с примененными тегами</returns>
        public static string ActionLimitTag(string source)
        {
            source = source.Replace("<Center>", "");
            source = source.Replace("<Left>", "");
            source = source.Replace("<Right>", "");
            source = source.Replace("<Strech>", "");


            if (!HasLimitTag(source))
                return source;

            if (GetFirstLimitTag(source) == String.Empty)
                return source;

            try
            {
                string firstStat = ApplyLimitTag(source.Substring(0, source.IndexOf(GetFirstLimitTag(source))), GetFirstLimitTag(source));
                return firstStat + ActionLimitTag(source.Substring(source.IndexOf(GetFirstLimitTag(source)) + 10));
            }
            catch
            {
                return source;
            }
        }

        /// <summary>
        /// Возвращает флаг присутсвия во входной строке лимитирующего тега
        /// </summary>
        /// <param name="source">Строка для проверки</param>
        /// <returns></returns>
        public static bool HasLimitTag(string source)
        {
            int i = 0;
            if (
                    source.Contains("<LimitL") && 
                    (source.Length >= (source.IndexOf("<LimitL") + 10)) && 
                    source[source.IndexOf("<LimitL")+9] == '>' && 
                    int.TryParse(source.Substring(source.IndexOf("<LimitL")+ 7, 2), out i))
            {
                return true;
            }
            if (
                    source.Contains("<LimitR") &&
                    (source.Length >= (source.IndexOf("<LimitR") + 10)) &&
                    source[source.IndexOf("<LimitR") + 9] == '>' &&
                    int.TryParse(source.Substring(source.IndexOf("<LimitR") + 7, 2), out i))
            {
                return true;
            }
            if (
                    source.Contains("<LimitC") &&
                    (source.Length >= (source.IndexOf("<LimitC") + 10)) &&
                    source[source.IndexOf("<LimitC") + 9] == '>' &&
                    int.TryParse(source.Substring(source.IndexOf("<LimitC") + 7, 2), out i))
            {
                return true;
            }
            return false;
        }

        /// <summary>
        /// Возвращает 10 символьную строку первого лимитирующего тега (<LimitL20>, <LimitR40> ...)
        /// </summary>
        /// <param name="source">Исходная строка</param>
        /// <returns>Значение лимитирующего тега</returns>
        public static string GetFirstLimitTag(string source)
        {
            try
            {
                return source.Substring(source.IndexOf("<Limit"), 10); 
            }
            catch
            {
                return String.Empty;
            }
        }

        /// <summary>
        /// Возвращает строку с примененным тегом
        /// </summary>
        /// <param name="source">Входная строка</param>
        /// <param name="tag">Значение лимитирующего тега</param>
        /// <returns></returns>
        public static string ApplyLimitTag(string source, string tag)
        {
            int i = 0;

            if (tag.Length != 10)
                return source;

            if (tag.Substring(0, 6) != "<Limit")
                return source;

            char just = tag[6];

            if (just != 'L' && just != 'R' && just != 'C')
                return source;

            if (int.TryParse(tag.Substring(7, 2), out i))
            {
                if (just == 'L')
                    source = source.Length >= i ? source.Substring(0, i) : source.PadRight(i);
                if (just == 'R')
                    source = source.Length >= i ? source.Substring(0, i) : source.PadLeft(i);
                if (just == 'C')
                    source = source.Length >= i ? source.Substring(0, i) : source.PadLeft(source.Length + (i - source.Length) / 2).PadRight(i);
            }

            return source;
        }

    }
}
