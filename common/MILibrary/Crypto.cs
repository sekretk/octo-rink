using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml;
using System.Xml.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Security.Cryptography;

namespace MILibrary
{
    /// <summary>
    /// Static class for encrypt and decrypts data
    /// </summary>
    public static class Crypto
    {        
        static private byte[] IV = { 18, 52, 86, 120, 144, 171, 205, 239 };

        static private string PKey()
        {

            string result = String.Empty;

            int[] arrPsw = { 72, 100, 94, 109, 106, 100, 105, 113, 96, 110, 111, 49, 46, 51, 43 };
            foreach (int i in arrPsw)
                result += (char)(i + 5);
            return result;

        }

        /// <summary>
        /// Шифрует полученную строку
        /// </summary>
        /// <param name="strText">Шифруемая строка</param>
        /// <returns>Зашифрованную строку или "Bad Data" если встретил проблемы</returns>
        static public string EncryptText(string strText)
        {
            return Encrypt(strText, PKey());
        }

        /// <summary>
        /// Расшифровывает полученную строку
        /// </summary>
        /// <param name="strText">Зашифрованная строка</param>
        /// <returns>Расшифрованная строка</returns>
        static public string DecryptText(string strText)
        {
            return Decrypt(strText, PKey());
        }

        static private string Encrypt(string strText, string strEncrKey)
        {
            byte[] byKey;
            byte[] IV = { 18, 52, 86, 120, 144, 171, 205, 239 };
            try
            {
                UTF8Encoding UTF8 = new UTF8Encoding();
                byKey = UTF8.GetBytes(strEncrKey.Substring(0, 8));

                DESCryptoServiceProvider des = new DESCryptoServiceProvider();
                byte[] inputbyteArray = UTF8.GetBytes(strText);
                MemoryStream ms = new MemoryStream();
                CryptoStream cs = new CryptoStream(ms, des.CreateEncryptor(byKey, IV), CryptoStreamMode.Write);
                cs.Write(inputbyteArray, 0, inputbyteArray.Length);
                cs.FlushFinalBlock();
                cs.Close();
                cs = null;
                des.Clear();
                des = null;
                byte[] arrX = ms.ToArray();
                ms.Close();
                ms = null;
                GC.Collect(0, GCCollectionMode.Forced);
                return Convert.ToBase64String(arrX);
            }
            catch
            {                
                return "Bad Data";
            }
        }

        static private string Decrypt(string strText, string sDecrKey)
        {
            byte[] byKey = {};
            byte[] IV = { 18, 52, 86, 120, 144, 171, 205, 239 };
            byte[] inputbyteArray = new byte[strText.Length];

            try
            {
                UTF8Encoding UTF8 = new UTF8Encoding();
                byKey = UTF8.GetBytes(sDecrKey.Substring(0, 8));
                DESCryptoServiceProvider des = new DESCryptoServiceProvider();
                inputbyteArray = Convert.FromBase64String(strText);
                MemoryStream ms = new MemoryStream();
                CryptoStream cs = new CryptoStream(ms, des.CreateDecryptor(byKey, IV), CryptoStreamMode.Write);
                

                cs.Write(inputbyteArray, 0, inputbyteArray.Length);
                cs.FlushFinalBlock();
                Encoding encoding = System.Text.Encoding.UTF8;
                cs.Close();
                cs = null;
                des.Clear();

                des = null;
                byte[] arrX = ms.ToArray();
                ms.Close();
                ms = null;
                GC.Collect(0, GCCollectionMode.Forced);
                return encoding.GetString(arrX);
            }
            catch (Exception ex)
            {                
                return "Bad Data";
            }
            
        }

        /// <summary>
        /// Шифрует массив чисел
        /// </summary>
        /// <param name="arrSource">Шифруемый массив</param>
        /// <returns>Зашифрованный массив</returns>
        static public byte[] EncryptArray(byte[] arrSource)
        {
            return Encrypt(arrSource, PKey());
        }

        /// <summary>
        /// Расшифровывает массив чисел
        /// </summary>
        /// <param name="arrSource">Зашифрованный массив</param>
        /// <returns>Расшифрованный массив</returns>
        static public byte[] DecryptArray(byte[] arrSource)
        {
            return Decrypt(arrSource, PKey());
        }

        static private byte[] Encrypt(byte[] arrX, string encrKey)
        {
            byte[] byKey;
            byte[] IV = { 18, 52, 86, 120, 144, 171, 205, 239 };
            try
            {
                UTF8Encoding UTF8 = new UTF8Encoding();
                byKey = UTF8.GetBytes(encrKey.Substring(0, 8));
                DESCryptoServiceProvider des = new DESCryptoServiceProvider();
                MemoryStream ms = new MemoryStream();
                CryptoStream cs = new CryptoStream(ms, des.CreateEncryptor(byKey, IV), CryptoStreamMode.Write);

                cs.Write(arrX, 0, arrX.Length);
                cs.Flush();
                cs.FlushFinalBlock();
                cs.Close();
                cs.Dispose();
                des.Clear();
                des = null;
                GC.Collect(0, GCCollectionMode.Forced);
                return ms.ToArray();
            }
            catch
            {                
                return null;
            }
            
        }

        static private byte[] Decrypt(byte[] arrX, string sDecrKey)
        {
            byte[] byKey;
            byte[] IV = { 18, 52, 86, 120, 144, 171, 205, 239 };

            try
            {
                UTF8Encoding UTF8 = new UTF8Encoding();
                byKey = UTF8.GetBytes(sDecrKey.Substring(0, 8));
                DESCryptoServiceProvider des = new DESCryptoServiceProvider();
                MemoryStream ms = new MemoryStream();
                CryptoStream cs = new CryptoStream(ms, des.CreateDecryptor(byKey, IV), CryptoStreamMode.Write);

                cs.Write(arrX, 0, arrX.Length);

                cs.Flush();
                cs.FlushFinalBlock();
                cs.Close();
                cs = null;
                des.Clear();
                des = null;
                GC.Collect(0, GCCollectionMode.Forced);
                return ms.ToArray();
            }
            catch
            {                
                return null;
            }
        }
    }
}
