using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CtrEvents;
using ZGuard;
using ZPort;

namespace CtrEventsTest
{
    public class CvrtHandler
    {
        public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_COM;
        public const string CvtPortName = "COM9";

        private static CtrlListener _lister;
        private static CtrlListener _lister2;

        public CvrtHandler()
        {
            // Проверяем версию SDK
            UInt32 nVersion = ZGIntf.ZG_GetVersion();
            if ((((nVersion & 0xFF)) != ZGIntf.ZG_SDK_VER_MAJOR) || (((nVersion >> 8) & 0xFF) != ZGIntf.ZG_SDK_VER_MINOR))
            {
                Console.WriteLine("Неправильная версия SDK Guard.");
                Console.ReadLine();
                return;
            }

            IntPtr hCvt = new IntPtr(0);
            int hr;
            hr = ZGIntf.ZG_Initialize(ZPIntf.ZP_IF_NO_MSG_LOOP);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Initialize ({0}).", hr);
                Console.ReadLine();
                return;
            }
            try
            {
                ZG_CVT_INFO rInfo = new ZG_CVT_INFO();
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = CvtPortType;
                rOp.pszName = CvtPortName;
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;

                hr = ZGIntf.ZG_Cvt_Open(ref hCvt, ref rOp, rInfo);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Cvt_Open ({0}).", hr);
                    Console.ReadLine();
                    return;
                }



                _lister = new CtrlListener(hCvt, 2);
                _lister2 = new CtrlListener(hCvt, 3);

                Task.Factory.StartNew(() =>
                {
                    _lister.Start();
                    _lister2.Start();
                });

                Console.ReadLine();
            }
            finally
            {
                if (_lister != null)
                    _lister.Close();

                if (hCvt != IntPtr.Zero)
                    ZGIntf.ZG_CloseHandle(hCvt);
                ZGIntf.ZG_Finalyze();
            }
        }
    }
}
