using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using Framework;
using IvTx.Core;
using IvTx.Core.Log;
using IvTx.IronLogic.Facade.Enums;
using ZGuard;
using ZPort;
using Timer = System.Timers.Timer;

namespace IvTx.IronLogic.Facade
{
    public class DummyCvrtHandler : ICvrtHandler
    {
        private readonly Func<CtrlEvent, bool> _gateAcceptor;
        private readonly Action<CtrlEvent> _gateListener;
        private readonly IEnumerable<string> _cards;


        public void Dispose()
        {

        }

        public void StartMonitoringEvents()
        {
            Timer tmr = new Timer(2000);

            tmr.Elapsed += (sender, args) =>
                           {
                               _cards.ForEach(x =>
                                              {
                                                  var evnt = new CtrlEvent() { Controller = new Controller(), Key = x };

                                                  var result = _gateAcceptor(evnt);

                                                  _gateListener(evnt);
                                              });
                           };

            tmr.Start();
        }

        public void TurnFireMode(bool b)
        {
            throw new NotImplementedException();
        }

        public void OpenGates()
        {
            throw new NotImplementedException();
        }

        public DummyCvrtHandler(Func<CtrlEvent, bool> gateAcceptor, Action<CtrlEvent> gateListener, IEnumerable<string> cards)
        {
            _gateAcceptor = gateAcceptor;
            _gateListener = gateListener;
            _cards = cards;
        }
    }

    public interface ICvrtHandler : IDisposable
    {
        void StartMonitoringEvents();

        void TurnFireMode(bool b);

        void OpenGates();
    }

    public class CvrtHandler : ICvrtHandler
    {
        IntPtr hCvt = new IntPtr(0);

        public bool InFire { get; private set; }

        public void OpenGates()
        {
            throw new NotImplementedException();
        }

        public void TurnFireMode(bool on)
        {
            InFire = on;
        }

        //public const ZP_PORT_TYPE CvtPortType = ZP_PORT_TYPE.ZP_PORT_IP;
        public string CvtPortName;

        private ISet<CtrlListener> _ctrlListeners = new HashSet<CtrlListener>();

        private bool _listening;
        public bool Listening
        {
            get { return _listening; }
            private set { _listening = value; }
        }

        public CvrtHandler(string port, ConnectionType type, IEnumerable<Controller> controllers, Func<CtrlEvent, bool> gateAcceptor, Action<CtrlEvent> gateListener)
        {
            CvtPortName = port;

            // Проверяем версию SDK
            UInt32 nVersion = ZGIntf.ZG_GetVersion();
            if ((((nVersion & 0xFF)) != ZGIntf.ZG_SDK_VER_MAJOR) ||
                (((nVersion >> 8) & 0xFF) != ZGIntf.ZG_SDK_VER_MINOR))
            {
                Logger.Error("Неправильная версия SDK Guard.");
                return;
            }

            int hr;
            hr = ZGIntf.ZG_Initialize(ZPIntf.ZP_IF_NO_MSG_LOOP);
            if (hr < 0)
            {
                Logger.Error("Ошибка ZG_Initialize ({0}).", hr);
                return;
            }

            Failover.Execute(() =>
            {
                ZG_CVT_INFO rInfo = new ZG_CVT_INFO();
                ZG_CVT_OPEN_PARAMS rOp = new ZG_CVT_OPEN_PARAMS();
                rOp.nPortType = type == ConnectionType.COM ? ZP_PORT_TYPE.ZP_PORT_COM : ZP_PORT_TYPE.ZP_PORT_IP; //todo: add other ports
                rOp.pszName = CvtPortName;
                rOp.nSpeed = ZG_CVT_SPEED.ZG_SPEED_57600;

                hr = ZGIntf.ZG_Cvt_Open(ref hCvt, ref rOp, rInfo);
                if (hr < 0)
                {
                    Logger.Error("Ошибка ZG_Cvt_Open ({0}).", hr);
                    return;
                }

                controllers.ForEach(c => _ctrlListeners.Add(new CtrlListener(hCvt, c, gateAcceptor, gateListener)));

            }, Logger.Error);

        }

        public void StartMonitoringEvents()
        {
            _listening = false;

            _ctrlListeners.ForEach(l => l.Start());
            //while (_listening)
            //{

            //}
        }

        public void StopMonitoringEvents()
        {
            _ctrlListeners.ForEach(l => l.Stop());
            _listening = false;
        }

        public void Dispose()
        {
            StopMonitoringEvents();

            _ctrlListeners.ForEach(l => l.Close());

            if (hCvt != IntPtr.Zero)
                ZGIntf.ZG_CloseHandle(hCvt);

            ZGIntf.ZG_Finalyze();
        }
    }
}
