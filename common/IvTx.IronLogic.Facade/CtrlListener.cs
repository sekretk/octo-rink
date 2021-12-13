using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using IvTx.Core;
using IvTx.Core.Log;
using IvTx.IronLogic.Facade.Enums;
using ZGuard;
using ZPort;

namespace IvTx.IronLogic.Facade
{
    public class CtrlListener
    {
        public IntPtr m_hCtr = new IntPtr(0);
        public int m_nCtrMaxEvents;
        public bool m_fProximity;
        public UInt32 m_nCtrFlags;
        public int m_nAppReadEventIdx;

        private readonly IntPtr _hCvt;
        private readonly Controller _ctrlInfo;
        private string _description;

        ManualResetEvent m_oEvent = null;
        bool m_fThreadActive;
        Thread m_oThread = null;
        private Func<CtrlEvent, bool> gateAcceptor;
        private Action<CtrlEvent> _gateListener;
        private string _card;

        int CheckNotifyMsgs(IntPtr ctrl)
        {
            //Logger.Info($"Иницирована проверка событий. Поток {Thread.CurrentThread.ManagedThreadId}");
            int hr;
            UInt32 nMsg = 0;
            IntPtr nMsgParam = IntPtr.Zero;
            while ((hr = ZGIntf.ZG_Ctr_GetNextMessage(ctrl, ref nMsg, ref nMsgParam)) == ZGIntf.S_OK)
            {
                Logger.Info($"Чтение событий. Поток {Thread.CurrentThread.ManagedThreadId}");

                switch (nMsg)
                {
                    case ZGIntf.ZG_N_CTR_NEW_EVENT:
                        {
                            ZG_N_NEW_EVENT_INFO pInfo = (ZG_N_NEW_EVENT_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZG_N_NEW_EVENT_INFO));
                            Logger.Info($"==> Новые события: {pInfo.nNewCount}. Поток {Thread.CurrentThread.ManagedThreadId}");
                            HandleNewEvents(pInfo.nReadIdx, pInfo.nNewCount);
                            m_nAppReadEventIdx = pInfo.nWriteIdx;
                        }
                        break;
                }
            }
            if (hr == ZPIntf.ZP_S_NOTFOUND)
                hr = ZGIntf.S_OK;
            return hr;
        }

        private ZG_CTR_INFO _rCtrInfo;

        public CtrlListener(IntPtr hCvt, Controller ctrlInfo)
        {
            _hCvt = hCvt;

            _ctrlInfo = ctrlInfo;

            _rCtrInfo = new ZG_CTR_INFO();

            int hr = ZGIntf.ZG_Ctr_Open(ref m_hCtr, hCvt, ctrlInfo.Address, 0, ref _rCtrInfo);

            if (hr < 0)
            {
                Logger.Error("Ошибка ZG_Ctr_Open ({0}).", hr);
                return;
            }
            m_nCtrMaxEvents = _rCtrInfo.nMaxEvents;
            m_fProximity = ((_rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_PROXIMITY) != 0);
            m_nCtrFlags = _rCtrInfo.nFlags;

            _description =
                $"{ZGuardDictionary.CtrTypeStrs[(int)_rCtrInfo.nType]} адрес: {_rCtrInfo.nAddr}, с/н: {_rCtrInfo.nSn}, v{_rCtrInfo.nVersion & 0xff}.{(_rCtrInfo.nVersion >> 8) & 0xff}, Количество событий: {_rCtrInfo.nMaxEvents}, Тип ключей: {_rCtrInfo.nFlags}.";

            Logger.Info(_description);

            _ctrlInfo.Name = ZGuardDictionary.CtrTypeStrs[(int)_rCtrInfo.nType];
            _ctrlInfo.SerialNumber = _rCtrInfo.nSn;
            _ctrlInfo.FirmwareVersion = $"v.{_rCtrInfo.nVersion & 0xff}, {(_rCtrInfo.nVersion >> 8) & 0xff}";

            //m_fCtrNotifyEnabled = false;
            int nWrIdx = 0;
            int nRdIdx = 0;
            hr = ZGIntf.ZG_Ctr_ReadEventIdxs(m_hCtr, ref nWrIdx, ref nRdIdx);
            if (hr < 0)
            {
                Logger.Error("Ошибка ZG_Ctr_ReadEventIdxs ({0}).", hr);
                return;
            }

            m_nAppReadEventIdx = nWrIdx;
        }

        public CtrlListener(IntPtr hCvt, Controller ctrlInfo, Func<CtrlEvent, bool> gateAcceptor) : this(hCvt, ctrlInfo)
        {
            this.gateAcceptor = gateAcceptor;
        }

        public CtrlListener(IntPtr hCvt, Controller ctrlInfo, Func<CtrlEvent, bool> gateAcceptor, Action<CtrlEvent> gateListener) : this(hCvt, ctrlInfo, gateAcceptor)
        {
            this._gateListener = gateListener;
        }

        bool _listening = true;

        public void Start()
        {
            

            Thread thd = new Thread(() =>
            {
                int hr;

                m_oEvent = new ManualResetEvent(false);

                ZG_CTR_NOTIFY_SETTINGS rNS = new ZG_CTR_NOTIFY_SETTINGS(ZGIntf.ZG_NF_CTR_NEW_EVENT, m_oEvent.SafeWaitHandle, IntPtr.Zero, 0, m_nAppReadEventIdx, 300, 0);

                hr = ZGIntf.ZG_Ctr_SetNotification(m_hCtr, rNS);

                if (hr < 0)
                {
                    Logger.Error("Ошибка ZG_Ctr_SetNotification ({0}).", hr);
                    return;
                }

                Logger.Info($"Успешно подписан на события контроллера {_description}. Поток {Thread.CurrentThread.ManagedThreadId}");

                //StartNotifyThread();

                while (_listening && m_hCtr != IntPtr.Zero)
                {
                    Failover.Execute(() => CheckNotifyMsgs(m_hCtr), exception => Logger.Error(exception));
                    Thread.Sleep(100);
                }
            });

            thd.Start();

            //if (m_hCtr != IntPtr.Zero)
            //    CheckNotifyMsgs(m_hCtr);
        }

        void DoShowNewEvents()
        {
            int hr;
            int nWrIdx = 0;
            int nRdIdx = 0;
            hr = ZGIntf.ZG_Ctr_ReadEventIdxs(m_hCtr, ref nWrIdx, ref nRdIdx);
            if (hr < 0)
            {
                Logger.Error("Ошибка ZG_Ctr_ReadEventIdxs ({0}).", hr);
                return;
            }
            int nNewCount;
            if (nWrIdx >= m_nAppReadEventIdx)
                nNewCount = (nWrIdx - m_nAppReadEventIdx);
            else
                nNewCount = (m_nCtrMaxEvents - m_nAppReadEventIdx + nWrIdx);
            
            //if (nNewCount != 0)
            //    Logger.Info($"Контроллер {_rCtrInfo.nAddr}, тип прохода {_ctrlInfo.EntranceWay}. Доступно {nNewCount} новых событий ({nRdIdx}-{nWrIdx}).");

            int nShowCount;

            while (nNewCount > 0)
            {
                nShowCount = 25;
                if (nShowCount > nNewCount)
                    nShowCount = nNewCount;
                HandleNewEvents(m_nAppReadEventIdx, nShowCount);
                nNewCount -= nShowCount;
                m_nAppReadEventIdx = (m_nAppReadEventIdx + nShowCount) % m_nCtrMaxEvents;
            }
        }

        void StopNotifyThread()
        {
            if (m_oThread == null)
                return;
            m_fThreadActive = false;
            m_oEvent.Set();
            // Wait until oThread finishes. Join also has overloads
            // that take a millisecond interval or a TimeSpan object.
            m_oThread.Join();
            m_oThread = null;
        }

        public void Stop()
        {
            StopNotifyThread();
        }

        void DoNotifyWork()
        {
            while (m_fThreadActive)
            {
                if (m_oEvent.WaitOne())
                {
                    m_oEvent.Reset();
                    if (m_hCtr != IntPtr.Zero)
                        CheckNotifyMsgs(m_hCtr);
                }
            }
        }

        void StartNotifyThread()
        {
            if (m_oThread != null)
                return;
            m_fThreadActive = true;
            m_oThread = new Thread(DoNotifyWork);

            m_oThread.Start();
        }

        void HandleNewEvents(int nStart, int nCount)
        {
            //Logger.Info($"Контроллер {_rCtrInfo.nAddr}, тип прохода {_ctrlInfo.EntranceWay}. IronLogic считал новые события: {nStart}, {nCount}. Поток {Thread.CurrentThread.ManagedThreadId}");

            ZG_CTR_EVENT[] aEvents = new ZG_CTR_EVENT[6];
            ZG_CTR_EVENT rEv;
            int i = 0;
            int nIdx, nCnt;
            int hr;

            while (i < nCount)
            {
                nIdx = (nStart + i) % m_nCtrMaxEvents;
                nCnt = (nCount - i);
                if (nCnt > aEvents.Length)
                    nCnt = aEvents.Length;
                if ((nIdx + nCnt) > m_nCtrMaxEvents)
                    nCnt = (m_nCtrMaxEvents - nIdx);

                //Logger.Info($"Контроллер {_rCtrInfo.nAddr}, тип прохода {_ctrlInfo.EntranceWay}. IronLogic индексы: {nIdx}, {nCnt} при максимуме инвентов {m_nCtrMaxEvents}. Поток {Thread.CurrentThread.ManagedThreadId}");

                hr = ZGIntf.ZG_Ctr_ReadEvents(m_hCtr, nIdx, aEvents, nCnt, null, IntPtr.Zero);
                if (hr < 0)
                {
                    Logger.Error("Ошибка ZG_Ctr_ReadEvents ({0}).", hr);
                    return;
                }

                for (int j = 0; j < nCnt; j++)
                {
                    rEv = aEvents[j];
                    switch (rEv.nType)
                    {
                        case ZG_CTR_EV_TYPE.ZG_EV_ELECTRO_ON:
                        case ZG_CTR_EV_TYPE.ZG_EV_ELECTRO_OFF:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_EC_SUB_EV nSubEvent = new ZG_EC_SUB_EV();
                                UInt32 nPowerFlags = 0;
                                ZGIntf.ZG_Ctr_DecodeEcEvent(m_hCtr, rEv.aData, ref rTime, ref nSubEvent, ref nPowerFlags);
                                Logger.Info("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Power flags: {8:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.EcSubEvStrs[(int)nSubEvent], nPowerFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_FIRE_STATE:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_FIRE_SUB_EV nSubEvent = new ZG_FIRE_SUB_EV();
                                UInt32 nFireFlags = 0;
                                ZGIntf.ZG_Ctr_DecodeFireEvent(m_hCtr, rEv.aData, ref rTime, ref nSubEvent, ref nFireFlags);
                                Logger.Info("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Fire flags: {8:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.FireSubEvStrs[(int)nSubEvent], nFireFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_SECUR_STATE:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_SECUR_SUB_EV nSubEvent = new ZG_SECUR_SUB_EV();
                                UInt32 nSecurFlags = 0;
                                ZGIntf.ZG_Ctr_DecodeSecurEvent(m_hCtr, rEv.aData, ref rTime, ref nSubEvent, ref nSecurFlags);
                                Logger.Info("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Security flags: {8:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.SecurSubEvStrs[(int)nSubEvent], nSecurFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_MODE_STATE:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_MODE nMode = new ZG_CTR_MODE();
                                ZG_MODE_SUB_EV nSubEvent = new ZG_MODE_SUB_EV();
                                ZGIntf.ZG_Ctr_DecodeModeEvent(m_hCtr, rEv.aData, ref rTime, ref nMode, ref nSubEvent);
                                Logger.Info("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Mode: {7} Sub_event: {8}",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.ModeStrs[(int)nMode], ZGuardDictionary.ModeSubEvStrs[(int)nSubEvent]);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_UNKNOWN_KEY:
                            {
                                Byte[] rKeyNum = new Byte[16];
                                ZGIntf.ZG_Ctr_DecodeUnkKeyEvent(m_hCtr, rEv.aData, rKeyNum);
                                _card = ZGIntf.EmMarineToStr(rKeyNum);

                                Logger.Info($"IronLogic получено событие считывания карты: {_card}. Поток {Thread.CurrentThread.ManagedThreadId}");

                                //Logger.Info("Контроллер: {0}. Событие #{1}, Считана не найденная в базе карта: {2}", _description, nIdx + j, card);

                                //ZG_EV_TIME rTime = new ZG_EV_TIME();
                                //ZG_CTR_DIRECT nDirect = new ZG_CTR_DIRECT();
                                //int nKeyIdx = 0;
                                //int nKeyBank = 0;
                                //ZGIntf.ZG_Ctr_DecodePassEvent(m_hCtr, rEv.aData, ref rTime, ref nDirect, ref nKeyIdx, ref nKeyBank);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_HOTEL40:
                        case ZG_CTR_EV_TYPE.ZG_EV_HOTEL41:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_HOTEL_MODE nMode = new ZG_HOTEL_MODE();
                                ZG_HOTEL_SUB_EV nSubEvent = new ZG_HOTEL_SUB_EV();
                                UInt32 nFlags = new UInt32();
                                ZGIntf.ZG_DecodeHotelEvent(rEv.aData, ref rTime, ref nMode, ref nSubEvent, ref nFlags);
                                Logger.Info("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Mode: {7} Sub_event: {8} flags: {9:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.HModeStrs[(int)nMode], ZGuardDictionary.HotelSubEvStrs[(int)nSubEvent],
                                    nFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_KEY_NOT_FOUND:
                            {
                                //Byte[] rKeyNum = new Byte[16];
                                //ZGIntf.ZG_Ctr_DecodeUnkKeyEvent(m_hCtr, rEv.aData, rKeyNum);
                                //_card = ZGIntf.EmMarineToStr(rKeyNum);

                                var m_rFindNum = new Byte[16];
                                ZGIntf.ZG_Ctr_ReadLastKeyNum(m_hCtr, m_rFindNum);
                                _card = ZGIntf.EmMarineToStr(m_rFindNum);

                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_DIRECT nDirect = new ZG_CTR_DIRECT();
                                int nKeyIdx = 0;
                                int nKeyBank = 0;
                                ZGIntf.ZG_Ctr_DecodePassEvent(m_hCtr, rEv.aData, ref rTime, ref nDirect, ref nKeyIdx, ref nKeyBank);

                                var evnt = new CtrlEvent()
                                {
                                    Controller = _ctrlInfo,
                                    Key = _card,
                                    EventType = (EventType)rEv.nType,
                                    Direction = (Direction)nDirect,
                                };

                                Logger.Info($"Номер карты: {_card}. Событие {(EventType)rEv.nType}. Направление {(Direction)nDirect}. Адрес контроллера {_ctrlInfo.Address}. Поток {Thread.CurrentThread.ManagedThreadId}");

                                if (gateAcceptor != null && gateAcceptor(evnt))
                                {
                                    hr = ZGIntf.ZG_Ctr_OpenLock(m_hCtr, (Direction)nDirect == Direction.In ? 0 : 1);

                                    Logger.Error($"Контроллер {_rCtrInfo.nAddr}, тип прохода {(Direction)nDirect}. По карте {_card} открыт турникет {_ctrlInfo.Address}", hr);

                                    if (hr < 0)
                                        Logger.Error("Ошибка ZG_Ctr_OpenLock ({0}).", hr);

                                    evnt.AccessGranted = true;
                                }
                                else
                                    Logger.Error($"Контроллер {_rCtrInfo.nAddr}, тип прохода {(Direction)nDirect}. По карте {_card} запрещен проход на турникете {_ctrlInfo.Address}", hr);

                                //Logger.Info($"Контроллер {_rCtrInfo.nAddr}, тип прохода {_ctrlInfo.EntranceWay}. IronLogic принято решение о карте {_card}");

                                _gateListener?.Invoke(evnt);

                                //Logger.Info($"IronLogic события считывания карты {_card} обработано");
                            }
                            break;
                        default:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_DIRECT nDirect = new ZG_CTR_DIRECT();
                                int nKeyIdx = 0;
                                int nKeyBank = 0;
                                ZGIntf.ZG_Ctr_DecodePassEvent(m_hCtr, rEv.aData, ref rTime, ref nDirect, ref nKeyIdx, ref nKeyBank);
                                Logger.Info("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} {7} (key_idx: {8}, bank#: {9})",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.DirectStrs[(int)nDirect], ZGuardDictionary.EvTypeStrs[(int)rEv.nType],
                                    nKeyIdx, nKeyBank);

                                _gateListener?.Invoke(new CtrlEvent() { Controller = _ctrlInfo, Key = _card, EventType = (EventType) rEv.nType, Direction = (Direction)nDirect });
                            }
                            break;
                    }
                }
                i += nCnt;
            }
        }

        public void Close()
        {
            if (_hCvt != IntPtr.Zero)
                ZGIntf.ZG_CloseHandle(m_hCtr);
        }
    }
}
