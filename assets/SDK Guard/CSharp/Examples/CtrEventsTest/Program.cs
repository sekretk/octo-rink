using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using CtrEventsTest;
using ZGuard;
using ZPort;

namespace CtrEvents
{
    public class CtrlListener
    {
        public IntPtr m_hCtr = new IntPtr(0);
        public int m_nCtrMaxEvents;
        public bool m_fProximity;
        public UInt32 m_nCtrFlags;
        public int m_nAppReadEventIdx;

        private readonly IntPtr _hCvt;
        private string _description;

        ManualResetEvent m_oEvent = null;
        bool m_fThreadActive;
        Thread m_oThread = null;

        int CheckNotifyMsgs(IntPtr ctrl)
        {
            int hr;
            UInt32 nMsg = 0;
            IntPtr nMsgParam = IntPtr.Zero;
            while ((hr = ZGIntf.ZG_Ctr_GetNextMessage(ctrl, ref nMsg, ref nMsgParam)) == ZGIntf.S_OK)
            {
                switch (nMsg)
                {
                    case ZGIntf.ZG_N_CTR_NEW_EVENT:
                        {
                            ZG_N_NEW_EVENT_INFO pInfo = (ZG_N_NEW_EVENT_INFO)Marshal.PtrToStructure(nMsgParam, typeof(ZG_N_NEW_EVENT_INFO));
                            Console.WriteLine("==> Новые события: {0}.", pInfo.nNewCount);
                            ShowEvents(pInfo.nReadIdx, pInfo.nNewCount);
                            m_nAppReadEventIdx = pInfo.nWriteIdx;
                        }
                        break;
                }
            }
            if (hr == ZPIntf.ZP_S_NOTFOUND)
                hr = ZGIntf.S_OK;
            return hr;
        }


        public CtrlListener(IntPtr hCvt, byte CtrAddr)
        {
            _hCvt = hCvt;
            
            ZG_CTR_INFO rCtrInfo = new ZG_CTR_INFO();

            int hr = ZGIntf.ZG_Ctr_Open(ref m_hCtr, hCvt, CtrAddr, 0, ref rCtrInfo);

            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_Open ({0}).", hr);
                Console.ReadLine();
                return;
            }
            m_nCtrMaxEvents = rCtrInfo.nMaxEvents;
            m_fProximity = ((rCtrInfo.nFlags & ZGIntf.ZG_CTR_F_PROXIMITY) != 0);
            m_nCtrFlags = rCtrInfo.nFlags;

            _description = String.Format("{0} адрес: {1}, с/н: {2}, v{3}.{4}, Количество событий: {5}, Тип ключей: {6}.", ZGuardDictionary.CtrTypeStrs[(int)rCtrInfo.nType],
                rCtrInfo.nAddr,
                rCtrInfo.nAddr,
                rCtrInfo.nSn,
                rCtrInfo.nVersion & 0xff, (rCtrInfo.nVersion >> 8) & 0xff,
                rCtrInfo.nMaxEvents);

            Console.WriteLine(_description);

            //m_fCtrNotifyEnabled = false;
            int nWrIdx = 0;
            int nRdIdx = 0;
            hr = ZGIntf.ZG_Ctr_ReadEventIdxs(m_hCtr, ref nWrIdx, ref nRdIdx);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_ReadEventIdxs ({0}).", hr);
                Console.ReadLine();
                return;
            }

            m_nAppReadEventIdx = nWrIdx;

            Console.WriteLine("-----");
        }

        public void Start()
        {
            int hr;

            m_oEvent = new ManualResetEvent(false);

            ZG_CTR_NOTIFY_SETTINGS rNS = new ZG_CTR_NOTIFY_SETTINGS(ZGIntf.ZG_NF_CTR_NEW_EVENT, m_oEvent.SafeWaitHandle, IntPtr.Zero, 0, m_nAppReadEventIdx, 300, 0);

            hr = ZGIntf.ZG_Ctr_SetNotification(m_hCtr, rNS);
            if (hr < 0)
            {
                Console.WriteLine("Ошибка ZG_Ctr_SetNotification ({0}).", hr);
                Console.ReadLine();
                return;
            }

            Console.WriteLine("Успешно подписан на ктр1");

            StartNotifyThread();
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

        void ShowEvents(int nStart, int nCount)
        {
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
                hr = ZGIntf.ZG_Ctr_ReadEvents(m_hCtr, nIdx, aEvents, nCnt, null, IntPtr.Zero);
                if (hr < 0)
                {
                    Console.WriteLine("Ошибка ZG_Ctr_ReadEvents ({0}).", hr);
                    Console.ReadLine();
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
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Power flags: {8:X2}h",
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
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Fire flags: {8:X2}h",
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
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Sub_event: {7} Security flags: {8:X2}h",
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
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Mode: {7} Sub_event: {8}",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.ModeStrs[(int)nMode], ZGuardDictionary.ModeSubEvStrs[(int)nSubEvent]);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_UNKNOWN_KEY:
                            {
                                Byte[] rKeyNum = new Byte[16];
                                ZGIntf.ZG_Ctr_DecodeUnkKeyEvent(m_hCtr, rEv.aData, rKeyNum);
                                Console.WriteLine("{0}.  Key \"{1}\"",
                                    nIdx + j,
                                    ZGIntf.CardNumToStr(rKeyNum, m_fProximity));
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
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} Mode: {7} Sub_event: {8} flags: {9:X2}h",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.EvTypeStrs[(int)rEv.nType], ZGuardDictionary.HModeStrs[(int)nMode], ZGuardDictionary.HotelSubEvStrs[(int)nSubEvent],
                                    nFlags);
                            }
                            break;
                        case ZG_CTR_EV_TYPE.ZG_EV_KEY_NOT_FOUND:
                            {
                                Byte[] rKeyNum = new Byte[16];
                                ZGIntf.ZG_Ctr_DecodeUnkKeyEvent(m_hCtr, rEv.aData, rKeyNum);

                                var m_rFindNum = new Byte[16];
                                ZGIntf.ZG_Ctr_ReadLastKeyNum(m_hCtr, m_rFindNum);

                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_DIRECT nDirect = new ZG_CTR_DIRECT();
                                int nKeyIdx = 0;
                                int nKeyBank = 0;
                                ZGIntf.ZG_Ctr_DecodePassEvent(m_hCtr, rEv.aData, ref rTime, ref nDirect, ref nKeyIdx, ref nKeyBank);

                                Console.WriteLine("{0}.  Key \"{1}\"",
                                    nIdx + j,
                                    ZGIntf.CardNumToStr(rKeyNum, m_fProximity));
                            }
                            break;
                        default:
                            {
                                ZG_EV_TIME rTime = new ZG_EV_TIME();
                                ZG_CTR_DIRECT nDirect = new ZG_CTR_DIRECT();
                                int nKeyIdx = 0;
                                int nKeyBank = 0;
                                ZGIntf.ZG_Ctr_DecodePassEvent(m_hCtr, rEv.aData, ref rTime, ref nDirect, ref nKeyIdx, ref nKeyBank);
                                Console.WriteLine("{0}. {1:D2}.{2:D2} {3:D2}:{4:D2}:{5:D2} {6} {7} (key_idx: {8}, bank#: {9})",
                                    nIdx + j,
                                    rTime.nDay, rTime.nMonth,
                                    rTime.nHour, rTime.nMinute, rTime.nSecond, ZGuardDictionary.DirectStrs[(int)nDirect], ZGuardDictionary.EvTypeStrs[(int)rEv.nType],
                                    nKeyIdx, nKeyBank);
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

    class Program
    {
        static void Main(string[] args)
        {
            CvrtHandler cvt = new CvrtHandler();
                Console.ReadLine();
        }
    }
}
