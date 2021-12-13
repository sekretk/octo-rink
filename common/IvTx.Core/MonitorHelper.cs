using System;
using System.Threading;

namespace IvTx.Core
{
    public static class MonitorHelper
    {
        public static void ExecuteInMonitor(this object syncRoot, Action a)
        {
            if (!Monitor.TryEnter(syncRoot)) return;
            try
            {
                a();
            }
            finally
            {
                Monitor.Exit(syncRoot);
            }
        }
    }
}