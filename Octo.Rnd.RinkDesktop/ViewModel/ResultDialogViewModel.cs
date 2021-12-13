using System;
using System.Windows;
using Framework.UI.Controls;
using Prism.Commands;
using Prism.Mvvm;

namespace Octo.Rnd.RinkDesktop.ViewModel
{
    public abstract class ResultDialogViewModel<T, TR> : DialogViewModel<T> where T : OverlayWindow, new()
    {
        protected Action<TR> OnResult;

        public virtual void Show(Action<TR> onResult)
        {
            OnResult = onResult;

            base.Show();
        }

        public virtual void OnEnd(TR result)
        {
            OnResult?.Invoke(result);
            Close();
        }
    }
}
