using System.Windows;

namespace Framework.UI.Controls
{
    using System.Windows.Controls;

    public sealed class MessageDialogButton : Button
    {
        public MessageBoxResult Result { get; set; } = MessageBoxResult.None;
    }
}
