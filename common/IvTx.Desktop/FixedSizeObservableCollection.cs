using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IvTx.Desktop
{
    [Serializable()]
    public class FixedSizeObservableCollection<T> : Collection<T>, INotifyCollectionChanged, INotifyPropertyChanged
    {
        private readonly int _size;
        //----------------------------------------------------- 
        //
        //  Constructors 
        //
        //-----------------------------------------------------

        #region Constructors 
        /// <summary>
        /// Initializes a new instance of ObservableCollection that is empty and has default initial capacity. 
        /// </summary> 
        public FixedSizeObservableCollection(int size) : base()
        {
            _size = size;
        }

        /// <summary>
        /// Initializes a new instance of the ObservableCollection class
        /// that contains elements copied from the specified list
        /// </summary> 
        /// <param name="list">The list whose elements are copied to the new list.
        /// <remarks> 
        /// The elements are copied onto the ObservableCollection in the 
        /// same order they are read by the enumerator of the list.
        /// </remarks> 
        /// <exception cref="ArgumentNullException"> list is a null reference </exception>
        public FixedSizeObservableCollection(List<T> list, int size)
            : base((list != null) ? new List<T>(list.Count) : list)
        {
            _size = size;
            // Workaround for VSWhidbey bug 562681 (tracked by Windows bug 1369339).
            // We should be able to simply call the base(list) ctor.  But Collection<t> 
            // doesn't copy the list (contrary to the documentation) - it uses the 
            // list directly as its storage.  So we do the copying here.
            // 
            IList<T> items = Items;
            if (list != null && items != null)
            {
                using (IEnumerator<T> enumerator = list.GetEnumerator())
                {
                    while (enumerator.MoveNext())
                    {
                        items.Add(enumerator.Current);
                    }
                }
            }
        }

        #endregion Constructors


        //------------------------------------------------------
        // 
        //  Public Methods
        //
        //-----------------------------------------------------

        #region Public Methods

        /// <summary> 
        /// Move item at oldIndex to newIndex.
        /// </summary> 
        public void Move(int oldIndex, int newIndex)
        {
            MoveItem(oldIndex, newIndex);
        }

        #endregion Public Methods 


        //------------------------------------------------------ 
        //
        //  Public Events
        //
        //------------------------------------------------------ 

        #region Public Events 

        //-----------------------------------------------------
        #region INotifyPropertyChanged implementation 

        /// <summary>
        /// PropertyChanged event (per <see cref="INotifyPropertyChanged">).
        /// </see></summary> 
        event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
        {
            add
            {
                PropertyChanged += value;
            }
            remove
            {
                PropertyChanged -= value;
            }
        }
        #endregion INotifyPropertyChanged implementation 


        //------------------------------------------------------
        /// <summary>
        /// Occurs when the collection changes, either by adding or removing an item.
        /// </summary> 
        /// <remarks>
        /// see <seealso cref="INotifyCollectionChanged"> 
        /// </seealso></remarks> 
        public virtual event NotifyCollectionChangedEventHandler CollectionChanged;

        #endregion Public Events


        //----------------------------------------------------- 
        //
        //  Protected Methods 
        // 
        //-----------------------------------------------------

        #region Protected Methods

        /// <summary>
        /// Called by base class Collection<T> when the list is being cleared; 
        /// raises a CollectionChanged event to any listeners.
        /// </summary> 
        protected override void ClearItems()
        {
            CheckReentrancy();
            base.ClearItems();
            OnPropertyChanged(CountString);
            OnPropertyChanged(IndexerName);
            OnCollectionReset();
        }

        /// <summary> 
        /// Called by base class Collection<T> when an item is removed from list;
        /// raises a CollectionChanged event to any listeners. 
        /// </summary>
        protected override void RemoveItem(int index)
        {
            CheckReentrancy();
            T removedItem = this[index];

            base.RemoveItem(index);

            OnPropertyChanged(CountString);
            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Remove, removedItem, index);
        }

        /// <summary>
        /// Called by base class Collection<T> when an item is added to list; 
        /// raises a CollectionChanged event to any listeners. 
        /// </summary>
        protected override void InsertItem(int index, T item)
        {
            CheckReentrancy();


            if (Count == _size)
            {
                base.RemoveItem(0);
                index--;
            }

            base.InsertItem(index, item);

            OnPropertyChanged(CountString);
            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Add, item, index);
        }

        /// <summary>
        /// Called by base class Collection<T> when an item is set in list;
        /// raises a CollectionChanged event to any listeners.
        /// </summary> 
        protected override void SetItem(int index, T item)
        {
            CheckReentrancy();
            T originalItem = this[index];
            base.SetItem(index, item);

            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Replace, originalItem, item, index);
        }

        /// <summary> 
        /// Called by base class ObservableCollection<T> when an item is to be moved within the list; 
        /// raises a CollectionChanged event to any listeners.
        /// </summary> 
        protected virtual void MoveItem(int oldIndex, int newIndex)
        {
            CheckReentrancy();

            T removedItem = this[oldIndex];

            base.RemoveItem(oldIndex);
            base.InsertItem(newIndex, removedItem);

            OnPropertyChanged(IndexerName);
            OnCollectionChanged(NotifyCollectionChangedAction.Move, removedItem, newIndex, oldIndex);
        }


        /// <summary> 
        /// Raises a PropertyChanged event (per <see cref="INotifyPropertyChanged">). 
        /// </see></summary>
        protected virtual void OnPropertyChanged(PropertyChangedEventArgs e)
        {
            if (PropertyChanged != null)
            {
                PropertyChanged(this, e);
            }
        }

        /// <summary>
        /// PropertyChanged event (per <see cref="INotifyPropertyChanged">). 
        /// </see></summary>
        protected virtual event PropertyChangedEventHandler PropertyChanged;

        /// <summary> 
        /// Raise CollectionChanged event to any listeners.
        /// Properties/methods modifying this ObservableCollection will raise 
        /// a collection changed event through this virtual method. 
        /// </summary>
        /// <remarks> 
        /// When overriding this method, either call its base implementation
        /// or call <see cref="BlockReentrancy"> to guard against reentrant collection changes.
        /// </see></remarks>
        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            if (CollectionChanged != null)
            {
                using (BlockReentrancy())
                {
                    CollectionChanged(this, e);
                }
            }
        }

        /// <summary> 
        /// Disallow reentrant attempts to change this collection. E.g. a event handler 
        /// of the CollectionChanged event is not allowed to make changes to this collection.
        /// </summary> 
        /// <remarks>
        /// typical usage is to wrap e.g. a OnCollectionChanged call with a using() scope:
        /// <code>
        ///         using (BlockReentrancy()) 
        ///         {
        ///             CollectionChanged(this, new NotifyCollectionChangedEventArgs(action, item, index)); 
        ///         } 
        /// </code>
        /// </remarks> 
        protected IDisposable BlockReentrancy()
        {
            _monitor.Enter();
            return _monitor;
        }

        /// <summary> Check and assert for reentrant attempts to change this collection. </summary> 
        /// <exception cref="InvalidOperationException"> raised when changing the collection
        /// while another collection change is still being notified to other listeners </exception> 
        protected void CheckReentrancy()
        {
            if (_monitor.Busy)
            {
                // we can allow changes if there's only one listener - the problem
                // only arises if reentrant changes make the original event args 
                // invalid for later listeners.  This keeps existing code working 
                // (e.g. Selector.SelectedItems).
                if ((CollectionChanged != null) && (CollectionChanged.GetInvocationList().Length > 1))
                    throw new InvalidOperationException("ObservableCollectionReentrancyNotAllowed");
            }
        }

        #endregion Protected Methods


        //-----------------------------------------------------
        // 
        //  Private Methods
        //
        //------------------------------------------------------

        #region Private Methods
        /// <summary> 
        /// Helper to raise a PropertyChanged event  />). 
        /// </summary>
        private void OnPropertyChanged(string propertyName)
        {
            OnPropertyChanged(new PropertyChangedEventArgs(propertyName));
        }

        /// <summary>
        /// Helper to raise CollectionChanged event to any listeners 
        /// </summary> 
        private void OnCollectionChanged(NotifyCollectionChangedAction action, object item, int index)
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(action, item, index));
        }

        /// <summary> 
        /// Helper to raise CollectionChanged event to any listeners
        /// </summary> 
        private void OnCollectionChanged(NotifyCollectionChangedAction action, object item, int index, int oldIndex)
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(action, item, index, oldIndex));
        }

        /// <summary>
        /// Helper to raise CollectionChanged event to any listeners 
        /// </summary>
        private void OnCollectionChanged(NotifyCollectionChangedAction action, object oldItem, object newItem, int index)
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(action, newItem, oldItem, index));
        }

        /// <summary>
        /// Helper to raise CollectionChanged event with action == Reset to any listeners
        /// </summary> 
        private void OnCollectionReset()
        {
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }
        #endregion Private Methods 

        //-----------------------------------------------------
        //
        //  Private Types 
        //
        //------------------------------------------------------ 

        #region Private Types

        // this class helps prevent reentrant calls
        [Serializable()]
        private class SimpleMonitor : IDisposable
        {
            public void Enter()
            {
                ++_busyCount;
            }

            public void Dispose()
            {
                --_busyCount;
            }

            public bool Busy { get { return _busyCount > 0; } }

            int _busyCount;
        }

        #endregion Private Types

        //------------------------------------------------------ 
        //
        //  Private Fields 
        // 
        //-----------------------------------------------------

        #region Private Fields

        private const string CountString = "Count";

        // This must agree with Binding.IndexerName.  It is declared separately
        // here so as to avoid a dependency on PresentationFramework.dll. 
        private const string IndexerName = "Item[]";

        private SimpleMonitor _monitor = new SimpleMonitor();

        #endregion Private Fields
    }
}
