using System.Data.Common;
using System.Data.Entity;
using dbfirstplay;
using IvTx.Mi.DataBase.DB;

namespace IvTx.Mi.DataBase
{

    public partial class Model : DbContext
    {
        public Model() : base("name=IceRinkModel") { }

        public Model(DbConnection con, bool ownContext) : base(con, ownContext){ }

        public virtual DbSet<ApplicationLog> ApplicationLog { get; set; }
        public virtual DbSet<CashBook> CashBook { get; set; }
        public virtual DbSet<Configuration> Configuration { get; set; }
        public virtual DbSet<Currencies> Currencies { get; set; }
        public virtual DbSet<CurrenciesHistory> CurrenciesHistory { get; set; }
        public virtual DbSet<Documents> Documents { get; set; }
        public virtual DbSet<ECRReceipts> ECRReceipts { get; set; }
        public virtual DbSet<Goods> Goods { get; set; }
        public virtual DbSet<GoodsGroups> GoodsGroups { get; set; }
        public virtual DbSet<InternalLog> InternalLog { get; set; }
        public virtual DbSet<Lots> Lots { get; set; }
        public virtual DbSet<Network> Network { get; set; }
        public virtual DbSet<NextAcct> NextAcct { get; set; }
        public virtual DbSet<Objects> Objects { get; set; }
        public virtual DbSet<ObjectsGroups> ObjectsGroups { get; set; }
        public virtual DbSet<Operations> Operations { get; set; }
        public virtual DbSet<OperationType> OperationType { get; set; }
        public virtual DbSet<Partners> Partners { get; set; }
        public virtual DbSet<PartnersGroups> PartnersGroups { get; set; }
        public virtual DbSet<Payments> Payments { get; set; }
        public virtual DbSet<PaymentTypes> PaymentTypes { get; set; }
        public virtual DbSet<PriceRules> PriceRules { get; set; }
        public virtual DbSet<Registration> Registration { get; set; }
        public virtual DbSet<Store> Store { get; set; }
        public virtual DbSet<dbfirstplay.System> System { get; set; }
        public virtual DbSet<Transformations> Transformations { get; set; }
        public virtual DbSet<Users> Users { get; set; }
        public virtual DbSet<UsersGroups> UsersGroups { get; set; }
        public virtual DbSet<UsersSecurity> UsersSecurity { get; set; }
        public virtual DbSet<VATGroups> VATGroups { get; set; }
        public virtual DbSet<SecurityEvents> SecurityEvents { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
        }
    }
}
