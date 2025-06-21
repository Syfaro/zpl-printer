pub use sea_orm_migration::prelude::*;

mod m20230628_042220_label_sizes;
mod m20230628_141206_printers;
mod m20230628_165729_labels;
mod m20230706_195504_history;
mod m20250620_183213_host_verifications;
mod m20250621_002016_printer_connections;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20230628_042220_label_sizes::Migration),
            Box::new(m20230628_141206_printers::Migration),
            Box::new(m20230628_165729_labels::Migration),
            Box::new(m20230706_195504_history::Migration),
            Box::new(m20250620_183213_host_verifications::Migration),
            Box::new(m20250621_002016_printer_connections::Migration),
        ]
    }
}
