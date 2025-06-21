use sea_orm_migration::{prelude::*, sea_orm::DatabaseBackend};

use crate::m20230628_042220_label_sizes::LabelSize;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .add_column_if_not_exists(ColumnDef::new(Printer::Connection).json_binary())
                    .to_owned(),
            )
            .await?;

        match manager.get_database_backend() {
            DatabaseBackend::Sqlite => {
                manager.get_connection().execute_unprepared(
            "UPDATE printer SET connection = json_object('connection_type', 'network', 'address', printer.address);"
        ).await?;
            }
            _ => {
                manager.get_connection().execute_unprepared(
            "UPDATE printer SET connection = json_build_object('connection_type', 'network', 'address', printer.address);"
        ).await?;
            }
        }

        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .drop_column(Printer::Address)
                    .to_owned(),
            )
            .await?;

        match manager.get_database_backend() {
            DatabaseBackend::Sqlite => {
                let conn = manager.get_connection();

                conn.execute_unprepared("ALTER TABLE printer RENAME TO printer_old")
                    .await?;

                manager
                    .create_table(
                        Table::create()
                            .table(Printer::Table)
                            .if_not_exists()
                            .col(ColumnDef::new(Printer::Id).uuid().not_null().primary_key())
                            .col(ColumnDef::new(Printer::Name).text().not_null())
                            .col(ColumnDef::new(Printer::Connection).json_binary().not_null())
                            .col(ColumnDef::new(Printer::Dpmm).tiny_unsigned().not_null())
                            .col(ColumnDef::new(Printer::LabelSizeId).uuid())
                            .foreign_key(
                                ForeignKey::create()
                                    .from(Printer::Table, Printer::LabelSizeId)
                                    .to(LabelSize::Table, LabelSize::Id),
                            )
                            .to_owned(),
                    )
                    .await?;

                conn.execute_unprepared("INSERT INTO printer SELECT * FROM printer_old")
                    .await?;

                conn.execute_unprepared("DROP TABLE printer_old").await?;
            }
            _ => {
                manager
                    .alter_table(
                        Table::alter()
                            .table(Printer::Table)
                            .modify_column(ColumnDef::new(Printer::Connection).not_null())
                            .to_owned(),
                    )
                    .await?;
            }
        }

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .drop_column(Printer::Connection)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .add_column_if_not_exists(ColumnDef::new(Printer::Address).text().not_null())
                    .to_owned(),
            )
            .await?;

        Ok(())
    }
}

/// Learn more at https://docs.rs/sea-query#iden
#[derive(Iden)]
enum Printer {
    Table,
    Id,
    Name,
    Address,
    Dpmm,
    LabelSizeId,
    Connection,
}
