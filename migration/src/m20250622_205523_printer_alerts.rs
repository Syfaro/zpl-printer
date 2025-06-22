use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .add_column_if_not_exists(ColumnDef::new(Printer::UniqueId).text())
                    .to_owned(),
            )
            .await?;

        manager
            .create_table(
                Table::create()
                    .table(Alert::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Alert::Id).uuid().not_null().primary_key())
                    .col(
                        ColumnDef::new(Alert::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null()
                            .default(SimpleExpr::Keyword(Keyword::CurrentTimestamp)),
                    )
                    .col(ColumnDef::new(Alert::AlertType).text().not_null())
                    .col(ColumnDef::new(Alert::AlertMessage).text().not_null())
                    .col(
                        ColumnDef::new(Alert::PrinterTimestamp)
                            .timestamp()
                            .not_null(),
                    )
                    .col(ColumnDef::new(Alert::UniqueId).text().not_null())
                    .col(ColumnDef::new(Alert::PrinterId).uuid())
                    .foreign_key(
                        ForeignKey::create()
                            .from(Alert::Table, Alert::PrinterId)
                            .to(Printer::Table, Printer::Id)
                            .on_delete(ForeignKeyAction::SetNull),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("printer_unique_id_idx")
                    .unique()
                    .table(Printer::Table)
                    .col(Printer::UniqueId)
                    .to_owned(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Alert::Table).to_owned())
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .drop_column(Printer::UniqueId)
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
    UniqueId,
}

#[derive(Iden)]
enum Alert {
    Table,
    Id,
    CreatedAt,
    AlertType,
    AlertMessage,
    PrinterTimestamp,
    UniqueId,
    PrinterId,
}
