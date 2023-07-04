use sea_orm_migration::prelude::*;

use super::m20230628_042220_label_sizes::LabelSize;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Printer::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Printer::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Printer::Name).text().not_null())
                    .col(ColumnDef::new(Printer::Address).text().not_null())
                    .col(ColumnDef::new(Printer::Dpmm).tiny_unsigned().not_null())
                    .col(ColumnDef::new(Printer::LabelSizeId).uuid())
                    .foreign_key(
                        ForeignKey::create()
                            .from(Printer::Table, Printer::LabelSizeId)
                            .to(LabelSize::Table, LabelSize::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Printer::Table).to_owned())
            .await
    }
}

#[derive(Iden)]
enum Printer {
    Table,
    Id,
    Name,
    Address,
    Dpmm,
    LabelSizeId,
}
