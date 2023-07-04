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
                    .table(Label::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Label::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Label::Name).text().not_null())
                    .col(ColumnDef::new(Label::LabelSizeId).uuid().not_null())
                    .col(ColumnDef::new(Label::Dpmm).tiny_unsigned().not_null())
                    .col(ColumnDef::new(Label::Zpl).text().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .from(Label::Table, Label::LabelSizeId)
                            .to(LabelSize::Table, LabelSize::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Label::Table).to_owned())
            .await
    }
}

#[derive(Iden)]
enum Label {
    Table,
    Id,
    Name,
    LabelSizeId,
    Dpmm,
    Zpl,
}
