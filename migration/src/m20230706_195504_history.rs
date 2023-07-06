use sea_orm_migration::prelude::*;

use crate::{m20230628_141206_printers::Printer, m20230628_165729_labels::Label};

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(History::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(History::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(History::PrinterId).uuid())
                    .foreign_key(
                        ForeignKey::create()
                            .from(History::Table, History::PrinterId)
                            .to(Printer::Table, Printer::Id)
                            .on_delete(ForeignKeyAction::SetNull),
                    )
                    .col(ColumnDef::new(History::LabelId).uuid())
                    .foreign_key(
                        ForeignKey::create()
                            .from(History::Table, History::LabelId)
                            .to(Label::Table, Label::Id)
                            .on_delete(ForeignKeyAction::SetNull),
                    )
                    .col(ColumnDef::new(History::Zpl).text().not_null())
                    .col(ColumnDef::new(History::Variables).json_binary().not_null())
                    .col(
                        ColumnDef::new(History::PrintedAt)
                            .timestamp_with_time_zone()
                            .not_null()
                            .default(SimpleExpr::Keyword(Keyword::CurrentTimestamp)),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(History::Table).to_owned())
            .await
    }
}

#[derive(Iden)]
enum History {
    Table,
    Id,
    PrinterId,
    LabelId,
    Zpl,
    Variables,
    PrintedAt,
}
