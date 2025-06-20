use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(History::Table)
                    .add_column_if_not_exists(
                        ColumnDef::new(History::Count)
                            .integer()
                            .not_null()
                            .default(1),
                    )
                    .add_column_if_not_exists(ColumnDef::new(History::Verification).json_binary())
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(History::Table)
                    .drop_column(History::Count)
                    .drop_column(History::Verification)
                    .to_owned(),
            )
            .await
    }
}

/// Learn more at https://docs.rs/sea-query#iden
#[derive(Iden)]
enum History {
    Table,
    Count,
    Verification,
}
