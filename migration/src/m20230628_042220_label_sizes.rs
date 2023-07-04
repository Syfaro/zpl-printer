use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(LabelSize::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(LabelSize::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(LabelSize::Width).decimal().not_null())
                    .col(ColumnDef::new(LabelSize::Height).decimal().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .unique()
                    .table(LabelSize::Table)
                    .col(LabelSize::Width)
                    .col(LabelSize::Height)
                    .to_owned(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(LabelSize::Table).to_owned())
            .await
    }
}

#[derive(Iden)]
pub enum LabelSize {
    Table,
    Id,
    Width,
    Height,
}
