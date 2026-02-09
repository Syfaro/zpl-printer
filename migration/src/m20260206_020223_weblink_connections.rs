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
                    .modify_column(ColumnDef::new(Printer::Connection).null())
                    .take(),
            )
            .await?;

        manager
            .create_table(
                Table::create()
                    .table(WebLinkCertificate::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(WebLinkCertificate::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(WebLinkCertificate::CertificateType)
                            .string()
                            .not_null(),
                    )
                    .col(ColumnDef::new(WebLinkCertificate::IssuerId).uuid())
                    .col(
                        ColumnDef::new(WebLinkCertificate::SerialNumber)
                            .binary()
                            .not_null()
                            .unique_key(),
                    )
                    .col(
                        ColumnDef::new(WebLinkCertificate::PublicKey)
                            .text()
                            .not_null(),
                    )
                    .col(ColumnDef::new(WebLinkCertificate::PrivateKey).binary())
                    .col(ColumnDef::new(WebLinkCertificate::PrinterId).uuid())
                    .col(
                        ColumnDef::new(WebLinkCertificate::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null()
                            .default(Expr::current_timestamp()),
                    )
                    .col(
                        ColumnDef::new(WebLinkCertificate::ExpiresAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(ColumnDef::new(WebLinkCertificate::RevokedAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(WebLinkCertificate::LastPingAt).timestamp_with_time_zone())
                    .foreign_key(
                        ForeignKey::create()
                            .from(WebLinkCertificate::Table, WebLinkCertificate::IssuerId)
                            .to(WebLinkCertificate::Table, WebLinkCertificate::Id),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(WebLinkCertificate::Table, WebLinkCertificate::PrinterId)
                            .to(Printer::Table, Printer::Id),
                    )
                    .take(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(Printer::Table)
                    .modify_column(ColumnDef::new(Printer::Connection).not_null())
                    .take(),
            )
            .await?;

        manager
            .drop_table(Table::drop().table(WebLinkCertificate::Table).take())
            .await?;

        Ok(())
    }
}

#[derive(DeriveIden)]
enum Printer {
    Table,
    Id,
    Connection,
}

#[derive(DeriveIden)]
enum WebLinkCertificate {
    Table,
    Id,
    CertificateType,
    IssuerId,
    SerialNumber,
    PublicKey,
    PrivateKey,
    PrinterId,
    CreatedAt,
    ExpiresAt,
    RevokedAt,
    LastPingAt,
}
