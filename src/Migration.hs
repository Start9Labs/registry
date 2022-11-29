module Migration where

import           Database.Persist.Migration     ( Column(Column)
                                                , ColumnProp(NotNull)
                                                , MigrateSql(MigrateSql)
                                                , Migration
                                                , MigrationPath((:=))
                                                , Operation(AddColumn, DropColumn, RawOperation)
                                                , PersistValue(PersistText)
                                                , SqlType(SqlString)
                                                , rawSql
                                                )
import           Database.Persist.Sql           ( Single(..) )
import           Startlude                      ( ($)
                                                , (<<$>>)
                                                , Maybe(Just)
                                                )

manualMigration :: Migration
manualMigration = [(0, 1) := migration_0_2_0, (1, 2) := migration_0_2_1, (2, 3) := migration_0_2_2]

migration_0_2_2 :: [Operation]
migration_0_2_2 = [DropColumn ("version", "arch")]

migration_0_2_1 :: [Operation]
migration_0_2_1 = [DropColumn ("category", "parent")]

migration_0_2_0 :: [Operation]
migration_0_2_0 =
    [ AddColumn "version" (Column "title" SqlString [NotNull])      (Just $ PersistText "")
    , AddColumn "version" (Column "desc_short" SqlString [NotNull]) (Just $ PersistText "")
    , AddColumn "version" (Column "desc_long" SqlString [NotNull])  (Just $ PersistText "")
    , AddColumn "version" (Column "icon_type" SqlString [NotNull])  (Just $ PersistText "")
    , populateMetadata
    , DropColumn ("pkg_record", "title")
    , DropColumn ("pkg_record", "desc_short")
    , DropColumn ("pkg_record", "desc_long")
    , DropColumn ("pkg_record", "icon_type")
    ]

populateMetadata :: Operation
populateMetadata =
    RawOperation "Populating Metadata"
        $     migrateMetadata
        <<$>> rawSql "SELECT pkg_id, title, desc_short, desc_long, icon_type FROM pkg_record" []
    where
        migrateMetadata (Single id', Single title', Single descShort', Single descLong', Single iconType') = MigrateSql
            "UPDATE version SET title = ?, desc_short = ?, desc_long = ?, icon_type = ? where pkg_id = ?"
            [PersistText title', PersistText descShort', PersistText descLong', PersistText iconType', PersistText id']
