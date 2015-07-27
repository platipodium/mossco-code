PRAGMA foreign_keys=ON;
BEGIN TRANSACTION;
  DROP TABLE IF EXISTS "tblRecipesSubstances";
  DROP TABLE IF EXISTS "tblnames";
  DROP TABLE IF EXISTS "tblrecipes";
  DROP TABLE IF EXISTS "tblSubstances";
  
CREATE TABLE IF NOT EXISTS "tblSubstances" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "SubstanceName" TEXT NOT NULL,
    "IsParameter" INTEGER DEFAULT (0),
    "ParameterValue" REAL
);
CREATE TABLE  IF NOT EXISTS "tblnames" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "Substance_ID" INTEGER NOT NULL,
    "Alias" TEXT NOT NULL,
    FOREIGN KEY ("Substance_ID") REFERENCES "tblSubstances" ("ID")
);
CREATE TABLE  IF NOT EXISTS "tblrecipes" (
    "ID" INTEGER NOT NULL,
    "Substance_ID_Base" INTEGER NOT NULL,
    "quality" INTEGER NOT NULL,
     FOREIGN KEY ("Substance_ID_Base") REFERENCES "tblSubstances" ("ID")
);
CREATE TABLE  IF NOT EXISTS "tblRecipesSubstances" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "Recipe_ID" INTEGER NOT NULL,
    "Substance_ID" INTEGER NOT NULL,
    "Relation" TEXT NOT NULL,
    "Group" INTEGER,
     FOREIGN KEY ("Recipe_ID") REFERENCES "tblRecipes" ("ID")
     FOREIGN KEY ("Substance_ID") REFERENCES "tblSubstances" ("ID")
);

ANALYZE sqlite_master;
COMMIT;

INSERT INTO "tblsubstances" (SubstanceName) VALUES ("N");
INSERT INTO "tblnames" (Substance_ID, alias) VALUES ( last_insert_rowid(), "TN");

SELECT SubstanceName FROM (tblSubstances JOIN tblNames on tblSubstances.ID=tblNames.Substance_ID) tb WHERE tblNames.Alias='TN';
