PRAGMA foreign_keys=ON;
BEGIN TRANSACTION;

CREATE TABLE IF NOT EXISTS "tblSubstances" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "SubstanceName" TEXT NOT NULL,
    "IsParameter" INTEGER DEFAULT (0),
    "ParameterValue" REAL
);
CREATE TABLE  IF NOT EXISTS "tblnames" (
    "ID" INTEGER NOT NULL,
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