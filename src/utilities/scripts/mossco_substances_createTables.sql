PRAGMA foreign_keys=ON;
BEGIN TRANSACTION;
 DROP TABLE IF EXISTS "tblSubstancesEquivalents";    
 DROP TABLE IF EXISTS "tblAppendix";
 DROP TABLE IF EXISTS "tblEquivalents";
 DROP TABLE IF EXISTS "tblRulesets";
 DROP TABLE IF EXISTS "tblSubstances";  
   
CREATE TABLE IF NOT EXISTS "tblSubstances" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "SubstanceName" TEXT NOT NULL,
    "DefaultUnit" TEXT
);

CREATE TABLE  IF NOT EXISTS "tblAppendix" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "Substance_ID" TEXT NOT NULL,
    "Condition" TEXT,
    "Location" TEXT,
    "Unit" TEXT,
    FOREIGN KEY ("Substance_ID") REFERENCES "tblSubstances" ("ID")
);

CREATE TABLE  IF NOT EXISTS "tblEquivalents" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "EquivalentName" TEXT NOT NULL UNIQUE
);

CREATE TABLE  IF NOT EXISTS "tblRulesets" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "RulesetName" TEXT NOT NULL UNIQUE
);

CREATE TABLE  IF NOT EXISTS "tblSubstancesEquivalents" (
     "Ruleset_ID" INTEGER NOT NULL,
    "Substance_ID" INTEGER NOT NULL,
    "Equivalent_ID" INTEGER NOT NULL,
    "Rule" TEXT,
     FOREIGN KEY ("Ruleset_ID") REFERENCES "tblRulesets" ("ID"),
     FOREIGN KEY ("Substance_ID") REFERENCES "tblSubstances" ("ID"),
     FOREIGN KEY ("Equivalent_ID") REFERENCES "tblEquivalents" ("ID")
);

ANALYZE sqlite_master;
COMMIT;

BEGIN TRANSACTION;

INSERT INTO "tblSubstances" (SubstanceName) VALUES ("O_2");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("NH_3");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("N");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("N_2");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("NO_2");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("NO_3");

INSERT INTO "tblAppendix" (Substance_ID,	Location) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"), 
	"_at_soil_surface");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"), 
	"_upward_flux", "_at_soil_surface");	
	
INSERT INTO "tblAppendix" (Substance_ID,	Location) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"), 
	"_at_soil_surface");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"), 
	"_upward_flux", "_at_soil_surface");	
	
INSERT INTO "tblAppendix" (Substance_ID,	Location) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="N"), 
	"_at_soil_surface");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="N"), 
	"_upward_flux", "_at_soil_surface");		
	

INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("oxygen");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_oxygen");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_oxygen_oxy");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("mole_concentration_of_nitrate");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("mole_concentration_of_ammonium");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("mole_concentration_of_phosphate");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_reduced_substances");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("slow_detritus_C");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("fast_detritus_C");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("detritus-P");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("nutrients");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("detritus");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_reduced_substances_odu");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("ammonium");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("nitrogen");

INSERT INTO "tblRulesets" (RulesetName) VALUES ("General");
INSERT INTO "tblRulesets" (RulesetName) VALUES ("HZG KW");
INSERT INTO "tblRulesets" (RulesetName) VALUES ("HZG RH");

INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="oxygen"));
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="dissolved_oxygen"));
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="HZG KW"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="dissolved_oxygen_oxy"));
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="ammonium"));	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="N"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="nitrogen"));	

INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="mole_concentration_of_ammonium"));		
	
COMMIT;

SELECT t.SubstanceName  FROM (tblEquivalents
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID) t
	WHERE tblRulesets.RulesetName="General" AND tblEquivalents.EquivalentName="oxygen";

SELECT t.SubstanceName || coalesce(t.Condition,"") || coalesce(t.Location,"") 
	FROM (tblAppendix 
	JOIN tblSubstances ON tblAppendix.Substance_ID=tblSubstances.ID) t;

SELECT t.EquivalentName || coalesce(t.Condition,"") || coalesce(t.Location,"") 
	FROM (tblAppendix
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t;

SELECT t.EquivalentName || coalesce(t.Condition,"") || coalesce(t.Location,"") 
	FROM (tblAppendix
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t
	WHERE tblRulesets.RulesetName IN("General", "HZG KW") AND tblSubstances.SubstanceName="O_2";

SELECT tblAppendix.ID
	FROM (tblAppendix
	JOIN tblSubstances ON tblSubstances.ID=tblAppendix.Substance_ID) t
	WHERE tblSubstances.SubstanceName="O_2";	
	
SELECT t.EquivalentName || coalesce(t.Condition,"") || coalesce(t.Location,"") 
	FROM (tblAppendix
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t
	WHERE tblRulesets.RulesetName IN("General", "HZG KW") 
	AND tblSubstances.SubstanceName="O_2"
	AND tblAppendix.ID=1;

SELECT SubstanceName FROM tblSubstances;


SELECT t.SubstanceName  FROM (tblEquivalents
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID) t
	WHERE tblRulesets.RulesetName="General" AND tblEquivalents.EquivalentName="oxygen";

SELECT t.SubstanceName || coalesce(t.Condition,"") || coalesce(t.Location,"")  
	FROM (tblAppendix 
	JOIN tblSubstances ON tblAppendix.Substance_ID=tblSubstances.ID
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblSubstances.ID
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t
	WHERE tblRulesets.RulesetName in ("General")
	AND t.EquivalentName || coalesce(t.Condition,"") || coalesce(t.Location,"") == "oxygen_at_soil_surface";
	
SELECT t.SubstanceName || coalesce(t.Condition,'') || coalesce(t.Location,''), t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,'') 
	FROM (tblAppendix JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID 
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID 
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID 
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t 
	WHERE tblRulesets.RulesetName IN('General', 'HZG KW') 
	AND tblSubstances.SubstanceName='NH_3';
	
SELECT t.SubstanceName || coalesce(t.Condition,'') || coalesce(t.Location,''), t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,'') 
	FROM (tblAppendix 
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID 
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID 
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID 
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t 
	WHERE tblRulesets.RulesetName IN('General', 'HZG KW') 
	AND tblSubstances.SubstanceName='O_2'; 
	
	
SELECT t.EquivalentName || coalesce(t.Condition,"") || coalesce(t.Location,"") 
	FROM (tblAppendix
	JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID
	JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID
	JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID
	JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t
	WHERE tblRulesets.RulesetName IN("General", "HZG KW") 
	AND tblSubstances.SubstanceName || coalesce(tblAppendix.Condition,"") || coalesce(tblAppendix.Location,"") == "O_2_at_soil_surface";