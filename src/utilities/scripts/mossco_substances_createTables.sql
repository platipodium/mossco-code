PRAGMA foreign_keys=ON;
BEGIN TRANSACTION;
 DROP TABLE IF EXISTS "tblSubstancesEquivalents";    
 DROP TABLE IF EXISTS "tblAppendix";
 DROP TABLE IF EXISTS "tblEquivalents";
 DROP TABLE IF EXISTS "tblRulesets";
 DROP TABLE IF EXISTS "tblSubstances";
   
CREATE TABLE IF NOT EXISTS "tblSubstances" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "SubstanceName" TEXT NOT NULL
);

CREATE TABLE  IF NOT EXISTS "tblAppendix" (
    "ID" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "Substance_ID" TEXT NOT NULL,
     "Unit" TEXT NOT NULL,
     "Condition" TEXT,
     "Location" TEXT,
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
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("nitrate");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("dissolved_reduced_substances");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("detritus");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("detritus-P");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("slow_detritus_C");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("fast_detritus_C");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("phosphate");
INSERT INTO "tblSubstances" (SubstanceName) VALUES ("nutrients");

INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="O_2"), 
	"_upward_flux", "_at_soil_surface","x");	

	INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nitrate"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nitrate"), 
	"_upward_flux", "_at_soil_surface","x");	
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="N"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="N"), 
	"_upward_flux", "_at_soil_surface","x");			
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"), 
	"_upward_flux", "_at_soil_surface","x");	
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="slow_detritus_C"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="slow_detritus_C"), 
	"_upward_flux", "_at_soil_surface","x");	
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="fast_detritus_C"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="fast_detritus_C"), 
	"_upward_flux", "_at_soil_surface","x");	

INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus-P"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus-P"), 
	"_upward_flux", "_at_soil_surface","x");		
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="phosphate"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="phosphate"), 
	"_upward_flux", "_at_soil_surface","x");			

INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="dissolved_reduced_substances"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="dissolved_reduced_substances"), 
	"_upward_flux", "_at_soil_surface","x");		
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus"), 
	"_upward_flux", "_at_soil_surface","x");		
	
INSERT INTO "tblAppendix" (Substance_ID,	Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nutrients"), 
	"_at_soil_surface","x");
INSERT INTO "tblAppendix" (Substance_ID,	Condition, Location, Unit) VALUES (
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nutrients"), 
	"_upward_flux", "_at_soil_surface","x");		



INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("oxygen");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_oxygen");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_oxygen_oxy");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("nitrate");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("mole_concentration_of_nitrate");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("nitrogen");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("ammonium");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("mole_concentration_of_ammonium");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_ammonium_nh3");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("phosphate");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("mole_concentration_of_phosphate");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("Dissolved_Inorganic_Phosphorus_DIP_nutP");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("Dissolved_Inorganic_Nitrogen_DIN_nutN");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("detritus");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("slow_detritus_C");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("fast_detritus_C");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("detritus-P");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("Detritus_Phosphorus_detP");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("nutrients");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_reduced_substances");
INSERT INTO "tblEquivalents" (EquivalentName) VALUES ("dissolved_reduced_substances_odu");



INSERT INTO "tblRulesets" (RulesetName) VALUES ("General");
INSERT INTO "tblRulesets" (RulesetName) VALUES ("HZG KW");
INSERT INTO "tblRulesets" (RulesetName) VALUES ("Fallback");



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
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nitrate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="nitrate"));	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nitrate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="mole_concentration_of_nitrate"));	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="Fallback"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nitrate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="Dissolved_Inorganic_Nitrogen_DIN_nutN"));

INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="Fallback"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nitrate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="nutrients"));	
	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="N"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="nitrogen"));	
	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="ammonium"));	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="dissolved_ammonium_nh3"));
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="NH_3"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="mole_concentration_of_ammonium"));	

	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="slow_detritus_C"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="slow_detritus_C"));	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="fast_detritus_C"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="fast_detritus_C"));	

	INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus-P"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="detritus-P"));	
	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="phosphate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="phosphate"));		
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="phosphate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="mole_concentration_of_phosphate"));		

INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="phosphate"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="Dissolved_Inorganic_Phosphorus_DIP_nutP"));	
	

INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="dissolved_reduced_substances"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="dissolved_reduced_substances"));
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="dissolved_reduced_substances"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="dissolved_reduced_substances_odu"));	
	
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="detritus"));		
	

INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus-P"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="detritus-P"));	
	
	INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="detritus-P"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="Detritus_Phosphorus_detP"));
	
INSERT INTO "tblSubstancesEquivalents" (Ruleset_ID, Substance_ID, Equivalent_ID) VALUES (
	(SELECT ID FROM tblRulesets WHERE RulesetName="General"),
	(SELECT ID FROM tblSubstances WHERE SubstanceName="nutrients"),
	(SELECT ID FROM tblEquivalents WHERE EquivalentName="nutrients"));		

COMMIT;

