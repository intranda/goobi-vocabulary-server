/*M!999999\- enable the sandbox mode */ 
-- MariaDB dump 10.19-11.4.5-MariaDB, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: vocabulary
-- ------------------------------------------------------
-- Server version	11.4.5-MariaDB-ubu2404

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*M!100616 SET @OLD_NOTE_VERBOSITY=@@NOTE_VERBOSITY, NOTE_VERBOSITY=0 */;

--
-- Table structure for table `field_definition`
--

DROP TABLE IF EXISTS `field_definition`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `field_definition` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `schema_id` bigint(20) NOT NULL,
  `label` varchar(255) NOT NULL,
  `type_id` bigint(20) DEFAULT NULL,
  `required` bit(1) NOT NULL,
  `distinctive` bit(1) NOT NULL,
  `main_entry` bit(1) DEFAULT NULL,
  `title_field` bit(1) NOT NULL,
  `multi_valued` bit(1) NOT NULL,
  `reference_vocabulary_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `uc_46e8e276223be04d41c2677f0` (`schema_id`,`label`),
  UNIQUE KEY `uc_5da3ceb45920df55de6c71310` (`schema_id`,`main_entry`),
  KEY `FK_FIELDDEFINITION_ON_TYPE` (`type_id`),
  KEY `FK_FIELD_DEFINITION_ON_REFERENCE_VOCABULARY` (`reference_vocabulary_id`),
  CONSTRAINT `FK_FIELDDEFINITION_ON_SCHEMA` FOREIGN KEY (`schema_id`) REFERENCES `vocabulary_schema` (`id`),
  CONSTRAINT `FK_FIELDDEFINITION_ON_TYPE` FOREIGN KEY (`type_id`) REFERENCES `field_type` (`id`),
  CONSTRAINT `FK_FIELD_DEFINITION_ON_REFERENCE_VOCABULARY` FOREIGN KEY (`reference_vocabulary_id`) REFERENCES `vocabulary` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `field_definition`
--

LOCK TABLES `field_definition` WRITE;
/*!40000 ALTER TABLE `field_definition` DISABLE KEYS */;
/*!40000 ALTER TABLE `field_definition` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `field_instance`
--

DROP TABLE IF EXISTS `field_instance`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `field_instance` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `field_definition_id` bigint(20) NOT NULL,
  `record_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_FIELDINSTANCE_ON_FIELD_DEFINITION` (`field_definition_id`),
  KEY `FK_FIELDINSTANCE_ON_RECORD` (`record_id`),
  CONSTRAINT `FK_FIELDINSTANCE_ON_FIELD_DEFINITION` FOREIGN KEY (`field_definition_id`) REFERENCES `field_definition` (`id`),
  CONSTRAINT `FK_FIELDINSTANCE_ON_RECORD` FOREIGN KEY (`record_id`) REFERENCES `vocabulary_record` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `field_instance`
--

LOCK TABLES `field_instance` WRITE;
/*!40000 ALTER TABLE `field_instance` DISABLE KEYS */;
/*!40000 ALTER TABLE `field_instance` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `field_translation`
--

DROP TABLE IF EXISTS `field_translation`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `field_translation` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `field_value_id` bigint(20) NOT NULL,
  `language_id` bigint(20) DEFAULT NULL,
  `content` longtext DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_FIELDTRANSLATION_ON_FIELD_VALUE` (`field_value_id`),
  KEY `FK_FIELDTRANSLATION_ON_LANGUAGE` (`language_id`),
  CONSTRAINT `FK_FIELDTRANSLATION_ON_FIELD_VALUE` FOREIGN KEY (`field_value_id`) REFERENCES `field_value` (`id`),
  CONSTRAINT `FK_FIELDTRANSLATION_ON_LANGUAGE` FOREIGN KEY (`language_id`) REFERENCES `language` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `field_translation`
--

LOCK TABLES `field_translation` WRITE;
/*!40000 ALTER TABLE `field_translation` DISABLE KEYS */;
/*!40000 ALTER TABLE `field_translation` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `field_type`
--

DROP TABLE IF EXISTS `field_type`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `field_type` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `type_name` varchar(255) NOT NULL,
  `validation` varchar(255) DEFAULT NULL,
  `large` bit(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `uc_fieldtype_type_name` (`type_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `field_type`
--

LOCK TABLES `field_type` WRITE;
/*!40000 ALTER TABLE `field_type` DISABLE KEYS */;
/*!40000 ALTER TABLE `field_type` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `field_value`
--

DROP TABLE IF EXISTS `field_value`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `field_value` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `field_instance_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_FIELDVALUE_ON_FIELD_INSTANCE` (`field_instance_id`),
  CONSTRAINT `FK_FIELDVALUE_ON_FIELD_INSTANCE` FOREIGN KEY (`field_instance_id`) REFERENCES `field_instance` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `field_value`
--

LOCK TABLES `field_value` WRITE;
/*!40000 ALTER TABLE `field_value` DISABLE KEYS */;
/*!40000 ALTER TABLE `field_value` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `flyway_schema_history`
--

DROP TABLE IF EXISTS `flyway_schema_history`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `flyway_schema_history` (
  `installed_rank` int(11) NOT NULL,
  `version` varchar(50) DEFAULT NULL,
  `description` varchar(200) NOT NULL,
  `type` varchar(20) NOT NULL,
  `script` varchar(1000) NOT NULL,
  `checksum` int(11) DEFAULT NULL,
  `installed_by` varchar(100) NOT NULL,
  `installed_on` timestamp NOT NULL DEFAULT current_timestamp(),
  `execution_time` int(11) NOT NULL,
  `success` tinyint(1) NOT NULL,
  PRIMARY KEY (`installed_rank`),
  KEY `flyway_schema_history_s_idx` (`success`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `flyway_schema_history`
--

LOCK TABLES `flyway_schema_history` WRITE;
/*!40000 ALTER TABLE `flyway_schema_history` DISABLE KEYS */;
INSERT INTO `flyway_schema_history` VALUES
(1,'1','empty-database','SQL','V1__empty-database.sql',0,'vocabulary','2025-03-11 12:13:02',1,1),
(2,'2','initial-database','SQL','V2__initial-database.sql',-159679804,'vocabulary','2025-03-11 12:13:03',148,1),
(3,'3','refactor-validation-into-type','SQL','V3__refactor-validation-into-type.sql',-679174315,'vocabulary','2025-03-11 12:13:03',19,1),
(4,'4','remove-language-from-field-definition','SQL','V4__remove-language-from-field-definition.sql',-1405727544,'vocabulary','2025-03-11 12:13:03',6,1),
(5,'5','make-field-content-text','SQL','V5__make-field-content-text.sql',-10650889,'vocabulary','2025-03-11 12:13:03',9,1),
(6,'6','introduce-language-translations-and-multi-values','SQL','V6__introduce-language-translations-and-multi-values.sql',-1853893854,'vocabulary','2025-03-11 12:13:03',53,1),
(7,'7','refactor-translations','SQL','V7__refactor-translations.sql',1616092781,'vocabulary','2025-03-11 12:13:03',33,1),
(8,'8','hierarchical-records','SQL','V8__hierarchical-records.sql',-1819714809,'vocabulary','2025-03-11 12:13:03',32,1),
(9,'9','multi-value-field-definition','SQL','V9__multi-value-field-definition.sql',-140907708,'vocabulary','2025-03-11 12:13:03',5,1),
(10,'10','refactor-type-selectable-values','SQL','V10__refactor-type-selectable-values.sql',915993470,'vocabulary','2025-03-11 12:13:03',59,1),
(11,'11','translation-definition','SQL','V11__translation-definition.sql',1679173710,'vocabulary','2025-03-11 12:13:03',38,1),
(12,'12','non-translatable-values-reference-null-language','SQL','V12__non-translatable-values-reference-null-language.sql',28340973,'vocabulary','2025-03-11 12:13:03',8,1),
(13,'13','large-types-and-vocabulary-references','SQL','V13__large-types-and-vocabulary-references.sql',1644593345,'vocabulary','2025-03-11 12:13:03',19,1),
(14,'14','make-type-optional','SQL','V14__make-type-optional.sql',191239510,'vocabulary','2025-03-11 12:13:03',11,1),
(15,'15','single-root-element-field','SQL','V15__single-root-element-field.sql',-844758885,'vocabulary','2025-03-11 12:13:03',4,1),
(16,'16','add-vocabulary-metadata-schema','SQL','V16__add-vocabulary-metadata-schema.sql',-1285586611,'vocabulary','2025-03-11 12:13:03',14,1),
(17,'17','make-records-metadata-aware','SQL','V17__make-records-metadata-aware.sql',108486097,'vocabulary','2025-03-11 12:13:03',5,1),
(18,'18','migrate-from-sequence-id-generation-to-auto-increment','SQL','V18__migrate-from-sequence-id-generation-to-auto-increment.sql',-857913058,'vocabulary','2025-03-11 12:13:03',116,1);
/*!40000 ALTER TABLE `flyway_schema_history` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `language`
--

DROP TABLE IF EXISTS `language`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `language` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `abbreviation` varchar(255) NOT NULL,
  `full_name` varchar(255) DEFAULT NULL,
  `is_default` bit(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `uc_language_abbreviation` (`abbreviation`),
  UNIQUE KEY `uc_language_is_default` (`is_default`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `language`
--

LOCK TABLES `language` WRITE;
/*!40000 ALTER TABLE `language` DISABLE KEYS */;
/*!40000 ALTER TABLE `language` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `selectable_value`
--

DROP TABLE IF EXISTS `selectable_value`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `selectable_value` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `selection_value` varchar(255) NOT NULL,
  `field_type_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_SELECTABLEVALUE_ON_FIELD_TYPE` (`field_type_id`),
  CONSTRAINT `FK_SELECTABLEVALUE_ON_FIELD_TYPE` FOREIGN KEY (`field_type_id`) REFERENCES `field_type` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `selectable_value`
--

LOCK TABLES `selectable_value` WRITE;
/*!40000 ALTER TABLE `selectable_value` DISABLE KEYS */;
/*!40000 ALTER TABLE `selectable_value` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `translation_definition`
--

DROP TABLE IF EXISTS `translation_definition`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `translation_definition` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `language_id` bigint(20) NOT NULL,
  `fallback` bit(1) DEFAULT NULL,
  `field_definition_id` bigint(20) DEFAULT NULL,
  `required` bit(1) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `uc_237dd75d39305be1cae4ab970` (`field_definition_id`,`fallback`),
  KEY `FK_TRANSLATION_DEFINITION_ON_LANGUAGE` (`language_id`),
  CONSTRAINT `FK_TRANSLATION_DEFINITION_ON_FIELD_DEFINITION` FOREIGN KEY (`field_definition_id`) REFERENCES `field_definition` (`id`),
  CONSTRAINT `FK_TRANSLATION_DEFINITION_ON_LANGUAGE` FOREIGN KEY (`language_id`) REFERENCES `language` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `translation_definition`
--

LOCK TABLES `translation_definition` WRITE;
/*!40000 ALTER TABLE `translation_definition` DISABLE KEYS */;
/*!40000 ALTER TABLE `translation_definition` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vocabulary`
--

DROP TABLE IF EXISTS `vocabulary`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `vocabulary` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `schema_id` bigint(20) NOT NULL,
  `title` varchar(255) NOT NULL,
  `description` varchar(4096) DEFAULT NULL,
  `metadata_schema_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `uc_vocabulary_title` (`title`),
  KEY `FK_VOCABULARY_ON_SCHEMA` (`schema_id`),
  KEY `FK_VOCABULARY_ON_METADATA_SCHEMA` (`metadata_schema_id`),
  CONSTRAINT `FK_VOCABULARY_ON_METADATA_SCHEMA` FOREIGN KEY (`metadata_schema_id`) REFERENCES `vocabulary_schema` (`id`),
  CONSTRAINT `FK_VOCABULARY_ON_SCHEMA` FOREIGN KEY (`schema_id`) REFERENCES `vocabulary_schema` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vocabulary`
--

LOCK TABLES `vocabulary` WRITE;
/*!40000 ALTER TABLE `vocabulary` DISABLE KEYS */;
/*!40000 ALTER TABLE `vocabulary` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vocabulary_record`
--

DROP TABLE IF EXISTS `vocabulary_record`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `vocabulary_record` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `vocabulary_id` bigint(20) NOT NULL,
  `parent_record_id` bigint(20) DEFAULT NULL,
  `is_metadata` bit(1) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_VOCABULARYRECORD_ON_VOCABULARY` (`vocabulary_id`),
  KEY `FK_VOCABULARYRECORD_ON_PARENT_RECORD` (`parent_record_id`),
  CONSTRAINT `FK_VOCABULARYRECORD_ON_PARENT_RECORD` FOREIGN KEY (`parent_record_id`) REFERENCES `vocabulary_record` (`id`),
  CONSTRAINT `FK_VOCABULARYRECORD_ON_VOCABULARY` FOREIGN KEY (`vocabulary_id`) REFERENCES `vocabulary` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vocabulary_record`
--

LOCK TABLES `vocabulary_record` WRITE;
/*!40000 ALTER TABLE `vocabulary_record` DISABLE KEYS */;
/*!40000 ALTER TABLE `vocabulary_record` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vocabulary_schema`
--

DROP TABLE IF EXISTS `vocabulary_schema`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `vocabulary_schema` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `hierarchical_records` bit(1) DEFAULT NULL,
  `single_root_element` bit(1) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vocabulary_schema`
--

LOCK TABLES `vocabulary_schema` WRITE;
/*!40000 ALTER TABLE `vocabulary_schema` DISABLE KEYS */;
/*!40000 ALTER TABLE `vocabulary_schema` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*M!100616 SET NOTE_VERBOSITY=@OLD_NOTE_VERBOSITY */;

-- Dump completed on 2025-03-11 12:13:53
