package io.goobi.vocabulary.exception;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

@Getter
@Setter
@NoArgsConstructor
public class VocabularyException extends RuntimeException {
    public enum ErrorCode {
        DataIntegrityViolation,
        Mapping,
        EntityNotFound,
        MissingRequiredAttribute,
        InsertingNonExistingItem,
        DeletionOfReferencedVocabulary,
        DeletionOfReferencedVocabularyRecord,
        IllegalAttributeProvided,
        GenericValidation,
        SchemaValidationNoDefinitions,
        SchemaValidationMissingMainField,
        SchemaValidationTooManyMainFields,
        SchemaValidationMainFieldIsNotRequired,
        SchemaValidationTitleFieldsAreNotRequired,
        SchemaValidationDefinitionIssues,
        VocabularyValidationMetadataSchemaIssues,
        VocabularyValidationMetadataSchemaIsNotSetToSingleRootElement,
        VocabularyValidationMetadataSchemaIsHierarchical,
        RecordValidation,
        RecordValidationMissingRequiredFields,
        RecordValidationUndefinedFieldsGiven,
        RecordValidationHierarchyIssues,
        RecordValidationParentReferenceNotAllowed,
        RecordValidationChildrenReferencesNotAllowed,
        RecordValidationSingleRootElementVocabularyAlreadyContainsRecord,
        RecordImport,
        RecordImportUnsupportedExcelCellType,
        FieldInstanceIssues,
        FieldInstanceIsEmpty,
        FieldInstanceBelongsToWrongSchema,
        FieldInstanceValueIssues,
        FieldInstanceMultipleValuesNotAllowed,
        FieldDefinitionNonUniqueFallbackLanguageSpecified,
        FieldDefinitionFallbackNotRequired,
        FieldDefinitionReferenceVocabularyAndTypeNotPossible,
        FieldDefinitionNeitherReferenceVocabularyNorTypeNotPossible,
        FieldTypeInvalidRegexSyntax,
        FieldTypeSelectableValuesDoNotMatchSpecifiedValidationRegex,
        FieldValuesDoNotMatchSpecifiedValidationRegex,
        FieldValuesAreNonSelectableValues,
        FieldValueIsBlank,
        FieldValueIsNotUnique,
        FieldValueHasNonAllowedTranslations,
        FieldValueContainsNonTranslatedValue,
        FieldValueUnspecifiedTranslations,
        FieldValueMissingRequiredTranslations,
        FieldValueReferencedRecordIssues,
        FieldValueReferencedRecordBelongsToWrongVocabulary
    }

    private String exceptionType;
    private ErrorCode errorType;
    private String message;
    private List<VocabularyException> causes;
    private Map<String, String> params;

    public VocabularyException(ErrorCode errorType, List<VocabularyException> causes, Map<String, String> params, Function<Map<String, String>, String> messageGenerator) {
        this.exceptionType = getClass().getSimpleName();
        this.errorType = errorType;
        this.causes = causes;
        this.params = params;
        this.message = Optional.ofNullable(messageGenerator).map(g -> g.apply(params)).orElseThrow(() -> new RuntimeException("Something very unexpected happened"));
    }
}
