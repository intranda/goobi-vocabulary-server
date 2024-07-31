package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.VocabularyValidationMetadataSchemaIsHierarchical;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.VocabularyValidationMetadataSchemaIsNotSetToSingleRootElement;
import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.VocabularyValidationMetadataSchemaIssues;

@Service
public class VocabularyValidatorImpl extends BaseValidator<VocabularyEntity> {
    public VocabularyValidatorImpl() {
        super("Vocabulary");
        setValidations(List.of(
                this::metadataSchemaAllowsOnlySingleRootElementAndNoHierarchy
        ));
    }

    private void metadataSchemaAllowsOnlySingleRootElementAndNoHierarchy(VocabularyEntity vocabulary) throws VocabularyException {
        if (vocabulary.getMetadataSchema() != null) {
            List<VocabularyException> errors = new ArrayList<>(2);
            if (!vocabulary.getMetadataSchema().isSingleRootElement()) {
                errors.add(new VocabularyException(VocabularyValidationMetadataSchemaIsNotSetToSingleRootElement, null, null, params -> "Metadata schema has to be restricted to a single root element"));
            }
            if (vocabulary.getMetadataSchema().isHierarchicalRecords()) {
                errors.add(new VocabularyException(VocabularyValidationMetadataSchemaIsHierarchical, null, null, params -> "Metadata schema is not allowed to be hierarchical"));
            }
            if (!errors.isEmpty()) {
                throw new VocabularyException(VocabularyValidationMetadataSchemaIssues, errors, Map.of(
                        "metadataSchemaId", String.valueOf(vocabulary.getMetadataSchema().getId())
                ),
                        params -> "Unable to use schema [" + params.get("metadataSchemaId") + "] as metadata schema");
            }
        }
    }
}
