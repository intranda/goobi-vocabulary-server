package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyValidationException;
import io.goobi.vocabulary.model.jpa.VocabularyEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class VocabularyValidatorImpl extends BaseValidator<VocabularyEntity> {
    public VocabularyValidatorImpl() {
        super("Vocabulary");
        setValidations(List.of(
                this::metadataSchemaAllowsOnlySingleRootElementAndNoHierarchy
        ));
    }

    private void metadataSchemaAllowsOnlySingleRootElementAndNoHierarchy(VocabularyEntity vocabulary) throws VocabularyValidationException {
        if (vocabulary.getMetadataSchema() != null) {
            List<String> errors = new ArrayList<>(2);
            if (!vocabulary.getMetadataSchema().isSingleRootElement()) {
                errors.add("Metadata schema has to be restricted to a single root element");
            }
            if (vocabulary.getMetadataSchema().isHierarchicalRecords()) {
                errors.add("Metadata schema is not allowed to be hierarchical");
            }
            if (!errors.isEmpty()) {
                throw new VocabularyValidationException("Unable to use schema [" + vocabulary.getMetadataSchema().getId() + "] as metadata schema:\n\t- " + String.join("\n\t- ", errors));
            }
        }
    }
}
