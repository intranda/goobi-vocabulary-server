package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.VocabularyException;
import io.goobi.vocabulary.model.jpa.Identifiable;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

import static io.goobi.vocabulary.exception.VocabularyException.ErrorCode.GenericValidation;

public class BaseValidator<T extends Identifiable> implements Validator<T> {
    private final String name;
    @Setter
    private List<ValidationMethod<T>> validations;

    public BaseValidator(String name) {
        this.name = name;
    }

    @Override
    public final void validate(T t) throws VocabularyException {
        List<VocabularyException> errors = new LinkedList<>();
        for (ValidationMethod<T> m : validations) {
            try {
                m.validate(t);
            } catch (VocabularyException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new VocabularyException(GenericValidation, errors, null, (params) -> "Validation error [" + t.getId() + "]");
        }
    }
}
