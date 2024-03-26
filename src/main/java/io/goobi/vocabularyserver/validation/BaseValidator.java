package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class BaseValidator<T> implements Validator<T> {
    private final String name;
    @Setter
    private List<ValidationMethod<T>> validations;

    public BaseValidator(String name) {
        this.name = name;
    }

    @Override
    public final void validate(T t) throws ValidationException {
        List<Throwable> errors = new LinkedList<>();
        for (ValidationMethod<T> m : validations) {
            try {
                m.validate(t);
            } catch (ValidationException e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty()) {
            throw new ValidationException("Error validating " + name + ":\n\t"
                    + errors.stream()
                    .map(Throwable::getMessage)
                    .collect(Collectors.joining("\n\t")));
        }
    }
}
