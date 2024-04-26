package io.goobi.vocabulary.validation;

import io.goobi.vocabulary.exception.ValidationException;
import io.goobi.vocabulary.model.jpa.FieldTypeEntity;
import io.goobi.vocabulary.model.jpa.SelectableValueEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class FieldTypeValidationTests {
    @InjectMocks
    private FieldTypeValidatorImpl validator;

    private FieldTypeEntity fieldType;

    @BeforeEach
    public void setUp() {
        fieldType = new FieldTypeEntity();
        fieldType.setId(1L);
        fieldType.setName("Type");
    }

    private void setUpSelectableValues(String... values) {
        fieldType.setSelectableValues(IntStream
                .range(0, values.length)
                .mapToObj(i -> {
                    SelectableValueEntity sv = new SelectableValueEntity();
                    sv.setId((long) i);
                    sv.setFieldType(fieldType);
                    sv.setValue(values[i]);
                    return sv;
                })
                .collect(Collectors.toList())
        );
    }

    @Test
    void givenSelectableValuesViolatingValidation_whenValidatingFieldType_throwValidationException() {
        fieldType.setValidation("\\d+");
        setUpSelectableValues("1", "2", "3", "nan");

        assertThrows(ValidationException.class, () -> validator.validate(fieldType));
    }

    @Test
    void givenSelectableValuesMatchingValidation_whenValidatingFieldType_success() throws ValidationException {
        fieldType.setValidation("\\d+");
        setUpSelectableValues("1", "2", "3");

        validator.validate(fieldType);
    }
}
