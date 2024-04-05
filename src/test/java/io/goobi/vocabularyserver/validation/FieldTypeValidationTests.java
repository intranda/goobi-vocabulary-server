package io.goobi.vocabularyserver.validation;

import io.goobi.vocabularyserver.exception.ValidationException;
import io.goobi.vocabularyserver.model.FieldType;
import io.goobi.vocabularyserver.model.SelectableValue;
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

    private FieldType fieldType;

    @BeforeEach
    public void setUp() {
        fieldType = new FieldType();
        fieldType.setId(1L);
        fieldType.setName("Type");
    }

    private void setUpSelectableValues(String... values) {
        fieldType.setSelectableValues(IntStream
                .range(0, values.length)
                .mapToObj(i -> {
                    SelectableValue sv = new SelectableValue();
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
