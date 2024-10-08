package io.goobi.vocabulary.api;

import io.goobi.vocabulary.exception.VocabularyException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.servlet.resource.NoResourceFoundException;

import java.util.Map;

@ControllerAdvice
public class ControllerErrorHandlers {
    private Logger logger = LoggerFactory.getLogger(ControllerErrorHandlers.class);

    @ResponseBody
    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    @ResponseStatus(HttpStatus.METHOD_NOT_ALLOWED)
    String methodNotSupportedHandler(HttpRequestMethodNotSupportedException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(NoResourceFoundException.class)
    @ResponseStatus(HttpStatus.METHOD_NOT_ALLOWED)
    String noResourceFoundHandler(NoResourceFoundException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(UnsupportedOperationException.class)
    @ResponseStatus(HttpStatus.NOT_IMPLEMENTED)
    String methodNotImplementedYetHandler(UnsupportedOperationException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(VocabularyException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    VocabularyException validationFailsHandler(VocabularyException e) {
        return e;
    }

    @ResponseBody
    @ExceptionHandler(NullPointerException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String nullPointerHandler(NullPointerException e) {
        logger.error("NullPointerException", e);
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(DataIntegrityViolationException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    VocabularyException missingValuesHandler(DataIntegrityViolationException e) {
        // TODO: This is a bit too much magic
        int startIndex = e.getMessage().indexOf('[') + 1;
        int endIndex = e.getMessage().indexOf(']');
        if (startIndex < 0) {
            startIndex = 0;
        }
        if (endIndex <= 0) {
            endIndex = e.getMessage().length();
        }
        return new VocabularyException(VocabularyException.ErrorCode.DataIntegrityViolation, null, Map.of(
                "reason", e.getMessage().substring(startIndex, endIndex)
        ),
                (params) -> "Data integrity violation: " + params.get("reason"));
    }
}
