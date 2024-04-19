package io.goobi.vocabulary.api;

import io.goobi.vocabulary.exception.EntityNotFoundException;
import io.goobi.vocabulary.exception.IllegalAttributeProvidedException;
import io.goobi.vocabulary.exception.MissingAttributeException;
import io.goobi.vocabulary.exception.MissingValuesException;
import io.goobi.vocabulary.exception.UnsupportedEntityReplacementException;
import io.goobi.vocabulary.exception.ValidationException;
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

@ControllerAdvice
public class ControllerErrorHandlers {
    private Logger logger = LoggerFactory.getLogger(ControllerErrorHandlers.class);

    @ResponseBody
    @ExceptionHandler(EntityNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    String entityNotFoundHandler(EntityNotFoundException e) {
        return e.getMessage();
    }

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
    @ExceptionHandler(MissingValuesException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String missingValuesHandler(MissingValuesException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(MissingAttributeException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String missingAttributesHandler(MissingAttributeException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(IllegalAttributeProvidedException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String illegalAttributeHandler(IllegalAttributeProvidedException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(ValidationException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String validationFailsHandler(ValidationException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(UnsupportedEntityReplacementException.class)
    @ResponseStatus(HttpStatus.NOT_IMPLEMENTED)
    String unsupportedResourcesReplaceMethod(UnsupportedEntityReplacementException e) {
        return e.getMessage();
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
    String missingValuesHandler(DataIntegrityViolationException e) {
        // TODO: This is a bit too much magic
        int startIndex = e.getMessage().indexOf('[') + 1;
        int endIndex = e.getMessage().indexOf(']');
        if (startIndex < 0) {
            startIndex = 0;
        }
        if (endIndex <= 0) {
            endIndex = e.getMessage().length();
        }
        return "Data integrity violation:\n\t" + e.getMessage().substring(startIndex, endIndex);
    }
}
