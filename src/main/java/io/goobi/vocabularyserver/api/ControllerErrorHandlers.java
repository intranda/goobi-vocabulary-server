package io.goobi.vocabularyserver.api;

import io.goobi.vocabularyserver.exception.EntityNotFoundException;
import io.goobi.vocabularyserver.exception.MissingValuesException;
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
    @ExceptionHandler(NullPointerException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String nullPointerHandler(NullPointerException e) {
        return e.getMessage();
    }

    @ResponseBody
    @ExceptionHandler(DataIntegrityViolationException.class)
    @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
    String missingValuesHandler(DataIntegrityViolationException e) {
        // TODO: This is a bit too much magic
        int startIndex = e.getMessage().indexOf('[') + 1;
        int endIndex = e.getMessage().indexOf(']');
        return e.getMessage().substring(startIndex, endIndex);
    }
}