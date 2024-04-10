package io.goobi.vocabularyserver.exception;

public class MappingException extends RuntimeException {
    public MappingException(Class<?> from, Class<?> to, Throwable e) {
        super("Unable to map \"" + from.getSimpleName() + "\" to \"" + to.getSimpleName() + "\". Reason: " + e.getMessage());
    }
}
