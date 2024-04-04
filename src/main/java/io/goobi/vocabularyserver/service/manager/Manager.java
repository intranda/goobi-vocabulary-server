package io.goobi.vocabularyserver.service.manager;

import io.goobi.vocabularyserver.exception.ValidationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface Manager<T> {
    default Page<T> listAll(Pageable pageable) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T get(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T create(T newT) throws ValidationException {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T replace(T newT, long id) throws ValidationException {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T delete(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
