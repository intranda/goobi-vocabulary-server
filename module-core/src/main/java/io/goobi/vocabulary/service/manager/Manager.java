package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.ValidationException;
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

    default T replace(T newT) throws ValidationException {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T delete(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
