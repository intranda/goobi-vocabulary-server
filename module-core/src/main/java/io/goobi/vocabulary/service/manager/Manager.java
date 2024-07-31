package io.goobi.vocabulary.service.manager;

import io.goobi.vocabulary.exception.VocabularyException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface Manager<T> {
    default List<T> listAll() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default Page<T> list(Pageable pageable) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T get(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T create(T newT) throws VocabularyException {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T replace(T newT) throws VocabularyException {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T delete(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
