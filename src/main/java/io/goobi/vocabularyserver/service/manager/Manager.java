package io.goobi.vocabularyserver.service.manager;

import java.util.List;

public interface Manager<T> {
    default List<T> listAll() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T get(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T create(T newT) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T replace(T newT, long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    default T delete(long id) {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
